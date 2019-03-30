package progen.grammartraverser.fill

import progen.grammarparser.Node
import progen.grammartraverser.AST
import progen.peg.entities.GlobalTable
import progen.symtab.SymTab

class ClassMemberFiller(val symTab: SymTab, val globalTable: GlobalTable) {
  def fillMembers(tree: AST[Node]): AST[Node]={
    val classBodyDeclarations = FillerUtils.findTreesWithNoChildren(tree,"<classbodydeclaration>")
    if(classBodyDeclarations.nonEmpty) {
      val fields = symTab.symTabEntry.fields match{
        case Some(f) => f.filter(!_.inherited)
        case None => sys.error("no fields found during fillage of class members in ast")
      }

      val mapByType = fields.groupBy(_.stype)
      val noOfFields = mapByType.size
      val classBodyDeclsGrouped = classBodyDeclarations.splitAt(noOfFields)
      val fieldsTrees = classBodyDeclsGrouped._1

      if (fieldsTrees.nonEmpty) {
        val fieldFiller = new FieldFiller(symTab, globalTable)
        val fieldsFilled = fieldFiller.fillClass(fieldsTrees)
      }
      if(classBodyDeclsGrouped._2.nonEmpty) {
        val methodFiller = new MethodFiller(symTab, globalTable)
        val methodsFilledTree = methodFiller.fillClassWithMethods(classBodyDeclsGrouped._2)
      }
      classBodyDeclarations.head
    }
    else
      tree
  }
}

