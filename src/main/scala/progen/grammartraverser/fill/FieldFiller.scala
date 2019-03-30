package progen.grammartraverser.fill

import progen.grammarparser.Node
import progen.grammartraverser.AST
import progen.grammartraverser.utils.IDGenerator
import progen.peg.entities.{Field, GlobalTable}
import progen.symtab.SymTab

class FieldFiller(val symTab:SymTab, val globalTable: GlobalTable){

  def fillClass(classBodyDeclTrees: List[AST[Node]]): AST[Node] ={
    val fields = symTab.symTabEntry.fields match{
      case Some(f) => f.filter(!_.inherited)
      case None => sys.error("no fields found during fillage of class members in ast")
    }
    val fieldsWithSameType = fields.groupBy(_.stype)
    val fieldsWithSameTypeAndTrees = classBodyDeclTrees zip fieldsWithSameType
    val filledFields = fieldsWithSameTypeAndTrees.map(c => fillFields(c._1,c._2))
    classBodyDeclTrees.head
  }
  def fillFields(classBodyDeclTree: AST[Node], typeAndFields: (String,List[Field])): AST[Node]={
    val classMemberDeclNode = classBodyDeclTree.node.toNodes.head.toNodes.head
    val classMemberDeclTree = classBodyDeclTree.add(classMemberDeclNode,IDGenerator.nextID)
    val fieldDeclNode = classMemberDeclNode.toNodes.head.toNodes.head
    val fieldDeclTree = classMemberDeclTree.add(fieldDeclNode,IDGenerator.nextID)
    val filledTree = fieldDeclNode.toNodes.map(n => handleFieldDeclChildren(n,fieldDeclTree,typeAndFields))
    classBodyDeclTree
  }

  def handleFieldDeclChildren(node: Node, fieldDeclTree: AST[Node], tuple: (String, List[Field])): AST[Node]= node.description match{
    case "<type>" =>
      val nodeType = if(globalTable.primitiveTypes.contains(tuple._1)) node.toNodes(1) else node.toNodes.head
      val nodeTypeTree = fieldDeclTree.add(node,IDGenerator.nextID)
      FillerUtils.visitUntilDFS("<identifier>",nodeType,nodeTypeTree).add(new Node(tuple._1,false,List(),true),IDGenerator.nextID)
    case "<variabledeclarators>" => handleDeclarators(node,fieldDeclTree,tuple)
    case ";" =>
      fieldDeclTree.add(node,IDGenerator.nextID)
  }

  def handleDeclarators(node: Node, tree: AST[Node], tuple: (String, List[Field])): AST[Node]={

    def loop(n: Node, no: Int,fields: List[Field],tree: AST[Node]): AST[Node]= no match{
      case 1 =>
        val variableDeclaratorNode = n.toNodes.head.toNodes.head
        val variableDeclaratorTree = tree.add(variableDeclaratorNode,IDGenerator.nextID)
        FillerUtils.visitUntilDFS("<identifier>",variableDeclaratorNode.toNodes.head,variableDeclaratorTree).add(new Node(fields.head.name,false,List(),true),IDGenerator.nextID)
      case x =>
        val varDeclsVarDeclNode = n.toNodes(1)
        val varDeclsVarDeclTree = tree.add(varDeclsVarDeclNode,IDGenerator.nextID)
        val varDeclsNode = varDeclsVarDeclNode.toNodes.head
        loop(varDeclsNode,x-1,fields.tail,varDeclsVarDeclTree)
        val commaNode = varDeclsVarDeclNode.toNodes(1)
        val commaTree = varDeclsVarDeclTree.add(commaNode,IDGenerator.nextID)
        val varDeclNode = varDeclsVarDeclNode.toNodes(2)
        FillerUtils.visitUntilDFS("<identifier>",varDeclNode,varDeclsVarDeclTree).add(new Node(fields.head.name,false,List(),true),IDGenerator.nextID)
    }
    loop(node,tuple._2.size,tuple._2.reverse,tree)
  }


}

