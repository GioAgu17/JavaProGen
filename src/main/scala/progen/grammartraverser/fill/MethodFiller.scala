package progen.grammartraverser.fill

import progen.grammarparser.Node
import progen.grammartraverser.AST
import progen.grammartraverser.utils.IDGenerator
import progen.peg.entities.{GlobalTable, MethodSignature}
import progen.symtab.SymTab

class MethodFiller(val symbolTable: SymTab, val globalTable: GlobalTable) {

  def fillClassWithMethods(cbdTrees:List[AST[Node]]): AST[Node] ={
    val methodSigns = symbolTable.symTabEntry.methods match{
      case Some(ms) => ms
      case None => sys.error("no methods found during fillage of method members in tree")
    }
    val cbdTreesAndMethods = cbdTrees zip methodSigns
    if(methodSigns.size != cbdTrees.size)
      sys.error("something went wrong: number of methods different than number of <classbodydeclaration> nodes available for the methods")
    val tree = cbdTreesAndMethods.map(tm => fillMethods(tm._1,tm._2))
    cbdTrees.head
  }
  def fillMethods(cbdTree: AST[Node], method:MethodSignature): AST[Node]={
    val classMemDeclnode = cbdTree.node.toNodes.head.toNodes.head
    val classMemDeclTree = cbdTree.add(classMemDeclnode,IDGenerator.nextID)
    val methodDeclNode =classMemDeclnode.toNodes(1).toNodes.head
    // adding method declaration tree with its id from signature
    val methodDeclTree = classMemDeclTree.add(methodDeclNode,method.id)
    val methodHeaderNode = methodDeclNode.toNodes.head
    val methodHeaderTree = methodDeclTree.add(methodHeaderNode,IDGenerator.nextID)
    val methodHeaderTreeFilled = FillerUtils.handleMethodHeader(methodHeaderTree,method,globalTable,false)
    val methodBodyNode = methodDeclNode.toNodes(1)
    val methodBodyTree = methodDeclTree.add(methodBodyNode,IDGenerator.nextID)
    methodBodyTree
  }


}


