package progen.grammartraverser.fill

import progen.grammarparser.Node
import progen.grammartraverser.AST
import progen.grammartraverser.utils.IDGenerator
import progen.peg.entities.{ConstructorSignature, GlobalTable}
import progen.symtab.SymTab

class ConstructorFiller(val symTab: SymTab, val globalTable: GlobalTable){

  def fillConstructor(classBody: AST[Node]): AST[Node]={
    val classBodyDecls = classBody.children(1)
    val constructors = symTab.symTabEntry.constructors match{
      case Some(c) => c
      case None => sys.error("no constructors found in class")
    }
    val constrFilled = fillWithConstructors(classBodyDecls,constructors)
    classBody
  }

  def fillWithConstructors(cbdsTree: AST[Node], signatures: List[ConstructorSignature]): AST[Node]={
    val classBodyDeclTrees = FillerUtils.findTreesWithNoChildren(cbdsTree,"<classbodydeclaration>")
    // adding constructor declaration trees with their ids from signatures
    val constrDeclTreesAndConstr = (classBodyDeclTrees.take(signatures.size) zip signatures).map(cs => (cs._1.add(cs._1.node.toNodes(1),cs._2.id),cs._2) )
    val c = constrDeclTreesAndConstr.map(c => handleConstructorDeclaration(c._1,c._2))
    cbdsTree
  }

  def handleConstructorDeclaration(constrDeclTree: AST[Node], signature: ConstructorSignature): AST[Node]={
    val constrDeclarationNode = constrDeclTree.node.toNodes.head

    val constrDeclaratorNode = constrDeclarationNode.toNodes.head

    val constrDeclaratorTree = constrDeclTree.add(constrDeclaratorNode,IDGenerator.nextID)
    val constrBodyNode = constrDeclarationNode.toNodes(1)
    val constrBodyTree = constrDeclTree.add(constrBodyNode,IDGenerator.nextID)
    val treeToReturn = handleConstrDeclarator(constrDeclaratorTree,signature)
    treeToReturn
  }
  def handleConstrDeclarator(value: AST[Node], signature: ConstructorSignature): AST[Node] ={
    // handling the first child of constructor declarator, i.e. <simple type name>
    val tree = FillerUtils.visitUntilDFS("<identifier>",value.node.toNodes.head,value).add(new Node(signature.className,false,List(),true),IDGenerator.nextID)
    val childrenRemained = value.node.toNodes.tail
    val paren1 = value.add(childrenRemained.head,IDGenerator.nextID)
    if(signature.formalParameters.nonEmpty) {
      val formalParamListNode = childrenRemained(1)

      FillerUtils.handleFormalParams(formalParamListNode,value, signature.formalParameters,globalTable)
    }
    val node = childrenRemained.last
    val paren2 = new Node(")",node.alternative,node.edges,node.terminal)
    val paren2Tree = value.add(paren2,IDGenerator.nextID)
    value
  }




}


