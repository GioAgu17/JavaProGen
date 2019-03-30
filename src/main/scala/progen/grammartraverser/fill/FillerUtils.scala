package progen.grammartraverser.fill

import progen.grammarparser.Node
import progen.grammartraverser.AST
import progen.grammartraverser.utils.IDGenerator
import progen.peg.entities.{GlobalTable, MethodSignature}

object FillerUtils {
  // utils function to dive into the graph and add intermediate nodes in a Depth First Search fashion
  // node and ast are at the same level, i.e. ast.node = node
  def visitUntilDFS(nodeDescription: String, node: Node, ast: AST[Node]): AST[Node] = node.description match {
    case `nodeDescription` => ast.add(node,IDGenerator.nextID)
    case _ =>
      val newNode = node.toNodes.head
      val newAst = ast.add(node,IDGenerator.nextID)
      visitUntilDFS(nodeDescription, newNode, newAst)

  }
  // finds the node with name equal to "description" from the tree "tree"
  def findTreesWithNoChildren(tree: AST[Node], description: String): List[AST[Node]] = tree.node.description match {
    case `description` => if(tree.children.isEmpty) List(tree) else List()
    case _ => tree.node.terminal match {
      case true => List()
      case false => tree.children.toList.flatMap(c => findTreesWithNoChildren(c,description))
    }
  }

  def findTrees(tree: AST[Node], description: String): List[AST[Node]] = tree.node.description match{
    case `description` => List(tree)
    case _ => tree.node.terminal match{
      case true => List()
      case false => tree.children.toList.flatMap(c => findTrees(c,description))
    }
  }
  def handleFormalParams(node: Node, constrDeclaratorTree : AST[Node], formalParams: List[(String, String)], globalTable: GlobalTable): AST[Node]={
    def loop(node: Node,no: Int, formalParams:List[(String,String)], tree: AST[Node]): AST[Node] =no match{

      case 1 => {
        val formalParamNode = node.toNodes.head.toNodes.head
        val formListTree = tree.add(node,IDGenerator.nextID)
        handleSingleFormalParam(formListTree.add(formalParamNode,IDGenerator.nextID),formalParams.head,globalTable)
      }
      case x =>
        val formalParamListAndParamNode = node.toNodes(1)
        val formalParamsListAndParamTree = tree.add(node,IDGenerator.nextID).add(formalParamListAndParamNode,IDGenerator.nextID)
        loop(formalParamListAndParamNode.toNodes.head,x-1,formalParams.tail,formalParamsListAndParamTree)
        val commaNode = formalParamListAndParamNode.toNodes(1)
        val commaTree = formalParamsListAndParamTree.add(commaNode,IDGenerator.nextID)
        val formalParamNode = formalParamListAndParamNode.toNodes.last
        val formParamTree = formalParamsListAndParamTree.add(formalParamNode,IDGenerator.nextID)
        handleSingleFormalParam(formParamTree,formalParams.head,globalTable)


    }
    loop(node,formalParams.size,formalParams.reverse,constrDeclaratorTree)
  }

  def handleSingleFormalParam(formParamTree: AST[Node], typeAndName: (String,String), globalTable: GlobalTable): AST[Node]={
    val typeNode = formParamTree.node.toNodes.head
    val node = if(globalTable.primitiveTypes.contains(typeAndName._1))typeNode.toNodes(1) else typeNode.toNodes.head
    val typeTree = FillerUtils.visitUntilDFS("<identifier>", node, formParamTree.add(typeNode,IDGenerator.nextID)).add(new Node(typeAndName._1, false, List(), true),IDGenerator.nextID)
    val varDeclIDNode = formParamTree.node.toNodes(1)
    val nodeAfter = varDeclIDNode.toNodes.head
    val varDecl = FillerUtils.visitUntilDFS("<identifier>",nodeAfter,formParamTree.add(varDeclIDNode,IDGenerator.nextID)).add(new Node(typeAndName._2,false,List(),true),IDGenerator.nextID)
    formParamTree
  }

  def handleMethodHeader(methodHeaderTree: AST[Node], signature: MethodSignature, globalTable: GlobalTable,forInterface: Boolean):AST[Node]={
    // if the method to be filled is from in Interface but it has to be added for a class and not for an interface, then add the public modifier
    if(!forInterface && signature.fromInterface){
      val modifierNode = methodHeaderTree.node.toNodes.head
      val modifierTree = methodHeaderTree.add(modifierNode,IDGenerator.nextID)
      // TODO: Randomize over the modifiers and use Prolog to validate
      val specificModifier = modifierNode.toNodes.head.toNodes.head
      val specificModifierTree = modifierTree.add(new Node(specificModifier.description,specificModifier.alternative,specificModifier.edges,specificModifier.terminal),IDGenerator.nextID)
    }

    val resultTypeNode = methodHeaderTree.node.toNodes(1)
    val resultTypeTree = methodHeaderTree.add(resultTypeNode,IDGenerator.nextID)
    val typeNode = resultTypeNode.toNodes.head
    val typeTree = resultTypeTree.add(typeNode,IDGenerator.nextID)
    val nodeType = if(globalTable.primitiveTypes.contains(signature.returnType)) typeNode.toNodes(1) else typeNode.toNodes.head
    val nodeTypeTree = typeTree.add(typeNode,IDGenerator.nextID)
    val prefix = if(!forInterface && signature.fromInterface) "" else "\n\t"
    val specificReturnTypeTree = FillerUtils.visitUntilDFS("<identifier>",nodeType,nodeTypeTree).add(new Node(prefix+signature.returnType,false,List(),true),IDGenerator.nextID)
    val methodDeclaratorNode = methodHeaderTree.node.toNodes(2)
    val methodDeclTree = methodHeaderTree.add(methodDeclaratorNode,IDGenerator.nextID)
    val idNode = methodDeclaratorNode.toNodes.head
    val ideTree = methodDeclTree.add(idNode,IDGenerator.nextID).add(new Node(signature.name,false,List(),true),IDGenerator.nextID)
    val childrenRemained = methodDeclaratorNode.toNodes.tail
    val paren1 = methodDeclTree.add(childrenRemained.head,IDGenerator.nextID)
    if(signature.formalParameters.nonEmpty) {
      val formalParamListNode = childrenRemained(1)

      val formalParamTree = FillerUtils.handleFormalParams(formalParamListNode,methodDeclTree, signature.formalParameters,globalTable)
    }
    val paren2 = methodDeclTree.add(childrenRemained.last,IDGenerator.nextID)
    methodDeclTree
  }

}
