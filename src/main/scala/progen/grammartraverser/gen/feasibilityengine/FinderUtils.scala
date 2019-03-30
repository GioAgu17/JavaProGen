package progen.grammartraverser.gen.feasibilityengine.finders

import progen.grammarparser.Node
import progen.grammartraverser.AST
import progen.grammartraverser.fill.FillerUtils
import progen.grammartraverser.utils.{IDGenerator, IdentifierHandler}
import progen.prolog.ClientRpc

import scala.collection.mutable.ListBuffer

object FinderUtils {

  def populateArgs(argLstTree: AST[Node], argTypes: List[String], typesAndNames: List[(String,String)],clientRpc: ClientRpc): AST[Node] ={
    def loop(node: Node, tree: AST[Node], typesAndNames: ListBuffer[(String,String)], argTypes: List[String]): AST[Node] = argTypes match{
      case Nil => tree
      case h :: Nil =>
        val exprSpaceNode = node.toNodes.head
        val exprNode = exprSpaceNode.toNodes.head
        val exprNodeID = IDGenerator.nextID
        val assExprNode = exprNode.toNodes.head
        val condExprNode = assExprNode.toNodes.head
        val condExprTree = tree.add(exprNode,exprNodeID).add(assExprNode,IDGenerator.nextID).add(condExprNode,IDGenerator.nextID)
        val postFixTree = FillerUtils.visitUntilDFS("<postfixexpression>",condExprNode.toNodes.head,condExprTree)
        val exprNameNode = postFixTree.node.toNodes(1).toNodes.head
        val exprNameTree = postFixTree.add(exprNameNode,IDGenerator.nextID)
        val idN = exprNameNode.toNodes.head
        val idTree = exprNameTree.add(idN,IDGenerator.nextID)
        val name = typesAndNames.find(nt => nt._1 == h ) match{
          case Some(n) => n
          case None => sys.error("variable with type "+h+" not found")
        }
        clientRpc.addType(name._1,exprNodeID)
        val newNode = new Node(name._2,false,List(),true)
        val newTree = idTree.add(newNode,IDGenerator.nextID)
        tree
      case h :: t =>
        val argListExprNode = node.toNodes(1)
        val argListExprTree = tree.add(argListExprNode,IDGenerator.nextID)
        val argListNode = argListExprNode.toNodes.head
        val argListTree = argListExprTree.add(argListNode,IDGenerator.nextID)
        val name = typesAndNames.find(nt => nt._1 == h ) match{
          case Some(n) => n
          case None => sys.error("variable with type "+h+" not found")
        }

        val newTree = loop(argListNode,argListTree,typesAndNames,t)
        val commaNode = argListExprNode.toNodes(1)
        val commaTree = argListExprTree.add(commaNode,IDGenerator.nextID)
        val lastTree = loop(argListExprNode,argListExprTree,ListBuffer(name),List(h))
        tree

    }
    val bufferList = typesAndNames.to[ListBuffer]
    loop(argLstTree.node,argLstTree,bufferList,argTypes.reverse)
  }
  def createNodeBasedOnPrimitiveType(primitiveType: String): Node = {
    val descr = IdentifierHandler.randomConstantExpression(primitiveType)
    new Node(descr, false, List(), true)
  }


}
