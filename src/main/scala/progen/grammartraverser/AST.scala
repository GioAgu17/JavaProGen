package progen.grammartraverser

import org.apache.commons.lang3.StringUtils
import org.slf4j.{Logger, LoggerFactory}
import progen.grammarparser.Node
import progen.grammartraverser.utils.GlobalVariables
import progen.prolog.ClientRpc

import scala.collection.mutable.ListBuffer

trait AST[A] {
  val node: A
  val parent: Option[AST[A]]
  val children: ListBuffer[AST[A]]
  val clientRpc: ClientRpc
  val depth: Int
  val id: Int
  def size: Int
  def add(n: A,id: Int) : AST[A]
  def getClassRep: String
  def getInterfRep: String
  def getTreeRep(tree: AST[Node]): String
  def findRootTree(description: String, tree: AST[A]): AST[A]
  def getSubTree(tree: AST[Node]): String
}

object AST  {
  val logger: Logger = LoggerFactory.getLogger("trace")
  case class ASTSimple(override val node: Node,override val clientRpc: ClientRpc,override val depth: Int,override val id: Int) extends AST[Node] {

    override val children: ListBuffer[AST[Node]] = ListBuffer[AST[Node]]()
    override val parent: Option[AST[Node]] = None



    override def add(n: Node,id: Int): AST[Node] = {
     val stringBuilder = new StringBuilder
      val oldNode = this
      val parId = this.id

      if(!n.terminal)
        clientRpc.addNodeToProlog(n.description, id, parId, depth + 1)
      stringBuilder.append("..............NODE ")
      stringBuilder.append(n.description)
      stringBuilder.append(" ADDED WITH FATHER: ")
      stringBuilder.append(oldNode.node.description)
      stringBuilder.append("........................")
      val toTrace = stringBuilder.mkString
      logger.trace(toTrace)

      val newNode = new ASTSimple(n,clientRpc,depth+1,id) {
        override val parent: Option[AST[Node]] = Some(oldNode)
      }
      this.children += newNode
      newNode
    }


    override def size: Int = {
      def _size(t: AST[Node]): Int = {
        1 + t.children.foldLeft(0)((sum, tree) => sum + _size(tree))
      }

      _size(this)
    }
    def findRootTree(nodeDescription: String, tree: AST[Node]): AST[Node] = tree.node.description match{
      case `nodeDescription` => tree
      case _ =>  tree.parent match{
        case Some(t) => findRootTree(nodeDescription,t)
        case None => sys.error("no tree found with root called: "+nodeDescription)
      }
    }

    def getClassRep: String ={
      val root = findRootTree("<classdeclaration>",this)
      getTreeRep(root)
    }

    def getInterfRep: String ={
      val root = findRootTree("<interfacedeclaration>",this)
      getTreeRep(root)
    }

    def getTreeRep(root: AST[Node]): String={
      val strBuilder = new StringBuilder
      def loop(tree: AST[Node]): Unit ={
        for(i <- tree.children) {
          if (i.node.terminal){
            val descr = i.node.description
            if(descr == "{" || descr == ";" || descr == "}") {
              strBuilder.append(" ")
              strBuilder.append(descr)
              strBuilder.append("\n\t")
              GlobalVariables.LOC +=1
            }

            else if(descr == "}") {
              strBuilder.append(" ")
              strBuilder.append(descr)
              strBuilder.append("\n")
              GlobalVariables.LOC +=1
            }
            else {
              strBuilder.append(" ")
              strBuilder.append(descr)
            }
          }

          else
            loop(i)
        }
      }
      loop(root)
      strBuilder.mkString
    }
    def getSubTree(root: AST[Node]): String ={
      val stringBuilder = new StringBuilder
      def loop(tree: AST[Node]): Unit ={
        for(i <- tree.children){
          if(i.node.terminal){
            stringBuilder.append(" ")
            stringBuilder.append(i.node.description)
          }
          else{
            stringBuilder.append(" ")
            stringBuilder.append(i.node.description)
            stringBuilder.append(" ")
            stringBuilder.append(loop(i))
          }
        }
      }
      loop(root)
      StringUtils.replace(stringBuilder.mkString,"\n","")

    }

  }



}
