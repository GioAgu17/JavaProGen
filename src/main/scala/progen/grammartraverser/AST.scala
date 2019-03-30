package progen.grammartraverser

import org.slf4j.{Logger, LoggerFactory}
import progen.grammarparser.Node
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
      val oldNode = this
      val parId = this.id

      if(!n.terminal)
        clientRpc.addNodeToProlog(n.description, id, parId, depth + 1)

      logger.trace(".........NODE "+n.description.toUpperCase+" ADDED WITH FATHER: "+oldNode.node.description.toUpperCase+"...............")
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
      var sentence = ""
      def loop(tree: AST[Node]): Unit ={
        for(i <- tree.children) {
          if (i.node.terminal){
            if(i.node.description == "{")
              sentence +=  " " + i.node.description + "\n\t"
            else if(i.node.description == ";")
              sentence +=  " " + i.node.description + "\n\t"
            else if(i.node.description == "}")
              sentence += " " + i.node.description +"\n"
            else
              sentence += " " + i.node.description
          }

          else
            loop(i)
        }
      }
      loop(root)
      sentence
    }
    def getSubTree(root: AST[Node]): String ={
      var sentence = ""
      def loop(tree: AST[Node]): Unit ={
        for(i <- tree.children){
          if(i.node.terminal)
            sentence = sentence+ " " + i.node.description
          else
            sentence = sentence + " " + i.node.description + " "+ loop(i)
        }
      }
      loop(root)
      sentence.replaceAll("\n","")
    }

  }



}
