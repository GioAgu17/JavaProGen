package progen.symtab

import java.io.{File, PrintWriter}

import progen.grammarparser.Node
import grizzled.slf4j.Logging
import progen.grammartraverser.AST
import progen.grammartraverser.fill.FillerUtils
import progen.prolog.ClientRpc

object TypeHandler extends Logging{
  var nodeType: String = "void"
  def handleNodeType(typeTree: Option[AST[Node]]): Unit = typeTree match{
    case Some(t) =>
      val idTree = t.children.head
      val name = idTree.children.head
      nodeType = name.node.description
    case None => error("there has been an error in handling the node type")
  }
  /**
    * Function that updates the current node type from the description of the nodes.
    * @param tree the option of an ast to return
    * @param symTab the sym tab related to the actual scope
    * @param node the node is the current node whose the information of its type is updated
    * @return the option of an ast
    */
  def updateCurrentNodeType(tree: Option[AST[Node]], symTab: SymTab, node: Node,id:Int,clientRpc: ClientRpc): Option[AST[Node]]= synchronized{
    node.description match{
      case "<primary>" =>
        val classSymTab = symTab.visitSymTab(SymTabEntryKind.CLASS,symTab)
        nodeType =  classSymTab match{
          case Some(st) => st.symTabEntry.name match{
            case Some(n) => n
            case None => "void"
          }
          case None => sys.error("symtab class not found")
        }
        clientRpc.addType(nodeType,id)
        tree
      case "<emptystatement>" =>
        nodeType = "void"
        clientRpc.addType(nodeType,id)
        tree
      case "<assignment>" =>
        nodeType = "void"
        clientRpc.addType(nodeType,id)
        tree
      case "<methodinvocation>" =>
        nodeType = findTypeForMethInvocation(tree,symTab)
        clientRpc.addType(nodeType,id)
        tree
      case "<localvariabledeclaration>" =>
        nodeType = "void"
        clientRpc.addType(nodeType,id)
        tree
      case "<typename>" =>
        handleNodeType(tree)
        clientRpc.addType(nodeType,id)
        tree
      case  "<variabledeclaratorid>" =>
        clientRpc.addType(nodeType,id)
        tree
      case "<expressionname>" =>
        val tr = tree match{
          case Some(t) => t
          case None =>   sys.error("error: tree with node <expressionname> is None")
        }
        val n = tr.children.headOption match{
          case Some(v) => v.children.headOption match{
            case Some(t) => Some(t.node.description)
            case None =>
              None
          }
          case None =>
            None
        }

        nodeType = n match{
          case Some(name) => symTab.lookUpType(name) match{
            case Some(t) => t
            case None => "void"
          }
          case None => "void"
        }
        clientRpc.addType(nodeType,id)
        tree
      case "<classinstancecreationexpression>" =>
        nodeType = tree match{
          case Some(ast) =>
            val typeNameTree = FillerUtils.findTrees(ast,"<typename>").head
            val nType = typeNameTree.children.head.children.head.node.description
            nType

          case None => "void"
        }
        clientRpc.addType(nodeType,id)
        tree
      case _ =>
        clientRpc.addType(nodeType,id)
        tree

    }
  }

  /**
    * This function finds the return type of the method invoked
    * @param tree the option of an ast
    * @param tab the scope in which the invocation is used
    * @return the return type for the method invoked
    */
  def findTypeForMethInvocation(tree: Option[AST[Node]], tab: SymTab): String =tree match{
    case Some(t) =>
      // t is <methodinvocation>: to go down until the name of method invoked must be down for three steps
      val tr1 = t.children.head
      val tr2 = tr1.children.head
      val tr3 = tr2.children.head
      val name = tr3.node.description
      val retType = tab.visitSymTab(SymTabEntryKind.CLASS,tab) match{
        case Some(sm) =>
          sm.symTabEntry.methods match{
            case Some(ms) =>
              ms.find(_.name == name) match{
                case Some(methSign) =>
                  methSign.returnType
                case None => ""
              }
            case None => ""
          }
        case None => ""
      }
      retType
    case None => ""
  }
  def allPossibleTypes(tab: SymTab): List[String] = {
    val fields = tab.lookUpFields match {
      case Some(v) => v
      case None => List()
    }
    val locVars = tab.lookUpLocVars(tab, List())
    val params = tab.symTabEntry.params match {
      case Some(p) => p
      case None => List()
    }

    fields.map(_._1) ++ locVars.map(_.sType) ++ params.map(_._1)
  }
}
