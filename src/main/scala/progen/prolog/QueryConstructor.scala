package progen.prolog

import java.io.{File, PrintWriter}

import progen.grammarparser.Node
import progen.grammartraverser.AST
import progen.grammartraverser.fill.FillerUtils
import progen.symtab.{SymTab, SymTabEntryKind, TypeHandler}

import scala.collection.{mutable => m}

object QueryConstructor {

    var counter: m.HashMap[String,Int] = m.HashMap()
    def askProlog(ast: AST[Node],clientRpc: ClientRpc,symTab: SymTab): Boolean = ast.node.description match{
      case "<returnstatement>" =>
        val retType = TypeHandler.nodeType

        val id = ast.parent match{
          case Some(p) => p.id
          case None => ast.id -1
        }
        val depth = ast.depth
        val response = clientRpc.validateRetStmt(retType,id,depth)
        val retTypeMethod = symTab.getRetType(symTab) match{
          case Some(r) => r
          case None => "void"
        }


        response
      case "this ( <argument list>? ) ; " =>
          val parId = ast.parent match{
            case Some(p) => p.id
            case None => Integer.MIN_VALUE
          }
        val argsLst = takeArgLst(ast,clientRpc)
        val response = clientRpc.validateThisInvocation(argsLst,parId,ast.depth)
        response

      case " super ( <argument list>? ) ;" =>
        val parId = ast.parent match{
          case Some(p) => p.id
          case None => Integer.MIN_VALUE
        }
          val argsLst = takeArgLst(ast,clientRpc)
          val response = clientRpc.validateSuperInvocation(argsLst,parId,ast.depth)
        response

      case "<variableinitializer>" =>
        val localVarDeclTree = ast.findRootTree("<localvariabledeclaration>",ast)
        val typeTree = localVarDeclTree.children.head
        val expected = FillerUtils.findTrees(typeTree,"<identifier>").head.children.head.node.description
        // going from <variableinitializer> to <costantexpression> or <classinstancecreationexpression>
        val childTree = ast.children.head.children.head
        val childTreeDescr = childTree.node.description
        //val actualType = TypeHandler.nodeType
        val childrenOfVarInitTree = ast.children.head.children.head
        val actualType = clientRpc.getType("X",childrenOfVarInitTree.id)
        val actType = actualType.substring(1,actualType.length -1)
        val response = clientRpc.validateVarInit(expected,actType)
        response
      case "<methodinvocation>" =>
        val name = ast.children.head.children.head.children.head.node.description
        val className = symTab.visitSymTab(SymTabEntryKind.CLASS,symTab) match{
          case Some(st) => st.symTabEntry.name match{
            case Some(n) => n
            case None => sys.error("symtab of kind CLASS has no name")
          }
          case None => sys.error("no symtab of kind CLASS found from method invocation tree")
        }
        val argsLst = takeArgLst(ast,clientRpc)
        val response =clientRpc.validateMethInvocation(name,className,argsLst)

        response
      case "<classinstancecreationexpression>" =>
        // the class name is the type of the expression
        val className = ast.children(1).children.head.children.head.children.head.node.description
        val args = takeArgLst(ast,clientRpc)
        val response = clientRpc.validateClassCreation(className,args)
        response
      case _ => true

    }

  /**
    * Takes the tree, looks at all the nodes <expression> in the subtree and call on them Prolog
    * to retrieve the types of the nodes
    * @param value the tree instance
    * @param clientRpc the client to ask Prolog
    * @return the list of the types for all the <expression> nodes found in subtrees of @param value
    */
    def takeArgLst(value: AST[Node],clientRpc: ClientRpc): List[String]={
      // look through all the expressions in the this invocation and call getType on them
      val trees = FillerUtils.findTrees(value,"<expression>")

      val typeValues = trees.map(t => clientRpc.getType("X",t.id)).map(str => str.replaceAll("\'",""))
      typeValues
    }
  def incrementCounter(key: String):Unit ={
    val prev = counter.getOrElse(key,0)
    val current = prev + 1
    counter(key) = current
  }
  def resetCounter(key: String): Unit ={
    counter(key) = 0
  }
  def getCounter(key: String): Int ={
    counter.getOrElse(key,0)
  }
}
