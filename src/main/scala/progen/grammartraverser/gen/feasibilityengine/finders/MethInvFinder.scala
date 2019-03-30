package progen.grammartraverser.gen.feasibilityengine.finders
import progen.grammarparser.Node
import progen.grammartraverser.AST
import progen.grammartraverser.utils.{IDGenerator, IdentifierHandler}
import progen.peg.entities.GlobalTable
import progen.prolog.ClientRpc
import progen.symtab.{SymTab, SymTabEntryKind, TypeHandler}

import scala.util.Random

class MethInvFinder extends Finder {
  override def findCompletedAST(ast: AST[Node], tab: SymTab, clientRpc: ClientRpc, possibleIds: List[String], context: GlobalTable): Option[AST[Node]] = {
    def loop(possibleNames: List[String],possibleTypesAndNames: List[(String,String)]): Option[AST[Node]] = possibleNames match {
      case Nil => None
      case list =>
        val methodName = Random.shuffle(list).head
        val methSignature = tab.lookUpMethods match {
          case Some(ms) => ms.find(m => m.name == methodName) match {
            case Some(sign) => sign
            case None => sys.error("Method signature not found for method " + methodName)
          }
          case None => sys.error("method signatures not found in current scope")
        }
        val params = methSignature.formalParameters
        if (params.isEmpty) {
          addMethName(ast, methodName)
          val paren1Node = ast.node.toNodes(1)
          val paren1Tree = ast.add(paren1Node, IDGenerator.nextID)
          val paren2Node = ast.node.toNodes(3)
          val paren2Tree = ast.add(paren2Node, IDGenerator.nextID)
          Option(ast)
        } else {

          val argTypes = params.map(_._1)
         val newPossibleTypesAndNames =  if(argTypes.exists(t => context.primitiveTypes.contains(t))){
           val primitiveTypes = context.primitiveTypes.intersect(argTypes)
           val primitiveNodesDescr = primitiveTypes.map(p => FinderUtils.createNodeBasedOnPrimitiveType(p).description)
           val primitiveTypesAndNames = primitiveTypes zip primitiveNodesDescr
           possibleTypesAndNames ++ primitiveTypesAndNames
          } else possibleTypesAndNames

          val argTypesMap = argTypes.groupBy(identity).mapValues(_.size)
          val possibleTypes = newPossibleTypesAndNames.map(_._1)
          val possibleTypesMap = possibleTypes.groupBy(identity).mapValues(_.size)


          // if there are enough types
          if (argTypes.forall(t => possibleTypesMap.getOrElse(t,0)>=1)){
            TypeHandler.nodeType = methSignature.returnType
            Some(populateMethInv(methodName, ast, argTypes, newPossibleTypesAndNames,clientRpc))
          }
          else
            loop(possibleNames.filter(_ != methodName),possibleTypesAndNames)
        }
    }
    ast.children.clear()
    val possibleTypesAndNames = IdentifierHandler.retAllNames(tab).map(name => (tab.lookUpType(name) match {
      case Some(t) => t
      case None => "void"
    }, name)).filter(tn => tn._1 != "void")

    val methName = tab.visitSymTab(SymTabEntryKind.METHOD,tab) match{
      case Some(t) =>
        t.symTabEntry.name match{
          case Some(n) => n
          case None => sys.error("no name found in sym tab of kind METHOD")
        }
      case None => ""
    }
    // for avoiding recursion
    val possibleNames = possibleIds.filter(name => name!=methName)

   val ret =  loop(possibleNames,possibleTypesAndNames)
    ret
  }

  def populateMethInv(name: String, ast:AST[Node], argTypes: List[String], typesAndNames: List[(String,String)],clientRpc: ClientRpc): AST[Node] ={
    addMethName(ast,name)
    val paren1Node = ast.node.toNodes(1)
    val paren1Tree = ast.add(paren1Node, IDGenerator.nextID)
    val argListNode = ast.node.toNodes(2)
    val argListTree = ast.add(argListNode,IDGenerator.nextID)
    FinderUtils.populateArgs(argListTree,argTypes,typesAndNames,clientRpc )
    val paren2Node = ast.node.toNodes(3)
    val paren2Tree = ast.add(paren2Node, IDGenerator.nextID)
    ast
  }
  def addMethName(ast: AST[Node],name: String): Unit ={
    val methodNameNode = ast.node.toNodes.head
    val methodNameTree = ast.add(methodNameNode,IDGenerator.nextID)
    val idNode = methodNameNode.toNodes.head
    val idTree = methodNameTree.add(idNode,IDGenerator.nextID)
    val node = new Node(name,false,List(),true)
    val newTree = idTree.add(node,IDGenerator.nextID)
  }
}
