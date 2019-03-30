package progen.grammartraverser.gen.feasibilityengine.finders
import progen.grammarparser.Node
import progen.grammartraverser.AST

import progen.grammartraverser.utils.{IDGenerator, IdentifierHandler}
import progen.peg.entities.GlobalTable
import progen.prolog.ClientRpc
import progen.symtab.{SymTab, SymTabEntryKind}

import scala.util.Random

class ReturnStmtFinder extends Finder {
  override def findCompletedAST(ast: AST[Node], tab: SymTab, clientRpc: ClientRpc, possibleIds: List[String], globalTab: GlobalTable): Option[AST[Node]] = {
    val retType = tab.getRetType(tab) match {
      case Some(r) => r
      case None => sys.error("ret type not found in METHOD sym tab")
    }

    val methName = tab.visitSymTab(SymTabEntryKind.METHOD, tab) match {
      case Some(table) => table.symTabEntry.name match {
        case Some(n) => n
        case None => sys.error("sym tab of kind METHOD does not have a name")
      }
      case None => sys.error("sym tab of kind METHOD not found in return statement")
    }
    // now look into possible method invocations and class instance creation expressions
    val methodNames = tab.lookUpMethods match {
      case Some(ms) => ms.filter(m => m.returnType == retType && m.name != methName).map(s => (s.name, "mi"))
      case None => List()
    }
    val classNames = globalTab.classNames.filter(p => p == retType).map(s => (s, "class"))
    val suitableNames = methodNames ++ classNames
    val stmtExprTree = ast.children(1)


    def findFromPossibilities(suitableNames: List[(String,String)], stmtExprTree: AST[Node]): Option[AST[Node]] = {
      val stmtExprOrConst = Random.nextInt(2)
      val primitive = globalTab.primitiveTypes.contains(retType)
      if ( (primitive&& stmtExprOrConst == 1)||(primitive && suitableNames.isEmpty)) {
        ast.children.clear()
        val retNode = ast.node.toNodes.head
        val retTree = ast.add(retNode, IDGenerator.nextID)
        val newNode = FinderUtils.createNodeBasedOnPrimitiveType(retType)
        val newTree = ast.add(newNode, IDGenerator.nextID)
        val semiColonNode = ast.node.toNodes(2)
        val semiColonTree = ast.add(semiColonNode, IDGenerator.nextID)
        Option(ast)
      }else if(suitableNames.isEmpty){
        None
      }
      else {
        val selectedName = Random.shuffle(suitableNames).head
        val res = selectedName._2 match {
          case "mi" =>

                stmtExprTree.children.clear()
                val methInNode = stmtExprTree.node.toNodes.head
                val methInTree = stmtExprTree.add(methInNode, IDGenerator.nextID)
                val methInvNode = methInNode.toNodes.head
                val methInvTree = methInTree.add(methInvNode, IDGenerator.nextID)
                val methInvFinder = FinderFactory.getFinder(methInvTree.node.description)
                methInvFinder.findCompletedAST(methInvTree, tab, clientRpc, methodNames.map(n => n._1), globalTab)


          case "class" =>

                stmtExprTree.children.clear()
                val classInstancNode = stmtExprTree.node.toNodes(1)
                val classInstancTree = stmtExprTree.add(classInstancNode, IDGenerator.nextID)
                val classInstanceNode = classInstancNode.toNodes.head
                val classInstanceTree = classInstancTree.add(classInstanceNode, IDGenerator.nextID)
                val classInstFinder = FinderFactory.getFinder(classInstanceTree.node.description)

                classInstFinder.findCompletedAST(classInstanceTree, tab, clientRpc, classNames.map(n => n._1), globalTab)

          case _ => None
        }


        res match{
          case Some(r) => res
          case None =>
            val newSuitableNames = suitableNames.filter(n => n!=selectedName)
            stmtExprTree.children.clear()
            findFromPossibilities(newSuitableNames,stmtExprTree)
        }
      }
    }
    val res = findFromPossibilities(suitableNames,stmtExprTree)
    res
  }


}
