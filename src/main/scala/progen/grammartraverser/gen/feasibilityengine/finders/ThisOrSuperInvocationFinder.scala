package progen.grammartraverser.gen.feasibilityengine.finders
import progen.grammarparser.Node
import progen.grammartraverser.AST
import FinderUtils.populateArgs
import progen.grammartraverser.utils.{IDGenerator, IdentifierHandler}
import progen.peg.entities.{ConstructorSignature, GlobalTable}
import progen.prolog.{ClientRpc, QueryConstructor}
import progen.prolog.Check.ops._
import progen.symtab.{SymTab, SymTabEntryKind}

import scala.util.Random

class ThisOrSuperInvocationFinder(val thisOrSuper: Boolean) extends Finder {
  override def findCompletedAST(ast: AST[Node], tab: SymTab, clientRpc: ClientRpc, possibleIds: List[String], context: GlobalTable): Option[AST[Node]] = {
    if(thisOrSuper){
      val className = tab.symTabEntry.name match{
        case Some(n) => n
        case None => sys.error("error in symTab of supposed constructor, no name")
      }
      handleThisOrSuper(ast,tab,className,clientRpc)

    }else{
      val classTab = tab.visitSymTab(SymTabEntryKind.CLASS,tab) match{
        case Some(st) => st
        case None => sys.error("impossible to reach class sym tab from constructor sym tab")
      }

      val superClassName = classTab.symTabEntry.superClass match{
        case Some(r) => r
        case None => sys.error("ERROR: shouldn't call super if class has no super class")
      }
      handleThisOrSuper(ast,tab,superClassName,clientRpc)
    }
  }

  def handleThisOrSuper(ast: AST[Node], tab: SymTab, className:String,clientRpc: ClientRpc): Option[AST[Node]] ={
    def loop(constr: List[ConstructorSignature], possibleTypesAndNames: List[(String,String)]):Option[AST[Node]] =constr match{
      case Nil => None
      case list =>
        val cons = Random.shuffle(list).head
        val argTypes = cons.formalParameters.map(_._1)
        val argTypesMap = argTypes.groupBy(identity).mapValues(_.size)

        val possibleTypes = possibleTypesAndNames.map(_._1)
        val possibleTypesMap = possibleTypes.groupBy(identity).mapValues(_.size)
        // if for every type argument of the invocation there is at least one possible name,
        // start populating this invocation
        if (argTypes.forall(t => possibleTypesMap.getOrElse(t,0)>=1))
          Some(populateThisInvocation(ast,argTypes,possibleTypesAndNames,clientRpc))
        else
          loop(list.filter(_ != cons),possibleTypesAndNames)
    }
    val constructors = tab.lookUpConstructors(className) match{
      case Some(cs) => cs
      case None => sys.error("at least one constructor has to be found, i.e. the one in this call")
    }
    val constrEnclosingParams = tab.symTabEntry.params match{
      case Some(p) => p
      case None => List()
    }
    val constrs =
    if(constrEnclosingParams.isEmpty){
      constructors.filter(c => c.formalParameters != Nil)
    }else
      constructors.filter(c => c.formalParameters != constrEnclosingParams)


    if(constrs.isEmpty){
      ast.children.clear()
      val newEmptyNode = new Node("",false,List(),true)
      val newTree = ast.add(newEmptyNode,IDGenerator.nextID)
      val sTab = tab.leaveScope(tab,ast.node)
      Some(ast)
    }

    else {
      val possibleTypesAndNames = IdentifierHandler.retLocVarsAndParams(tab).map(name => (tab.lookUpType(name) match {
        case Some(tr) => tr
        case None => "void"
      }, name)).filter(tn => tn._1 != "void")
      ast.children.clear()
      loop(constrs,possibleTypesAndNames)
      val check = ast.checkWithKB(clientRpc, tab)
      if(check){
        Some(ast)
      }else{
        ast.children.clear()
        val newEmptyNode = new Node("",false,List(),true)
        val newTree = ast.add(newEmptyNode,IDGenerator.nextID)
        val sTab = tab.leaveScope(tab,ast.node)
        Some(ast)
      }

    }
  }
  def populateThisInvocation(ast:AST[Node],argTypes: List[String], typesAndNames: List[(String,String)],clientRpc: ClientRpc): AST[Node] ={

    val thisArgListNode = ast.node
    val thisNode = thisArgListNode.toNodes.head
    val thisTree = ast.add(thisNode,IDGenerator.nextID)
    val paren1 = thisArgListNode.toNodes(1)
    val paren1Tree = ast.add(paren1,IDGenerator.nextID)
    val argLst = thisArgListNode.toNodes(2)
    val argLstTree = ast.add(argLst,IDGenerator.nextID)
    populateArgs(argLstTree,argTypes,typesAndNames,clientRpc)
    val paren2 = thisArgListNode.toNodes(3)
    val paren2Tree = ast.add(paren2,IDGenerator.nextID)
    val colon = thisArgListNode.toNodes(4)
    val colonTree = ast.add(colon,IDGenerator.nextID)
    ast
  }
}
