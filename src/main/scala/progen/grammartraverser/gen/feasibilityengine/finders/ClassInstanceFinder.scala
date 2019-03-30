package progen.grammartraverser.gen.feasibilityengine.finders
import progen.grammarparser.Node
import progen.grammartraverser.AST
import progen.grammartraverser.fill.FillerUtils
import progen.grammartraverser.utils.{IDGenerator, IdentifierHandler}
import progen.peg.entities.{ConstructorSignature, GlobalTable}
import progen.prolog.ClientRpc
import progen.symtab.{SymTab, TypeHandler}

import scala.util.Random

class ClassInstanceFinder extends Finder {
  override def findCompletedAST(ast: AST[Node], tab: SymTab, clientRpc: ClientRpc, possibleIds: List[String], context: GlobalTable): Option[AST[Node]] = {
    def loop(possibleNames: List[String],possibleTypesAndNames:List[(String,String)]): Option[AST[Node]] = possibleNames match {
      case Nil => None
      case list =>
        val className = Random.shuffle(list).head
        val constructors = tab.lookUpConstructors(className)
        constructors match {
          case Some(cs) =>
            def innerLoop(constrs: List[ConstructorSignature]): Option[AST[Node]] = constrs match {
              case Nil => loop(possibleNames.filter(_ != className),possibleTypesAndNames)
              case h :: t =>
                val argTypes = h.formalParameters.map(_._1)
                val argTypesMap = argTypes.groupBy(identity).mapValues(_.size)

                val possibleTypes = possibleTypesAndNames.map(_._1)
                val possibleTypesMap = possibleTypes.groupBy(identity).mapValues(_.size)
                // if for all argument types of the selected constructor there is at least the same
                // number of possible types to use, then populate the class instance creation expression.
                // Otherwise, look for another constructor of the selected class
                if (argTypes.forall(t => possibleTypesMap.getOrElse(t,0)>=1)) {
                  TypeHandler.nodeType = className
                  Some(populateClassInstanceCreation(className, ast, argTypes, possibleTypesAndNames,clientRpc))
                }
                else
                  innerLoop(t)
            }

            innerLoop(Random.shuffle(cs))
          // here I check all the constructors and add the trees if found the right thing
          case None => loop(possibleNames.filter(_ != className),possibleTypesAndNames)
        }
    }
    ast.children.clear()
    val possibleTypesAndNames = IdentifierHandler.retAllNames(tab).map(name => (tab.lookUpType(name) match {
      case Some(tr) => tr
      case None => "void"
    }, name)).filter(tn => tn._1 != "void")

    val ret = loop(possibleIds,possibleTypesAndNames)
    ret
  }
  def populateClassInstanceCreation(className: String,ast: AST[Node],argTypes: List[String], typesAndNames: List[(String,String)],clientRpc: ClientRpc): AST[Node] ={
    val newNode = ast.node.toNodes.head
    val newTree = ast.add(newNode,IDGenerator.nextID)
    val classTypeNode = ast.node.toNodes(1)
    val nTree = FillerUtils.visitUntilDFS("<identifier>", classTypeNode,ast).add(new Node(className,false,List(),true),IDGenerator.nextID)
    val paren1Node = ast.node.toNodes(2)
    val paren1Tree = ast.add(paren1Node,IDGenerator.nextID)
    val argLstNode = ast.node.toNodes(3)
    val argLstTree = ast.add(argLstNode,IDGenerator.nextID)
    FinderUtils.populateArgs(argLstTree,argTypes,typesAndNames,clientRpc )
    val paren2Node = ast.node.toNodes(4)
    val paren2Tree = ast.add(paren2Node,IDGenerator.nextID)
    ast

  }

}
