package progen.grammartraverser.gen

import progen.grammarparser.{Graph, Node}
import progen.peg.entities.{ConstructorSignature, GlobalTable, MethodSignature}
import progen.prolog.Check.ops._
import progen.prolog.{ClientRpc, QueryConstructor}
import grizzled.slf4j.Logging
import progen.grammartraverser.AST
import progen.grammartraverser.gen.feasibilityengine.finders.FinderFactory
import progen.grammartraverser.utils._
import progen.symtab.{ScopeHandler, SymTab, TypeHandler}

import scala.util.Random

class BodyGenerator(val context: (GlobalTable,List[SymTab]), val grammarGraph: Graph,val clientRpc: ClientRpc) extends Logging {


  /**
    * Calls a traversal function for the body of either a method or a constructor. At the end of the function
    * adds a return statement if the body is that of a method
    * @param bodyTree the subtree of the body
    * @return the tree filled with children
    */
  def traverse(bodyTree: AST[Node],symTab: SymTab): AST[Node]= synchronized{
    ScopeHandler.currentScope = symTab
    val possibleNames = IdentifierHandler.handleIdentifier(bodyTree,symTab,context,List())
    traverseBody(Option(bodyTree),possibleNames,symTab) match{
      case Some(t) =>
        val retType = symTab.symTabEntry.retType match{case Some(rt) => rt case None => "constructor"}
        if(retType!="void" && retType!="constructor")
          ReturnStmtAdder.addRetStmt(bodyTree, retType,symTab,this)
        t
      case None =>
        val name = symTab.symTabEntry.name match{
         case Some(n) => n
         case None => ""
        }
      sys.error("cannot complete generation - too few resources")
    }
  }

  /**
    *Traverses the body of either a method or a function by calling two different
    * functions depending if the tree has an alternative node or not
    * @param ast an option of an ast
    * @param possibleNames the list of possible names to give to the node if the node is a child of <identifier>
    * @return an option of an ast filled with the body
    */
  def traverseBody(ast: Option[AST[Node]],possibleNames: List[String],symTab:SymTab): Option[AST[Node]]= ast match{
    case Some(t) =>
      val node = t.node
      if (node.alternative)
        traverseAlternative(t,possibleNames,symTab)
      else
        traverseNonAlternative(t,possibleNames,symTab)
    case None =>
      None
  }

  /**
    * Traverses a tree whose node is alternative, e.g. its children are to be exclusively selected. Calls a weighted sampling on
    * the edges of the node and calls on it two different functions if the node
    * is a terminal symbol of the grammar or not
    * @param ast an ast
    * @param possibleNames the list of possible names to give to the node if the node is a child of <identifier>
    * @return an option of an ast
    */
  def traverseAlternative(ast: AST[Node],possibleNames: List[String],symTab:SymTab): Option[AST[Node]]={
    val edges = ast.node.edges
   val newNode = if(ast.node.description == "<variabledeclarator>" && TypeHandler.nodeType.contains("Inter")) edges.head.toNode
   else edges(WeightedSampler.weightedSampling(edges.map(_.weight))).toNode
    if (newNode.terminal) {
      determineTerminal(newNode,possibleNames,ast,symTab)
    }
    else traverseNonTerminal(newNode,ast,possibleNames,symTab)
  }

  /**
    * Traverses a tree t whose node is not alternative. Calls the two functions <determineTerminal> and
    * <traverseNonTerminal> on each child of the node of t
    * @param t the tree traversed
    * @param possibleNames the list of possible names to give to the node if the node is a child of <identifier>
    * @return an option of an ast
    */
  def traverseNonAlternative(t: AST[Node], possibleNames: List[String],symTab:SymTab): Option[AST[Node]] ={
    val node = t.node
    // this is to handle the optional nodes like <argument list>?
    val childrenNodes = node.edges.map(e =>
      if(e.optional){
        val i = WeightedSampler.weightedSampling(List(0.8,0.2))
        i match{
          case 0 => e.toNode
          case 1 => new Node("",false,List(),true)
        }
      }else
          e.toNode)

    val treesAdded = childrenNodes.map(c =>
      if (c.terminal)
        determineTerminal(c,possibleNames,t,symTab)
      else
        traverseNonTerminal(c,t,possibleNames,symTab)
      )
    // if the list has some None in the elements
    val c = treesAdded.count(_.isDefined)  == treesAdded.size
    // if the childrenNodes are not empty

//    val b = childrenNodes.nonEmpty
   if(!c) goBackToAltern(t,symTab) else Some(t)
  }

  /**
    * when some node is not accepted for the generation and it wasn't an alternative, the algorithm goes
    * upwards until an alternative is found and the other choice is taken and traversed
    * @param tree an ast
    * @return an option of an ast
    */
  def goBackToAltern(tree: AST[Node],symTab:SymTab):Option[AST[Node]]={
    def goUpwards(ast: AST[Node],tab:SymTab): (Option[AST[Node]],SymTab) ={
      ast.parent match{
        case Some(t) =>
          ScopeHandler.leaveScope(ast.node,tab)
          val st = ScopeHandler.currentScope
          if(t.node.alternative){
            val edges = t.node.edges
            val edgesWeights = edges.map(e => e.weight)
            val edgeSize = edges.size
            // updating the weights to exclude the previous selected edge
            edges.foreach(e => if(e.toNode.description == ast.node.description) e.weight = 0.0 else e.weight = 1/(edgeSize-1))
            val newNode = edges(WeightedSampler.weightedSampling(edges.map(_.weight))).toNode
            // restore previous weights value
            (edges zip edgesWeights).foreach(ew => ew._1.weight = ew._2)
           // val pickedUpNode =  Random.shuffle(t.node.toNodes.filter(n => n.description != ast.node.description)).head
            t.children -= ast
            ScopeHandler.enterScope(t.node,st)
            val newSymTab = ScopeHandler.currentScope
            (Option(t.add(newNode,IDGenerator.nextID)),newSymTab)
          }else{
            goUpwards(t,st)
          }
        case None => (None,tab)
      }
    }
    println("GOBACKTOALTERN with ast: "+tree.node.description.toUpperCase)
    val res = goUpwards(tree,symTab)
    res._1 match{
      case Some(t) =>
        ScopeHandler.enterScope(t.node,symTab)
        val newSymTab = ScopeHandler.currentScope
        val possibleNames = IdentifierHandler.handleIdentifier(t,newSymTab,context,List())
        traverseBody(Option(t),possibleNames,newSymTab)
      case None =>
        None
    }
  }

  /**
    * Traverses a non terminal node. First, it identifies the possible names to assign to <identifier> children nodes.
    * Then it calls the body traversal function on the new ast created by adding the new node to the previous tree.
    * When the function returns, the type of the node is updated. With all this information the Prolog KB is called to
    * check whether the node can be accepted or not. If yes, it returns the result of the traverseBody function. Otherwise
    * the subtree is cleared and the traversal function is called again.
    * @param newNode the node to add to the tree
    * @param ast the ast to fill with generated new node
    * @param possibleNames the list of possible names to give to the node if the node is a child of <identifier>
    * @return an option of an ast
    */
  def traverseNonTerminal(newNode: Node, ast: AST[Node],possibleNames: List[String],st: SymTab): Option[AST[Node]] ={
    ScopeHandler.enterScope(newNode,st)
    val symTab = ScopeHandler.currentScope
    val newPossibleNames = IdentifierHandler.handleIdentifier(ast,symTab,context,possibleNames)
    val id = IDGenerator.nextID
    WeightInitializer.setWeights(newNode,symTab,grammarGraph)
    val newTree = Option(ast.add(newNode,id))
    val branch = traverseBody(newTree,newPossibleNames,symTab) match{
      case Some(b) =>
        WeightInitializer.resetWeightsToConfiguration(newNode,grammarGraph)
        val ret = TypeHandler.updateCurrentNodeType(newTree,symTab,newNode,id,clientRpc)

        val res = ret match{
          case Some(v) =>
            val checkKB = v.checkWithKB(clientRpc,symTab)
            if(checkKB) {
              Some(v)
            }else {

              if(QueryConstructor.getCounter(v.node.description) < GlobalVariables.noOfPrologIterations) {
                ast.children -= v
                ScopeHandler.leaveScope(newNode,symTab)
                val newSt = ScopeHandler.currentScope
                QueryConstructor.incrementCounter(v.node.description)
                val result = traverseNonTerminal(newNode, ast, newPossibleNames,newSt)
                QueryConstructor.resetCounter(v.node.description)
                result
              }else {
                QueryConstructor.resetCounter(v.node.description)
                val names = IdentifierHandler.handleIdentifier(v,symTab,context,newPossibleNames)
                val rightFinder = FinderFactory.getFinder(v.node.description)
                val ret = rightFinder.findCompletedAST(v,symTab,clientRpc,names,context._1)
                ScopeHandler.leaveScope(newNode,symTab)
                ret
              }
            }
          case None =>
            ScopeHandler.leaveScope(newNode,symTab)
            val newSymTab = ScopeHandler.currentScope
            goBackToAltern(ast,newSymTab)
        }
        res
      case None => None
    }

    branch
  }


  /**
    * Determined the terminal to add to the tree. The terminal node is always accepted and does not need a Prolog check.
    * Thus, it is checked just if the node is the child of the <identifier> node. In this case, a String is randomly selected
    * among the possibleNames list and a new node is created with the String as the description attribute of the node.
    * @param newNode the terminal node
    * @param possibleNames the list of possible names to assign if the node is the child of the <identifier> node
    * @param t the tree to which the function adds the terminal node
    * @return an option of an ast
    */
  def determineTerminal(newNode: Node,possibleNames: List[String],t: AST[Node],tab:SymTab): Option[AST[Node]]={
    if(newNode.description == "[A-Za-z][A-Za-z0-9_]*"){
      possibleNames match{
        case Nil => None
        case list =>
          val name = Random.shuffle(list).head
          val n = new Node(name,false,List(),true)
          Option(t.add(n,IDGenerator.nextID))
      }
    }else{
      Option(t.add(newNode,IDGenerator.nextID))
    }
  }





}
