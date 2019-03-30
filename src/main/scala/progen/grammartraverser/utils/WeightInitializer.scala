package progen.grammartraverser.utils

import com.typesafe.config.{Config, ConfigFactory}
import progen.WeightConfigurationRetriever
import progen.grammarparser.{Edge, Graph, Node}
import progen.symtab.{SymTab, SymTabEntryKind}


object WeightInitializer {
  val config: Config = ConfigFactory.load("my_app.conf")
  val weightsConfigurRet: WeightConfigurationRetriever = new WeightConfigurationRetriever(config)
  var returnConfigOn: Boolean = false

 def setWeightsToConfiguration(nodes: List[Node]): Unit = synchronized{
    val nodesAltern: List[Node] = nodes.filter(n => n.alternative && n.description != "<statementwithouttrailingsubstatement>" )
    val edgesAndTheirWeights : List[(List[Edge],List[Double])] = nodesAltern.map(n => (n.edges,weightsConfigurRet.getWeightConfiguration(n.description,n.edges.size)))
    edgesAndTheirWeights.foreach(ew =>
      (ew._1 zip ew._2).foreach(e => e._1.weight = e._2)
    )
   if(!returnConfigOn){
     val stmtWithTrailSubStmtNode = nodes.find(_.description == "<statementwithouttrailingsubstatement>" ) match{
       case Some(n) => n
       case None => sys.error("no node with <statementwithouttrailingsubstatement> description found")
     }
     val edgesAndItsWeights = stmtWithTrailSubStmtNode.edges zip weightsConfigurRet.getWeightConfiguration(stmtWithTrailSubStmtNode.description,stmtWithTrailSubStmtNode.edges.size)
     edgesAndItsWeights.foreach( e => e._1.weight = e._2)
   }
  }
  // gives 1.0 to the weight of the edge with toNode == description
  def updateWeightsOnDescription(n: Node, description: String): Unit = synchronized{
    n.edges.foreach(e =>
      e.toNode.description match {
        case `description` => e.weight = 1.0
        case _ => e.weight = 0.0
      })
  }
  def updateWeightsForRetStmt(nodes: List[Node]): Unit = synchronized{

    val blockStmtsNode = nodes.find(n => n.description == "<blockstatements>")
    blockStmtsNode match {
      case Some(n) =>
        updateWeightsOnDescription(n, "<block statement> ")
      case None => sys.error("BLOCKSTMTS NODE NOT FOUND")
    }

    val blockStmtNode = nodes.find(n => n.description == "<blockstatement>")
    blockStmtNode match {
      case Some(n) =>
        updateWeightsOnDescription(n, " <statement>")

      case None => sys.error("BLOCKSTMT NODE NOT FOUND")
    }
    val stmtNode = nodes.find(n => n.description == "<statementwithouttrailingsubstatement>")
    stmtNode match{
      case Some(n) =>
        updateWeightsOnDescription(n," <return statement>")
      case None => sys.error("STMT NODE NOT FOUND")
    }
  }

  def setWeights(node: Node,symTab:SymTab,graph: Graph): Unit =synchronized {
    node.description match {
      case "<explicitconstructorinvocation>" =>
        // set <assignment> edge weight to 0 because it cannot go in explicit constructor invocations!
        // And by consequence <conditional expression> to 1.0
        setNoAssignment(graph)
        // set rule <postfix expression> ::= <primary> | <expression name> with first edge set to 0.0 and second edge set to 1.0
        setNoPrimary(graph)
        // look for the sym tab related to this class
        val classSymTab = symTab.visitSymTab(SymTabEntryKind.CLASS, symTab)
        classSymTab match {
          case Some(tab) =>
            // if there is, look for the super class of the class if exists
            tab.symTabEntry.superClass match {
              // if a super class exist, look for the sym tab of that class
              case Some(superClass) =>
                node.edges.foreach(e => e.toNode.description match {
                  case "this ( <argument list>? ) ; " =>
                    e.weight = 0.5
                  case " super ( <argument list>? ) ;" =>
                    e.weight = 0.5
                })
              case None =>
                node.edges.foreach(e => e.toNode.description match {
                  case "this ( <argument list>? ) ; " =>
                    e.weight = 1.0
                  case " super ( <argument list>? ) ;" =>
                    e.weight = 0.0
                })
            }
          case None => sys.error("no class sym tab found")
        }

      case "<methodinvocation>" =>
        setNoAssignment(graph)
      case "<classinstancecreationexpression>" =>
        setNoAssignment(graph)
      case _ =>

    }
  }
  def setNoAssignment(graph:Graph): Unit = synchronized{
    graph.edges.find(e => e.fromNode.description == "<assignmentexpression>" && e.toNode.description == " <assignment>") match{
      case Some(edge) => edge.weight =0.0
        edge.fromNode.edges.head.weight = 1.0
      case None => sys.error("no edge found from <assignmentexpression> to  <assignment>")
    }
  }

  def resetWeightsToConfiguration(node: Node, graph:Graph): Unit = synchronized{
    node.description match {
      case "<explicitconstructorinvocation>" => setWeightsToConfiguration(graph.nodes.toList)
      case "<methodinvocation>" => setWeightsToConfiguration(graph.nodes.toList)
      case "<classinstancecreationexpression>" => setWeightsToConfiguration(graph.nodes.toList)
      case _ =>
    }
  }
  def setNoPrimary(graph: Graph): Unit ={
    val postfixeExprNode = graph.nodes.find(n => n.description == "<postfixexpression>") match{
      case Some(postFix) => postFix
      case None => sys.error("did not found <postfix expression> node in all nodes")
    }
    val edges = postfixeExprNode.edges
    val edgePrimary  = edges.head
    edgePrimary.weight = 0.0
    val edgeExprName = edges(1)
    edgeExprName.weight = 1.0

  }


}
