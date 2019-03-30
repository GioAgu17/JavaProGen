package progen.phase2.gen
import progen.grammarparser.{Graph, Node}
import progen.grammartraverser.fill.{FillerTest, FillerUtils}
import org.scalatest.FunSuite
import progen.grammartraverser.AST
import progen.grammartraverser.fill.FillerUtils
import progen.grammartraverser.gen.BodyGenerator
import progen.prolog.ClientRpc
import progen.symtab.SymTab


class BodyGeneratorTest extends FunSuite {
  val fillerTest: FillerTest = new FillerTest
  val classesAndSymbolTables: List[(AST[Node],SymTab)] = fillerTest.fillClasses
  val graph: Graph = fillerTest.grammarGraph
  val clientRpc: ClientRpc = fillerTest.clientRpc
  test("updateWeightsForRetStmt"){
    val methodBodyTree = FillerUtils.findTreesWithNoChildren(classesAndSymbolTables.head._1,"<methodbody>").head
    val symTabForMeth = methodBodyTree.parent match{
      case Some(parent) =>
        val methodName  =
          if(parent.children.head.children.head.node.description == "<resulttype>")
            parent.children.head.children(1).children.head.children.head.node.description
          else
            parent.children.head.children(2).children.head.children.head.node.description
        println(methodName)
        val symbolTable = classesAndSymbolTables.head._2
        val symTabClass = symbolTable
        val signatures = symbolTable.symTabEntry.methods match{
          case Some(ms) => ms
          case None => sys.error("signatures not found for class")
        }
        signatures.foreach(s => println(s.name))
        val signature = signatures.find(s => s.name == methodName)
        val symTab = signature match{
          case Some(ms) =>
            Option(Test.createMethSymTab(ms,symTabClass))
          case None =>  None
        }
        symTab
      case None => None
    }
    symTabForMeth match{
      case Some(symTab) =>
        val methodBodyTraversal = new BodyGenerator((fillerTest.globalTable,fillerTest.symbolTables),graph,clientRpc)

        graph.edges.foreach( e => println("EDGE FROM NODE "+e.fromNode+" TO NODE "+e.toNode+" WITH WEIGHT = "+e.weight))
      case None => println("ERROR IN FINDING SYM TAB FOR METHOD")
    }


  }

}
