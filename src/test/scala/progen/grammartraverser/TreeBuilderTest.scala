package progen.grammartraverser

import com.typesafe.config.ConfigFactory
import network.server.ServerRPC
import org.scalatest.FunSuite
import progen.ConfigurationRetriever
import progen.grammarparser.{Graph, ParserGen}
import progen.grammartraverser.utils.GlobalVariables
import progen.peg._
import progen.peg.entities.{Class, GlobalTable, InheritanceChain, Interface}
import progen.prolog.{ClientRpc, QueryConstructor}
import progen.symtab.{ScopeHandler, SymTab, TypeHandler}

import scala.io.{BufferedSource, Source}

class TreeBuilderTest extends FunSuite{
  disableWarning()
  val bufferedSrc: BufferedSource = Source.fromFile("src/main/resources/prolog.txt")
  val rules: List[String] = bufferedSrc.getLines().toList
  bufferedSrc.close()
  val configurationRetriever = new ConfigurationRetriever(ConfigFactory.load("my_app.conf"))
  val clientRpc = new ClientRpc(configurationRetriever.getUseServerRPC)
  val ok: Boolean = clientRpc.setRules(rules)
  assert(ok)
  ServerRPC.start()
  val classGenerator = new ClassGenerator(configurationRetriever,clientRpc)
  val classNames = List("A","C","F","G","H","I","L","M","N","O")
  val interfaceGenerator = new InterfaceGenerator(configurationRetriever,clientRpc)
  val interfaces: List[Interface] = interfaceGenerator.genInterfaces(List("Inter0","Inter1","Inter2","Inter3","Inter4","Inter5","Inter6","Inter7"),configurationRetriever.getAllowedTypes,classNames)
  val globalTable = new GlobalTable(interfaces,configurationRetriever.getAllowedTypes,classNames)
  val inheritanceChains: List[InheritanceChain] = classGenerator.createInheritances(classNames)
  val symbolTableGenerator = new SymbolTableGenerator(configurationRetriever,clientRpc)
  val classesAndLastOffSet: (List[Class],Int) = classGenerator.genClasses(globalTable)
  val lastOffSet: Int = classesAndLastOffSet._2
  val symbolTables:List[SymTab] = symbolTableGenerator.fillClassesIntoSymTables(classesAndLastOffSet._1)
  val gramBufferedSrc : BufferedSource= Source.fromFile("src/main/resources/featherweight_java.txt")
  val gramFile: List[String] = gramBufferedSrc.getLines().toList
  val grammarGraph: Graph = ParserGen.buildGraph(gramFile)
  val context: ((List[SymTab],Int),GlobalTable) = ((symbolTables,lastOffSet),globalTable)

  // update loc
  GlobalVariables.LOC = classNames.size + interfaces.size


  test("second phase generation"){
    val treeBuilder = new TreeBuilder(configurationRetriever,clientRpc)
    val tree = treeBuilder.secondPhaseGeneration(grammarGraph,context)

  }
  def disableWarning(): Unit = {
    System.err.close()
    System.setErr(System.out)
  }

}
