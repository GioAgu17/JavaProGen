package progen.grammartraverser.fill

import com.typesafe.config.{Config, ConfigFactory}
import progen.grammarparser.{Graph, Node, ParserGen}
import progen.{ConfigurationRetriever, WeightConfigurationRetriever}
import progen.peg._
import progen.peg.entities.{Class, GlobalTable, InheritanceChain, Interface}
import network.server.ServerRPC
import org.scalatest.FunSuite
import progen.grammartraverser.AST.ASTSimple
import progen.grammartraverser.{AST, TreeBuilder}
import progen.grammartraverser.utils.WeightInitializer
import progen.prolog.ClientRpc
import progen.symtab.SymTab

import scala.io.{BufferedSource, Source}

class FillerTest extends FunSuite {
  val bufferedSrc: BufferedSource = Source.fromFile("src/main/resources/prolog.txt")
  val rules: List[String] = bufferedSrc.getLines().toList
  bufferedSrc.close()
  val configurationRetriever = new ConfigurationRetriever(ConfigFactory.load("my_app.conf"))
  val clientRpc = new ClientRpc(configurationRetriever.getUseServerRPC)
  val ok: Boolean = clientRpc.setRules(rules)
  assert(ok)
  ServerRPC.start()
  val classGenerator = new ClassGenerator(configurationRetriever,clientRpc)
  val classNames = List("A","C","F","G","H","I","L","M","N","O","P","Q","R","S","T","U","V","Z","B","D","E","W")
  val interfaceGenerator = new InterfaceGenerator(configurationRetriever,clientRpc)
  val interfaces: List[Interface] = interfaceGenerator.genInterfaces(List("Inter0","Inter1","Inter2","Inter3","Inter4","Inter5","Inter6","Inter7","Inter8","Inter9","Inter10","Inter11","Inter12","Inter13","Inter14"),configurationRetriever.getAllowedTypes,classNames)
  val globalTable = new GlobalTable(interfaces,configurationRetriever.getAllowedTypes,classNames)
  val inheritanceChains: List[InheritanceChain] = classGenerator.createInheritances(classNames)
  val symbolTableGenerator = new SymbolTableGenerator(configurationRetriever,clientRpc)
  val classesAndLastOffSet: (List[Class],Int) = classGenerator.genClasses(globalTable)
  val symbolTables:List[SymTab] = symbolTableGenerator.fillClassesIntoSymTables(classesAndLastOffSet._1)


  val gramBufferedSrc : BufferedSource= Source.fromFile("src/main/resources/featherweight_java.txt")
  val gramFile: List[String] = gramBufferedSrc.getLines().toList
  val grammarGraph: Graph = ParserGen.buildGraph(gramFile)


//  val alternativeNodes: Set[Node] = grammarGraph.nodes.filter(_.alternative)
//  alternativeNodes.foreach(n => println(n.description + "  ->  "+n.edges.map(t => (t.toNode.description,t.weight)).mkString(", ")))
  val rootNode: Option[Node] = grammarGraph.nodes.find(_.description == "<compilationunit>")
  val node: Node = rootNode match{
    case Some(r) => r
    case None => sys.error("node compilation unit not found in graph")
  }

  val cuTree = ASTSimple(node,clientRpc,0,0)
  val treeBuilder = new TreeBuilder(configurationRetriever,clientRpc)
  val typeDeclTrees : List[AST[Node]]= treeBuilder.fillTypeDeclTrees(globalTable,cuTree)
  val splittedTypeDeclTrees: (List[AST[Node]],List[AST[Node]]) = typeDeclTrees.splitAt(globalTable.interfaces.size)





  def fillInterfaces : List[AST[Node]] ={
    val interfTypeDeclTrees: List[AST[Node]] = splittedTypeDeclTrees._1
    val typeDeclTreesInterfAndInterf: List[(AST[Node],Interface)] = interfTypeDeclTrees zip globalTable.interfaces
    val interfacesThreads:List[InterfaceThread] = typeDeclTreesInterfAndInterf.map(t => new InterfaceThread(t._2,globalTable,t._1))
    interfacesThreads.foreach(_.run())

    val interfTrees = interfacesThreads.map(it => it.typeDeclTree.children.head)
    interfTrees
  }

  def fillClasses : List[(AST[Node], SymTab)] ={
    val context: (GlobalTable,List[SymTab]) = (globalTable,symbolTables)
    val classTypeDeclTrees: List[AST[Node]] = splittedTypeDeclTrees._2
    val symTabs = context._2
    val symTabsAndClassTypeDeclTrees: List[(SymTab,AST[Node])] = symbolTables zip classTypeDeclTrees
    WeightInitializer.setWeightsToConfiguration(grammarGraph.nodes.toList)
    val classThreads: List[ClassThread] = symTabsAndClassTypeDeclTrees.map(st => new ClassThread(context,grammarGraph,clientRpc,st._1,st._2))
    classThreads.foreach(_.run())
    // translate every symbol table into SymTab for traversal part
    val classTreesAndSyms = classThreads.map(ct => (ct.treeRoot.children.head,ct.classSymTab))
    classTreesAndSyms
  }
  test("test the fill Interfaces and fill Classes functions"){



    val interfTrees = fillInterfaces
    val classTrees = fillClasses

    interfTrees.foreach(i => assert(i.node.description == "<interfacedeclaration>"))
    classTrees.foreach(c => assert(c._1.node.description == "<classdeclaration>"))
  }
}


