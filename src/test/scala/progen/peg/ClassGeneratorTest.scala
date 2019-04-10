package progen.peg

import com.typesafe.config.ConfigFactory
import grizzled.slf4j.Logging
import network.server.ServerRPC
import org.scalatest.FunSuite
import progen.grammarparser.ParserGen
import progen.grammartraverser.fill.{ClassThread, InterfaceThread}
import progen.peg.entities.{GlobalTable, InheritanceChain}
import progen.symtab.SymTab.SymTabSimple
import progen.symtab.{SymTabEntry, SymTabEntryKind}
import progen.ConfigurationRetriever
import progen.grammartraverser.AST.ASTSimple
import progen.grammartraverser.TreeBuilder
import progen.prolog.ClientRpc

import scala.io.Source

class ClassGeneratorTest extends FunSuite with Logging {
    // Setup for the tests: switching on the Server and creating classes and somethings
    val bufferedSrc = Source.fromFile("src/main/resources/prolog.txt")
    val rules = bufferedSrc.getLines().toList
    bufferedSrc.close()
    val configurationRetriever = new ConfigurationRetriever(ConfigFactory.load("my_app.conf"))
    val clientRpc = new ClientRpc(configurationRetriever.getUseServerRPC)
    val ok = clientRpc.setRules(rules)
    assert(ok)
    ServerRPC.start()
    val classGenerator = new ClassGenerator(configurationRetriever,clientRpc)
    val classNames = List("A","C","F","G","H","I","L","M","N","O","P","Q","R","S","T","U","V","Z","B","D","E","W")
    val interfaceGenerator = new InterfaceGenerator(configurationRetriever,clientRpc)
    val interfaces = interfaceGenerator.genInterfaces(List("Inter0","Inter1","Inter2","Inter3","Inter4","Inter5","Inter6","Inter7","Inter8","Inter9","Inter10","Inter11","Inter12","Inter13","Inter14"),configurationRetriever.getAllowedTypes,classNames)
    val globalTable = new GlobalTable(interfaces,configurationRetriever.getAllowedTypes,classNames)
     val inheritanceChains = classGenerator.createInheritances(classNames)

  test("test of createChain function"){
        val response = clientRpc.addNodeToProlog("compilationunit",0,0,0)
        val superClass = "A"
        val inheritanceChain = classGenerator.createChain(superClass,classNames).chain.toSet
        val classTypeSet = classNames.toSet
        assert(inheritanceChain.subsetOf(classTypeSet))
    }
  // sometimes the test does not terminate due to the limited number of classes
  test("test of createInheritances function"){

    val inheritanceLists = inheritanceChains.map(_.chain)
    val listCardinality = inheritanceLists.map(_.size)
    val inheritanceSets = inheritanceLists.map(_.toSet)
    val setCardinality = inheritanceSets.map(_.size)
    // there are no duplicates
    assert(listCardinality == setCardinality)
    // create a list with every combination of the sets
    val listOfCombinations = inheritanceSets.combinations(2).toList
    // for every combination of sets they are all disjoint
    listOfCombinations.foreach(setsList => assert(setsList.head.intersect(setsList.reverse.head).isEmpty))
  }

  test("genMethSign function"){
      val totChains = classGenerator.detTotChains(inheritanceChains,globalTable)
      val interfaceMap = classGenerator.assignInterfaces(globalTable)
      val (methodsMap,lastOffset) = classGenerator.genMethSign(totChains,globalTable,interfaceMap,0)
      assert(classNames.size==methodsMap.keys.size)
      // in every class every method name is different
      classNames.foreach(c => assert(methodsMap(c).size == methodsMap(c).map(ms => ms.name).distinct.size ))
  }

  test("assignInterfaces function"){
      val interfaceMap = classGenerator.assignInterfaces(globalTable)
      assert(interfaceMap.size == classNames.size)
      classNames.foreach(c => interfaceMap(c) match{
        case Some(l) => assert(l.size == l.map(l=>l.name).distinct.size)
        case None =>
      })
  }

  test("genFields function"){
    val totChains = classGenerator.detTotChains(classGenerator.createInheritances(classNames),globalTable)
    val fieldMap = classGenerator.genFields(totChains,globalTable)
    assert(fieldMap.size == classNames.size)
    classNames.foreach(c => assert(fieldMap(c).size == fieldMap(c).map(f => f.name).distinct.size))
  }

  test("findSuperClass function"){
    val inhChain = new InheritanceChain(List("A","B","C"))
    val totChains = classGenerator.detTotChains(List(inhChain),globalTable)
    val superClass1 = classGenerator.findSuperClass("A",totChains)
    val superClassExpected1 = "B"
    assert(superClass1 == Some(superClassExpected1))
    val superClass2 = classGenerator.findSuperClass("B",totChains)
    val superClassExpected2 = "C"
    assert(superClass2 == Some(superClassExpected2))
    val superClass3 = classGenerator.findSuperClass("C",totChains)
    assert(superClass3 == None)

  }

  test("detTotChains function"){
    val totChains = classGenerator.detTotChains(inheritanceChains,globalTable)
    val allClasses = totChains.flatMap(c => c.chain)
    val allClassesSet = allClasses.toSet
    assert(allClasses.size == allClassesSet.size)
    // I want to assert that in the totchains there are all the classes
    assert(classNames.toSet == allClassesSet)
  }


  test("genClasses function"){
    val classesAndLastOffset = classGenerator.genClasses(globalTable)
    val classes = classesAndLastOffset._1
    assert(classes.size == globalTable.classNames.size)
    val symTabCU = SymTabSimple(new SymTabEntry(SymTabEntryKind.COMPILATIONUNIT,None,None,None,None,None,None,None,None,None),None,None)
    val symTabs = classes.map(c => SymTabSimple(new SymTabEntry(SymTabEntryKind.CLASS,Option(c.name),c.superClass,c.interfaces,Option(c.methods),Option(c.constructors),Option(c.fields),None,None,None),Option(symTabCU),None))
    symTabCU.next = Option(symTabs)



    val context = (globalTable,symTabs)
    val gramBufferedSrc = Source.fromFile("src/main/resources/featherweight_java.txt")
    val gramFile = gramBufferedSrc.getLines().toList
    val grammarGraph = ParserGen.buildGraph(gramFile)
    val nodeRoot = grammarGraph.nodes.find(_.description == "<compilationunit>")
    val root = nodeRoot match{
      case Some(n) => n
      case None => sys.error("node with class declaration not found")
    }
    val cuTree = ASTSimple(root,clientRpc,0,0)
    val treeBuilder = new TreeBuilder(configurationRetriever,clientRpc)

    val typeDeclTrees = treeBuilder.fillTypeDeclTrees(globalTable,cuTree)
    val splittedTypeDeclTrees = typeDeclTrees.splitAt(globalTable.interfaces.size)
    val interfTypeDeclTrees = splittedTypeDeclTrees._1
    val typeDeclTreesInterfAndInterf = interfTypeDeclTrees zip globalTable.interfaces
    val interfacesThreads = typeDeclTreesInterfAndInterf.map(t => new InterfaceThread(t._2,globalTable,t._1))
    interfacesThreads.foreach(_.run())

    val classTypeDeclTrees = splittedTypeDeclTrees._2
    val symTabsAndClassTypeDeclTrees = symTabs zip classTypeDeclTrees
    val classThreads = symTabsAndClassTypeDeclTrees.map(st => new ClassThread(context,grammarGraph,clientRpc,st._1,st._2))
    classThreads.foreach(_.run())

  }


}
