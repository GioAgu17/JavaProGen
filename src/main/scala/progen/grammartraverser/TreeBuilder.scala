package progen.grammartraverser

import java.util.concurrent.{Executors, ThreadPoolExecutor, TimeUnit}

import grizzled.slf4j.Logging
import progen.ConfigurationRetriever
import progen.grammarparser.{Graph, Node}
import progen.grammartraverser.AST.ASTSimple
import progen.grammartraverser.fill.{ClassThread, FillerUtils, InterfaceThread}
import progen.grammartraverser.utils.{GlobalVariables, IDGenerator, WeightInitializer}
import progen.peg.entities.GlobalTable
import progen.prolog.ClientRpc
import progen.symtab.SymTab


class TreeBuilder(val configurationRetriever: ConfigurationRetriever,val clientRpc: ClientRpc) extends Logging{

  def secondPhaseGeneration(grammarGraph: Graph,context: ((List[SymTab],Int),GlobalTable)): AST[Node]={

    WeightInitializer.setWeightsToConfiguration(grammarGraph.nodes.toList)
    val cuTree = startSecondPhase(grammarGraph,context)
    cuTree
  }

  def startSecondPhase(grammarGraph: Graph, context: ((List[SymTab],Int), GlobalTable)): AST[Node]={
    // starting point of ID Generator
    val lastOffSet = context._1._2 + 1
    IDGenerator.id = lastOffSet
    val nodeRoot = grammarGraph.nodes.find(_.description == "<compilationunit>")
    val root = nodeRoot match {
      case Some(n) => n
      case None => sys.error("compilation unit node not found in grammar")
    }
    val symTabs = context._1._1
    val globalTable = context._2
    val cuTree = ASTSimple(root,clientRpc,0,lastOffSet)
    val typeDeclTrees = fillTypeDeclTrees(globalTable , cuTree)
    val splittedTypeDeclTrees = typeDeclTrees.splitAt(globalTable.interfaces.size)
    val interfTypeDeclTrees = splittedTypeDeclTrees._1
    val typeDeclTreesInterfAndInterf = interfTypeDeclTrees zip globalTable.interfaces
    val noOfInterfThreads = interfTypeDeclTrees.size
    val interfExecutor = Executors.newFixedThreadPool(noOfInterfThreads).asInstanceOf[ThreadPoolExecutor]
    for(t <- typeDeclTreesInterfAndInterf){
      val iThread  = new InterfaceThread(t._2, globalTable, t._1)
      interfExecutor.execute(iThread)
    }
    interfExecutor.shutdown()
    while(!interfExecutor.awaitTermination(10,TimeUnit.MINUTES)){
      info("Awaiting completion of all interface threads")
    }
    println("Lines of code of all interfaces: "+GlobalVariables.LOC)
    val classTypeDeclTrees = splittedTypeDeclTrees._2
    val symTabsAndClassTypeDeclTrees = symTabs zip classTypeDeclTrees
    // all classes without threads
   // symTabsAndClassTypeDeclTrees.foreach(st => new ClassThread((globalTable,symTabs),grammarGraph,clientRpc,st._1,st._2).run())


    // just first class
//    val st = symTabsAndClassTypeDeclTrees.head
//    val classThread = new ClassThread((globalTable,symTabs),grammarGraph,clientRpc,st._1,st._2)
//    classThread.run()


    // all classes with threads
    val classExecutor =  Executors.newFixedThreadPool(GlobalVariables.noOfClassThreads).asInstanceOf[ThreadPoolExecutor]
    // for each class, create the task and submit it to the executor to execute
    for(st <- symTabsAndClassTypeDeclTrees){
      val cThread = new ClassThread((globalTable,symTabs),grammarGraph,clientRpc,st._1,st._2)
      classExecutor.execute(cThread)
    }
    // previously tasks starts to execute, but no new tasks will be accepted
    classExecutor.shutdown()
    // blocks until all tasks have completed execution after a shutdown request
    while(!classExecutor.awaitTermination(20, TimeUnit.MINUTES)){
      info("Awaiting completion of all class threads")
    }
    cuTree
  }


  def fillTypeDeclTrees(globalTable:GlobalTable, cuTree: AST[Node]): List[AST[Node]]={
    val noOfRefTypes = globalTable.interfaces.size + globalTable.classNames.size
    val typedeclsNode = cuTree.node.toNodes.head
    val typeDeclsTree = cuTree.add(typedeclsNode,IDGenerator.nextID)

    def loop(node: Node, no: Int, typeDeclsTree: AST[Node]): AST[Node]=no match{
      case 1 =>
        val typeDeclNode = node.toNodes.head.toNodes.head
        typeDeclsTree.add(typeDeclNode,IDGenerator.nextID)
      case x => val typeDeclsTypeDeclNode = node.toNodes(1)
        val typeDeclsTypeDeclTree = typeDeclsTree.add(typeDeclsTypeDeclNode,IDGenerator.nextID)
        val typeDeclsNode = typeDeclsTypeDeclNode.toNodes.head
        val typeDeclsT = typeDeclsTypeDeclTree.add(typeDeclsNode,IDGenerator.nextID)

        loop(typeDeclsNode,x-1,typeDeclsT)
        val typeDeclNode = typeDeclsTypeDeclNode.toNodes(1)
        typeDeclsTypeDeclTree.add(typeDeclNode,IDGenerator.nextID)
    }

    loop(typedeclsNode,noOfRefTypes,typeDeclsTree)
    val typeDeclTrees = FillerUtils.findTreesWithNoChildren(cuTree,"<typedeclaration>")
    typeDeclTrees
  }

  /**
    * Starts the class thread that fills the partial class and generates the method and constructor bodies
    * @param grammarGraph the graph of the grammar
    * @param context the context for generating other things
    * @param cuAST the compilation unit tree
    * @param newSymTab the new SymTab representing the class
    * @return the compilation unit tree filled with the new class
    */
  def addClass(grammarGraph: Graph,context: ((List[SymTab],Int),GlobalTable), cuAST: AST[Node], newSymTab: SymTab): AST[Node] ={
    WeightInitializer.setWeightsToConfiguration(grammarGraph.nodes.toList)
    val typeDeclsTree = cuAST.children.head
    val typeDeclsNode = typeDeclsTree.node
    val typeDeclNode = typeDeclsNode.toNodes.head.toNodes.head
    val typeDeclTree = typeDeclsTree.add(typeDeclNode,IDGenerator.nextID)
    val classThread = new ClassThread((context._2,context._1._1),grammarGraph,clientRpc,newSymTab,typeDeclTree)
    classThread.run()
    cuAST
  }
}
