package progen.peg

import progen.ConfigurationRetriever
import progen.peg.entities.GlobalTable
import grizzled.slf4j.Logging
import progen.grammartraverser.utils.IDGenerator
import progen.prolog.ClientRpc
import progen.symtab.SymTab

/**
  * The goal of this class is to generate class skeletons and interfaces.
  *
  * @param configurationRetriever
  */
class ContextGenerator(val configurationRetriever: ConfigurationRetriever)extends Logging{

    def firstPhaseGeneration(clientRPC: ClientRpc): ((List[SymTab],Int),GlobalTable) ={
      info(".......FIRST PHASE GENERATION STARTED.....")
      info(".............INTERFACE NAMES GENERATION STARTED")
      // create interface types by retrieving from the configuration file

      def loop(noOfInterf: Int, maxNoOFClass: Int): Int = {
        if(noOfInterf< maxNoOFClass/2){
          val no = configurationRetriever.getInterfaces
          loop(no,maxNoOFClass)
        }else
          noOfInterf
      }
      val maxNoOfClasses = configurationRetriever.maxClasses

      val noOfInterfaces = loop(configurationRetriever.getInterfaces,maxNoOfClasses)

      val prefixInterfaceName = configurationRetriever.getInterfacePrefix
      val interfaceNames = ContextGenerator.genNames(noOfInterfaces,prefixInterfaceName)
      info(".............INTERFACE NAMES GENERATED.........")


      // create class types by retrieving from configuration
      info(".............CLASS NAMES GENERATION STARTED")
      val noOfClasses = configurationRetriever.getClasses
      val prefixClassName = configurationRetriever.getClassPrefix
      val classNames = ContextGenerator.genNames(noOfClasses,prefixClassName)
      info(".............CLASS NAMES GENERATED.........")

      // create interfaces
      info(".............INTERFACES GENERATION STARTED")
      val allowedTypes = configurationRetriever.getAllowedTypes
      val interfaceGenerator =  new InterfaceGenerator(configurationRetriever,clientRPC)
      val interfaces = interfaceGenerator.genInterfaces(interfaceNames,allowedTypes,classNames)
      info(".............INTERFACES GENERATED.........")



      /* create a global table with all the interfaces and the possible types
      usable by method and constructor bodies in the second phase */
      info(".............GLOBAL TABLE GENERATION STARTED")
      val globalTable = new GlobalTable(interfaces,allowedTypes, classNames)
      info(".............GLOBAL TABLE GENERATED.........")

      // generates a list of symbol tables that represent classes

      val symTabGenerator = new SymbolTableGenerator(configurationRetriever,clientRPC)

      info(".............SYMBOL TABLES GENERATION STARTED.........")
      val symTablesAndLastOffSet = symTabGenerator.genSymTables(globalTable)
      info(".............SYMBOL TABLES GENERATED..............")
      (symTablesAndLastOffSet,globalTable)
    }
object ContextGenerator {
  def genNames(n: Int, prefix: String): List[String] = {
    List.tabulate(n)(i => prefix + (i + 1))
  }
}

  /**
    * Adds a new class to the context by creating a new class names, creating a new global table and instantiating a new Class object
    * @param clientRpc client RPC to be passed
    * @param context the previous context that will be updated by this function
    * @return a new context with the added class
    */
  def addClass(clientRpc: ClientRpc, context:((List[SymTab],Int),GlobalTable) ): (((List[SymTab],Int),GlobalTable),SymTab) ={
    val globTab = context._2
    val noOfClasses = globTab.classNames.size
    val classPrefix = configurationRetriever.getClassPrefix
    val newClassName = classPrefix + noOfClasses.toString
    val oldClassNames = globTab.classNames
    val newClassNames = newClassName :: oldClassNames
    val newGlobTable = new GlobalTable(globTab.interfaces,globTab.primitiveTypes,newClassNames)
    val classGenerator = new ClassGenerator(configurationRetriever,clientRpc)
    val classGenerated = classGenerator.genSingleClass(newGlobTable,newClassName)
    val symbolTableGenerator = new SymbolTableGenerator(configurationRetriever,clientRpc)
    val newSymTab = symbolTableGenerator.addClassToSymTabs(classGenerated,context._1._1)
    val newSymTabs = newSymTab :: context._1._1
    val newContext = ((newSymTabs,context._1._2),context._2)
    (newContext,newSymTab)
  }
}
