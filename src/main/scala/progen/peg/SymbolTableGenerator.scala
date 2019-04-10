package progen.peg

import progen.ConfigurationRetriever
import progen.peg.entities.{Class, GlobalTable}
import progen.symtab.SymTab.SymTabSimple
import grizzled.slf4j.Logging
import progen.prolog.ClientRpc
import progen.symtab.{SymTab, SymTabEntry, SymTabEntryKind}

class SymbolTableGenerator(val configurationRetriever: ConfigurationRetriever,val clientRpc: ClientRpc ) extends Logging{



    def genSymTables(globalTable: GlobalTable): (List[SymTab],Int) ={
      info(".............CLASSES GENERATION STARTED.........")
      val classGenerator = new ClassGenerator(configurationRetriever,clientRpc)
      val classesAndLastOffset = classGenerator.genClasses(globalTable)
      info(".............CLASSES GENERATED.........")

      val symTables = fillClassesIntoSymTables(classesAndLastOffset._1)
      (symTables,classesAndLastOffset._2)
    }


  def fillClassesIntoSymTables(classes: List[Class]):List[SymTab] = {
    val symTabCU = SymTabSimple(new SymTabEntry(SymTabEntryKind.COMPILATIONUNIT,None,None,None,None,None,None,None,None,None),None,None)
    val classSymTabs = classes.map(c => SymTabSimple(new SymTabEntry(SymTabEntryKind.CLASS,Option(c.name),c.superClass,c.interfaces,Option(c.methods),Option(c.constructors),Option(c.fields),None,None,None),Option(symTabCU),None))
    symTabCU.next = Option(classSymTabs)
    classSymTabs
  }

  /**
    * create a new Sym Tab from the new generated Class and update the sym tabs
    * @param c the new added class
    * @param prevSymTabs the previous list of SymTabs
    * @return the new Sym Tab
    */
  def addClassToSymTabs(c: Class, prevSymTabs: List[SymTab]) : SymTab ={
    val symTabCU = prevSymTabs.head.prev match{
      case Some(st) => st
      case None => sys.error("impossible to find the symbol table of compilation unit")
    }
    val newSymTab = SymTabSimple(new SymTabEntry(SymTabEntryKind.CLASS,Option(c.name),c.superClass,c.interfaces,Option(c.methods),Option(c.constructors),Option(c.fields),None,None,None),Option(symTabCU),None)
    symTabCU.next match{
      case Some(ls) => symTabCU.next = Option(newSymTab :: ls)
      case None => symTabCU.next = Option(List(newSymTab))
    }
    newSymTab
  }

}
