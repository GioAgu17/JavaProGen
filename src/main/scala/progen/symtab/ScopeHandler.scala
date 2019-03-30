package progen.symtab

import java.io.{File, PrintWriter}

import progen.grammarparser.Node
import SymTab.SymTabSimple
import grizzled.slf4j.Logging

object ScopeHandler  {
  var currentScope: SymTab = SymTabSimple(new SymTabEntry(SymTabEntryKind.COMPILATIONUNIT, None, None, None, None, None, None, None, None, None), None, None)
  def enterScope(node: Node,st:SymTab): Unit = {

    if (node.description == "<block>") {
      val symTabEntry = new SymTabEntry(SymTabEntryKind.BLOCK, None, None, None, None, None, None, None, None, None)
      val oldScope = currentScope
      currentScope = SymTabSimple(symTabEntry, Option(oldScope), None)
      oldScope.next match {
        case Some(l) => oldScope.next = Option(currentScope :: l)
        case None => oldScope.next = Option(List(currentScope))
      }
    }else{
      currentScope = st
    }
  }
  def leaveScope(node: Node,st: SymTab): Unit ={
    if(node.description == "<block>") {
      val oldScope = currentScope
      currentScope = oldScope.prev match {
        case Some(t) => t
        case None => sys.error("cannot leave scope because this is the outermost scope")
      }
    } else{
      currentScope = st
    }
  }
}