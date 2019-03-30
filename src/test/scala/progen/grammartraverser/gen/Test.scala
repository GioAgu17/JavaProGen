package progen.phase2.gen

import progen.grammarparser.Node
import progen.peg.entities.MethodSignature
import progen.symtab.SymTab.SymTabSimple
import grizzled.slf4j.Logging
import org.scalatest.FunSuite
import progen.grammartraverser.AST
import progen.grammartraverser.fill.FillerTest
import progen.symtab.{SymTab, SymTabEntry, SymTabEntryKind}


class Test extends FunSuite with Logging{

  test("traverse function"){
    val fillerTest = new FillerTest

    val context = (fillerTest.globalTable,fillerTest.symbolTables)
    val interfaces = fillerTest.fillInterfaces
    val classTreeAndSymTabsTest: List[(AST[Node],SymTab)] = fillerTest.fillClasses



  }


}

object Test {
  def createMethSymTab(signature: MethodSignature, classSymTab: SymTab): SymTab = {
    val kind = SymTabEntryKind.METHOD
    val name = Option(signature.name)
    val methods = None
    val constructors = None
    val fields = None
    var locVars = None
    val retType = Option(signature.returnType)
    val params = Option(signature.formalParameters)
    val symTabEntry = new SymTabEntry(kind, name,None,None ,methods, constructors, fields, locVars, retType, params)
    SymTabSimple(symTabEntry, Option(classSymTab),None)
  }
}
