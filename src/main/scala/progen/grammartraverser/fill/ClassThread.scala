package progen.grammartraverser.fill

import java.io.{BufferedWriter, File, FileWriter}

import progen.grammarparser.{Graph, Node}
import progen.peg.entities.{ConstructorSignature, GlobalTable, MethodSignature}
import grizzled.slf4j.Logging
import progen.grammartraverser.AST
import progen.grammartraverser.gen.BodyGenerator
import progen.grammartraverser.utils.{GlobalVariables, IDGenerator}
import progen.prolog.ClientRpc
import progen.symtab.SymTab.SymTabSimple
import progen.symtab.{SymTab, SymTabEntry, SymTabEntryKind}


class ClassThread (val context: (GlobalTable,List[SymTab]), val grammar: Graph, val clientRpc: ClientRpc, val classSymTab: SymTab, val treeRoot: AST[Node]) extends Runnable with Logging {
  override def run(): Unit = {

    val classDeclNode = treeRoot.node.toNodes.head.toNodes.head
    val classDeclTree = treeRoot.add(classDeclNode,IDGenerator.nextID)
    val classFiller = new ClassInfoFiller(classSymTab)
    val classFilledNode = classFiller.fillTree(classDeclTree)
    val className = classSymTab.symTabEntry.name match{case Some(c) => c case None => ""}
    println("CLASS INFORMATION ADDED FOR CLASS: " + className )
    info("CLASS INFORMATION ADDED FOR CLASS: " + className)
    val constructorFiller = new ConstructorFiller(classSymTab, context._1)
    val classDeclsTree = constructorFiller.fillConstructor(classFilledNode)
    println("CONSTRUCTOR SIGNATURES FILLED IN TREE FOR CLASS " + className)
    info("CONSTRUCTOR SIGNATURES FILLED IN TREE FOR CLASS " + className)
    val classMemberFiller = new ClassMemberFiller(classSymTab, context._1)
    val classMembersFilled = classMemberFiller.fillMembers(classFilledNode)
    println("FIELDS AND METHOD SIGNATURES FILLED IN TREE FOR CLASS: " + className)
    info("FIELDS AND METHOD SIGNATURES FILLED IN TREE FOR CLASS: " + className)

    val classDeclaration = classMembersFilled.findRootTree("<classdeclaration>", classMembersFilled)



      val methodBodyTrees: List[AST[Node]] = FillerUtils.findTreesWithNoChildren(classDeclaration, "<methodbody>")
      val methodSignatures: List[MethodSignature] = classSymTab.symTabEntry.methods match{
        case Some(ms) => ms
        case None => sys.error("method signatures not found in sym tab class")
      }
      val methodSignaturesAndClassSymTab: List[(MethodSignature, SymTab)] = methodSignatures.map(m => (m, classSymTab))
      val methodSymTabs: List[SymTab] = methodSignaturesAndClassSymTab.map(mt => createMethSymTab(mt._1, mt._2))
      val mbTreesAndSymTabs: List[(AST[Node], SymTab)] = methodBodyTrees zip methodSymTabs
//
//      val mbTreeAndSymTabTest = Random.shuffle( mbTreesAndSymTabs).head
//      val testFilled = new BodyGenerator(context,grammar,clientRpc).traverse(mbTreeAndSymTabTest._1,mbTreeAndSymTabTest._2)
     val mbTreesFilled: List[AST[Node]] = mbTreesAndSymTabs.map(mt => new BodyGenerator(context, grammar,clientRpc).traverse(mt._1,mt._2))

      println("METHOD BODY TREES FILLED IN TREE FOR CLASS :" + className)
      val constrBodyTrees: List[AST[Node]] = FillerUtils.findTreesWithNoChildren(classDeclaration, "<constructorbody>")

    val constructors = classSymTab.symTabEntry.constructors match{
      case Some(c) => c
      case None => sys.error("no constructors found in class")
    }
      val constrSignsAndClassSymTabs: List[(ConstructorSignature, SymTab)] = constructors.map(c => (c, classSymTab))
      val constrSymTabs: List[SymTab] = constrSignsAndClassSymTabs.map(ct => createConstrSymTab(ct._1, ct._2))
      val constrTreesAndSymTabs: List[(AST[Node], SymTab)] = constrBodyTrees zip constrSymTabs
      val cbTreesFilled = constrTreesAndSymTabs.map(ct => new BodyGenerator(context, grammar,clientRpc).traverse(ct._1,ct._2))
      println("CONSTRUCTOR BODY TREES FILLED IN TREE FOR CLASS: " + className)

    val fileName = classDeclaration.children(1).children.head.node.description
    val file = new File("C:\\Users\\giova\\Documents\\Thesis_Project\\Trials\\src\\main\\java\\generated\\"+fileName+".java")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write("package generated;\n"+classMembersFilled.getClassRep)
    bw.close()
    println("Lines of code generated: "+GlobalVariables.LOC)


  }
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
    val ret = SymTabSimple(symTabEntry, Option(classSymTab),None)
    classSymTab.next match{
      case Some(st) => classSymTab.next = Option(ret :: st)
      case None => classSymTab.next = Option(List(ret))
    }
    ret

  }
  def createConstrSymTab(signature: ConstructorSignature, tab: SymTab): SymTab ={
    val kind  = SymTabEntryKind.CONSTRUCTOR
    val name = Option(signature.className)
    val methods = None
    val constructors = None
    val fields = None
    var locVars = None
    val retType = None
    val params = Option(signature.formalParameters)
    val symTabEntry = new SymTabEntry(kind,name,None,None,methods,constructors,fields,locVars,retType,params)
    val ret = SymTabSimple(symTabEntry,Option(tab),None)
    tab.next match{
      case Some(st) => tab.next = Option(ret :: st)
      case None => tab.next = Option(List(ret))
    }
    ret
  }
}