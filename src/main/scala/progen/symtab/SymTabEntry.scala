package progen.symtab

import progen.peg.entities.{ConstructorSignature, Field, Interface, MethodSignature}
import SymTabEntryKind.SymTabEntryKind

/**
  * class SymTabEntry is an entry of a symbolTable with these
  * @param kind kind of SymTabEntry: see SymTabEntryKind
  * @param name Some(String) if @kind is class, method or constructors, None otherwise
  * @param superClass Some(String) if @kind is CLASS and the class has a superclass
  * @param interfaces Some(List[Interface]) if @kind is CLASS and the class has interfaces
  * @param methods Some(List[MethodSignature]) if @kind is class, None otherwise
  * @param constructors Some(List[ConstructorSignature]) if @kind is class, None otherwise
  * @param fields Some(List[Field]) if @kind is class, None otherwise
  * @param locVars Some(List[(String,String)] if @kind is method or block, None otherwise. It has to be a var because it is the only field which can change during the execution
  * @param retType Some(String) if @kind is method, None otherwise
  * @param params Some(List[String,String]) if @kind is method or constructor, None otherwise
  */
class SymTabEntry(val kind: SymTabEntryKind,val name: Option[String],val superClass: Option[String],val interfaces: Option[List[Interface]],val methods: Option[List[MethodSignature]], val constructors: Option[List[ConstructorSignature]],val fields: Option[List[Field]],var locVars: Option[List[LocalVariable]],val retType: Option[String],val params: Option[List[(String,String)]]) {

}
