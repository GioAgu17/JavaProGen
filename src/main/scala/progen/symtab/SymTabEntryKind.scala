package progen.symtab

object SymTabEntryKind extends Enumeration{
  type SymTabEntryKind = Value
  val COMPILATIONUNIT: Value = Value("COMPILATIONUNIT")
  val CLASS: Value = Value("CLASS")
  val METHOD: Value = Value("METHOD")
  val CONSTRUCTOR: Value = Value("CONSTRUCTOR")
  val BLOCK : Value = Value("BLOCK")
}
