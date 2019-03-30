package progen.grammartraverser.gen.feasibilityengine.finders

object FinderFactory {
  def getFinder(nodeDescription:String): Finder = nodeDescription match{
    case "<returnstatement>" =>
      new ReturnStmtFinder
    case "<methodinvocation>" =>
      new MethInvFinder
    case "<classinstancecreationexpression>" =>
      new ClassInstanceFinder
    case "this ( <argument list>? ) ; " =>
      new ThisOrSuperInvocationFinder(true)
    case " super ( <argument list>? ) ;" =>
      new ThisOrSuperInvocationFinder(false)
    case "<variableinitializer>" =>
      new VarInitFinder
    case _ =>
      sys.error("FinderFactory applied to a wrong node")
  }
}
