package prolog.checkers
import org.eclipse.jdt.core.dom._
trait ParserAST {
  /**
    * Creates a ASTParser from the source code
    * @param source the source code
    * @return a compilation unit to be visited from the ASTVisitor
    */
  def createParser(source: String): CompilationUnit={
    val parser = ASTParser.newParser(AST.JLS10)
    parser.setSource(source.toCharArray)
    parser.setUnitName("test.Java")
    parser.setEnvironment(null,null,null,true)

    parser.setResolveBindings(true)
    parser.setKind(ASTParser.K_COMPILATION_UNIT)
    parser.setBindingsRecovery(true)
    val cu = parser.createAST(null).asInstanceOf[CompilationUnit]
    cu
  }
}
