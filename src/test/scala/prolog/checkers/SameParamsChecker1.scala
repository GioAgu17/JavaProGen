package prolog.checkers
import org.eclipse.jdt.core.dom._
import TuPrologHandlerTest.engine

class SameParamsChecker1 extends Checker with ParserAST {
  /**
    * Looks for formal parameters into the source code through the ASTParser and asks
    * the Prolog KB if the code is correct
    * @param source the source code to generation.phase2.parse
    * @return true if the source code is correct accordingly to the Prolog KB, false otherwise
    */
  override def parseAndCheckKB(source: String): Boolean = {
      var response = false
      val cu = createParser(source)
    cu.accept(new ASTVisitor() {
      override def visit(node: MethodDeclaration): Boolean = {
        val params = node.parameters.toArray
        // making the list of parameters a list of names of the parameters
        val namesLst = params.map(p =>
          p match{
            case s: SingleVariableDeclaration => s.getName.getIdentifier
            case _ => sys.error("something went wrong with method parameters in class " + this.getClass.getCanonicalName)
          })
        val paramLst = createParamLst(namesLst)

        val query = "formalParams("+paramLst+")."
        response = engine.solve(query).isSuccess
        false
      }
    })
    response
  }
}
