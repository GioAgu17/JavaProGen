package prolog.checkers

import alice.tuprolog.Theory
import TuPrologHandlerTest.engine
import org.eclipse.jdt.core.dom._
class MethSignChecker1 extends Checker with ParserAST {

  /**
    * Looks into the AST parser for method declarations and their signatures and
    * asks Prolog KB if the source code is correct
    * @param source the source code to generation.phase2.parse
    * @return true if the source code is correct accordingly to the Prolog KB, false otherwise
    */
  override def parseAndCheckKB(source: String): Boolean = {

    val cu = createParser(source)
    var className = ""
    var res = false

    cu.accept(new ASTVisitor() {
      // taking the class name
      override def visit(node: TypeDeclaration): Boolean = {
        if(!node.isInterface)
          className = node.getName.getIdentifier
        true
      }
      override def visit(node: MethodDeclaration): Boolean = {
        if (!node.isConstructor) {
          val name = node.getName
          val returnType = node.getReturnType2.toString
          val paramTypes = node.parameters.toArray
          val paramTypeNames = paramTypes.map(p =>
            p match {
              case t: SingleVariableDeclaration => t.getType.toString
              case _ => sys.error("something went wrong with parameters in class " + this.getClass.getCanonicalName)
            })
          val paramNames = createParamLst(paramTypeNames)
          val query = "useMethod('" + name + "','" + className + "','" + returnType + "'," + paramNames + ")."
          val response = engine.solve(query).isSuccess
          res = response
          if (response) {
            val theory = new Theory("method('" + name + "',_,'" + className + "','" + returnType + "'," + paramNames + ").")
            engine.addTheory(theory)
            true
          } else
            false
        }else
          false
      }
    })


    res
  }
}
