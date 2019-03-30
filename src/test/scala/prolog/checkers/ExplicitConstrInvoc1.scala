package prolog.checkers

import alice.tuprolog.Theory
import org.eclipse.jdt.core.dom._
import scala.util.Random
import TuPrologHandlerTest.engine


class ExplicitConstrInvoc1 extends Checker with ParserAST {

  /**
    * parses the source code through ASTParser and look through Explicit Constructor Invocation, retrieves the arguments
    * from the parser and queries the Prolog KB for validation
    * @param source the source code to be parsed
    * @return true if the Prolog KB validates the source code, false otherwise
    */
  override def parseAndCheckKB(source: String): Boolean = {

    val cu = createParser(source)
    var res = false


    cu.accept(new ASTVisitor() {
      override def visit(node: MethodDeclaration): Boolean = {
        if (node.isConstructor) {
          val constrName = node.getName.getIdentifier
          val paramTypes = node.parameters.toArray
          val paramTypeNames = paramTypes.map(p =>
            p match {
              case t: SingleVariableDeclaration => t.getType.toString
              case _ => sys.error("something went wrong with parameters in class " + this.getClass.getCanonicalName)
            })
          val paramLst = createParamLst(paramTypeNames)
          val query = "canUseConstructor('" + constrName + "'," + paramLst + ")."
          val response = engine.solve(query).isSuccess
          if (response) {
            val (id,parId,depth) = randomTreeParams(new Random)
            val th = new Theory("constructor('" + constrName + "'," + id+ "," + paramLst + ").")
            val th1 = new Theory("node('<constructordeclaration>'," + id+ "," + parId + "," + depth + ").")
            engine.addTheory(th)
            engine.addTheory(th1)

            node.accept(new ASTVisitor() {
              override def visit(node: ConstructorInvocation): Boolean = {
                val args = node.arguments.toArray
                val argNames = args.map(
                  p => p match {
                    case t: Expression => t.resolveTypeBinding.getName
                    case _ => sys.error("something went wrong with arguments of this invocation in class: " + this.getClass.getCanonicalName)
                  }
                )
                val paramLst = createParamLst(argNames)

                val query = "useThisInvocation(" + paramLst + "," + id + "," + (depth+1).toString + ")."
                res = engine.solve(query).isSuccess

                true
              }
            })
          }
        }
        true
      }
    })
    res
  }

}
