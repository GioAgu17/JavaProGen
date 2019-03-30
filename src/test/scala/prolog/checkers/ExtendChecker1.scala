package prolog.checkers

import TuPrologHandlerTest.engine
import org.eclipse.jdt.core.dom._
class ExtendChecker1 extends Checker with ParserAST {
  /**
    * Instantiates an AST parser from the source code given in input,
    * traverses it with the Visitor pattern and ask the Prolog system if
    * the extend rule is satisfied
    * @param source the source code
    * @return true if the Prolog rule is satisfied by the source code, false otherwise
    */
    override def parseAndCheckKB(source: String): Boolean ={

      var response = false
      val cu = createParser(source)
      cu.accept(new ASTVisitor() {
        override def visit(node: TypeDeclaration): Boolean = {
          val typeName = node.getName
          if (!node.isInterface) {
            val superType = node.getSuperclassType
            if (superType != null) {
              val superName = superType.toString
              val query = "canExtend(" + typeName + "," + superName + ")."
              response = engine.solve(query).isSuccess
            }
          }
          false
        }
      })
      response
    }
}
