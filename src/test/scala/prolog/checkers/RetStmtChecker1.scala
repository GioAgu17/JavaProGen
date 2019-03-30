package prolog.checkers

import alice.tuprolog.Theory
import TuPrologHandlerTest.engine
import org.eclipse.jdt.core.dom._

class RetStmtChecker1 extends Checker with ParserAST {
  /**
    * Looks into the parsed source code for return statements inside method declarations
    * and asks Prolog KB if the code is correct or not
    * @param source the code to generation.phase2.parse
    * @return true if the source code is correct accordingly to the Prolog KB, false otherwise
    */
  override def parseAndCheckKB(source: String): Boolean = {

    val cu = createParser(source)
    var response = false
    cu.accept(new ASTVisitor() {

      override def visit(node: MethodDeclaration): Boolean ={
        if(!node.isConstructor){
          // adding the node <method declaration> as the application would do
          val th1 = new Theory("node('<methoddeclaration>',3,_,3).")
          engine.addTheory(th1)
          val retType = node.getReturnType2
          val name = node.getName
          val retTypeStr = retType.toString
          val argLst = "['int','int']"
          // adding fact about the method as the application would do
          val th2 = new Theory("method('"+name+"',3,'A','"+retTypeStr+"',"+argLst+").")
          engine.addTheory(th2)

          node.accept(new ASTVisitor() {
            override def visit(node: ReturnStatement): Boolean ={
                  val rt = node.getExpression.resolveTypeBinding
                  val ret = rt.getName
                  // creating the query and asking Prolog engine to solve it
                  val query = "useRetStmt('"+ret+"',3,4)."
                  response = engine.solve(query).isSuccess
              false

            }
          })
        }
        false
        }
    })
    response
  }
}




