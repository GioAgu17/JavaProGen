package prolog.checkers

import alice.tuprolog.Theory
import org.eclipse.jdt.core.dom._
import TuPrologHandlerTest.engine

class ExtendChecker2 extends Checker with ParserAST {
  /**
    * Looks into the source code for a class and if the class extends another one, asks Prolog KB
    * if it is correct
    * @param source the source code to be parsed
    * @return true if the code is correct accordingly to the Prolog KB, false otherwise
    */
  override def parseAndCheckKB(source: String): Boolean = {

    var response = false
    val cu = createParser(source)
    cu.accept(new ASTVisitor() {
      override def visit(node: TypeDeclaration): Boolean = {
        val typeName = node.getName
        if (!node.isInterface) {
          val superType = node.getSuperclassType
          if (superType != null) {
            val superName = superType.toString
            val query = "canExtend('" + typeName + "','" + superName + "')."
            response = engine.solve(query).isSuccess
            if(response){
              val th1 = new Theory("extends('"+typeName+"','"+ superName + "').")
              engine.addTheory(th1)
            }
          }
        }
        if(response)
          true
        else false
      }
    })
    response
  }
}
