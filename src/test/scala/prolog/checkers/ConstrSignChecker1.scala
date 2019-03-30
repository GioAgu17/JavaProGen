package prolog.checkers

import alice.tuprolog.Theory
import org.eclipse.jdt.core.dom._
import prolog.checkers.TuPrologHandlerTest.engine

/**
  * Checks the signatures of the constructors enforcing JLS rule 8.8.2
  */
class ConstrSignChecker1 extends Checker with ParserAST {
  /**
    * overridden method from Checker trait: looks into constructor declarations and retrieves the formal parameter types
    * for querying the Prolog KB
    * @param source the source code to check
    * @return true if the Prolog KB validates the source, false otherwise
    */
  override def parseAndCheckKB(source: String): Boolean = {
    val cu = createParser(source)
    var response = false
    cu.accept(new ASTVisitor() {
      override def visit(node: TypeDeclaration): Boolean ={
        if(!node.isInterface){
          val name = node.getName.getIdentifier
          node.accept(new ASTVisitor() {
            override def visit(node: MethodDeclaration): Boolean ={
              if(node.isConstructor){
                val paramTypes = node.parameters.toArray
                val paramTypeNames = paramTypes.map(p =>
                  p match {
                    case t: SingleVariableDeclaration => t.getType.toString
                    case _ => sys.error("something went wrong with parameters in class " + this.getClass.getCanonicalName)
                  })
                val paramLst = createParamLst(paramTypeNames)
                val query = "canUseConstructor('"+name+"',"+paramLst+")."
                response = engine.solve(query).isSuccess
                if(response){
                  val th = new Theory("constructor('"+name+"',_,"+paramLst+").")
                  engine.addTheory(th)
                }
              }
              true
            }
          })
        }
        true
      }
    })
    response
  }
}
