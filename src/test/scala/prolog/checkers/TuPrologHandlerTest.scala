package prolog.checkers

import alice.tuprolog.{Prolog, Theory}

import scala.io.Source

object TuPrologHandlerTest {
  // 1 open the file and read it
  val bufferedSource = Source.fromFile("src/main/resources/jls.txt")
  val fileContents = bufferedSource.getLines().mkString

  // 2 close the file
  bufferedSource.close()

  // 3 start a new Prolog engine
  val engine = new Prolog()

  // 4 put the knowledge base into the Prolog engine as a theory
  val theory = new Theory(fileContents)
  engine.setTheory(theory)

  def addTheory(s: String) ={
    val th = new Theory(s)
    engine.addTheory(th)
  }

  def solveQuery(s: String):Boolean = {
    engine.solve(s).isSuccess
  }

}
