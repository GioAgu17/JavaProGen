package prolog

import prolog.checkers.CheckerFactory

trait Check[A] {
  def checkWithKB(a: A, rule: String): Boolean
}

object Check {

  def apply[A](implicit sh: Check[A]): Check[A] = sh

  object ops {
    def checkWithKB[A: Check](a: A)(rule: String): Boolean = Check[A].checkWithKB(a,rule: String)

    implicit class CheckOps[A: Check](a: A) {
      def checkWithKB(rule: String): Boolean = Check[A].checkWithKB(a,rule)
    }
  }



  implicit val stringCanCheck: Check[String] =
    (str,rule) => {
      val checker = CheckerFactory.getChecker(rule)
      checker.parseAndCheckKB(str)
    }

}
