package progen.prolog


import progen.grammarparser.Node
import progen.grammartraverser.AST
import progen.symtab.SymTab

trait Check[A] {
  def checkWithKB(a: A, clientRpc: ClientRpc,symTab:SymTab): Boolean
}

object Check{
  def apply[A](implicit sh: Check[A]): Check[A] = sh

  object ops{
    def checkWithKB[A:Check](a:A)(clientRpc: ClientRpc,symTab: SymTab): Boolean = Check[A].checkWithKB(a,clientRpc,symTab)

    implicit class CheckOps[A: Check](a:A){
      def checkWithKB(clientRpc: ClientRpc,symTab: SymTab): Boolean = Check[A].checkWithKB(a,clientRpc,symTab)
    }
  }
  implicit val astCanCheck: Check[AST[Node]] =
    (ast,client,symTab) => {
      val response = QueryConstructor.askProlog(ast,client,symTab)
      response
    }
}
