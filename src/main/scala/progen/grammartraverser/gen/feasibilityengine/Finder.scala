package progen.grammartraverser.gen.feasibilityengine.finders

import progen.grammarparser.Node
import progen.grammartraverser.AST
import progen.peg.entities.GlobalTable
import progen.prolog.ClientRpc
import progen.symtab.SymTab

trait Finder {
  def findCompletedAST(ast:AST[Node],tab: SymTab,clientRpc:ClientRpc,possibleIds:List[String],context:GlobalTable):Option[AST[Node]]
}
