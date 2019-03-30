package progen.grammarparser

class Graph (val nodes: Set[Node], val edges: Set[Edge]) {
  // here goes the traversing algorithm!
  override def toString: String = {
    nodes.mkString("\n")
  }



}