package progen.grammarparser

class Node (val description: String,val alternative: Boolean, var edges: List[Edge], val terminal: Boolean){
  override def toString: String = {
    val alter = if(alternative) "(alternative)" else "(not alternative)"
    "Node "+description+" "+alter+" has the following edges:\n" + edges.mkString("\n")
  }
  def toNodes: List[Node] ={
    this.edges.map(_.toNode)
  }
}
