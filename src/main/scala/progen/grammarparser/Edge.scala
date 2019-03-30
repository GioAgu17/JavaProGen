package progen.grammarparser

class Edge(val fromNode: Node, val toNode: Node, val optional: Boolean,var weight: Double) {
  //val logger = LoggerFactory.getLogger("scala.ingestion.Edge")
  /**
    * Updates the weight simply by using a ratio between 1 and the number of times
    * the edge has been used
    *
    * @param count number of times the edge has been used
    */
  def updateWeight(count: Double): Unit = {
    weight = 1.0 / count
  }

  override def toString: String = {
    fromNode.description + " -> " + toNode.description + "with weight: "+weight
  }
}