package progen.grammarparser


object ParserGen {

  def buildGraph(listOfRules: List[String]): Graph = {
    // each line of the grammar file becomes a list of three elements: the weight of the rule, the left part and the right part
    val listOfLists =listOfRules.map(_.split("::=").toList) map removeWhiteSpaces
    // list of non terminal nodes
    val listOfNonTerminalNodes = getNonTermNodes(listOfLists)
    // list of all the rules without alternatives
    val notAlternativeRules = getRule(listOfLists,listOfNonTerminalNodes,false)
    // creates all the edges for every non alternative node
    val notAlternativeEdges = notAlternativeRules.flatMap(elem => createNonAlternEdges(elem._1,elem._2,listOfNonTerminalNodes))

    // for alternative rules takes the alternatives and make them nodes. Every element of the first list is a tuple made by
    // the node on the left part and the list of alternatives on the right part
    val alternativeChoices = getRule(listOfLists,listOfNonTerminalNodes,true)
    // this list contains a tuple with a node and a list of nodes which have to be related together
    val alternativeRules = alternativeChoices.map(_._1) zip alternativeChoices.map(elem => createFictitiousNodes(elem._2))
    // creates the edges for the alternative rules
    val alternativeEdges = alternativeRules.flatMap(elem => createAlternEdges(elem._1,elem._2))
    // last thing is to create new edges for the fictitious nodes!
    // we need to take the description of these nodes and handle them as they are rules!
    val fictitiousNodes = alternativeRules.flatMap(elem => elem._2)
    val fictitiousRules = fictitiousNodes.map(fn => fromRuleToSyms(fn.description))
    val fictitiousNodesAndRules = fictitiousNodes zip fictitiousRules
    val edgesForFictitiousNodes = fictitiousNodesAndRules.flatMap(elem => createNonAlternEdges(elem._1,elem._2,listOfNonTerminalNodes))

    val allEdges = notAlternativeEdges ++ alternativeEdges ++ edgesForFictitiousNodes
    val toNodes = allEdges.map(e => e.toNode).toSet
    val fromNodes = allEdges.map(e => e.fromNode).toSet
    val allNodes = fromNodes ++ toNodes
    val graph = new Graph(allNodes,allEdges.toSet)

    graph

  }
  def createAlternEdges(fromNode: Node, toNodes: List[Node]): List[Edge] ={
    val edges = toNodes.map(tn => new Edge(fromNode,tn,false,1.0))
    fromNode.edges = edges
    edges
  }
  def createFictitiousNodes(alternatives: List[String]): List[Node] ={
    alternatives.map( al =>
      new Node(al,false,List[Edge](),false))
  }

  /**
    * Takes the complete rules, each one splitted by "::=" ,and reurns a list of all the non terminal nodes of the grammar
    * @param listOfLists list with every element representing a rule, which is in turn a list of three elements: the salience value,
    *                    the left part and the right part.
    * @return a list of Node objects representing all the non terminal nodes of the grammar
    */
  def getNonTermNodes(listOfLists: List[List[String]]): List[Node] = {
    // generation.phase2.parse the left part only
    val nonTerminalSymbols = listOfLists.map(_.head).map(_.replaceAll("\\s+(?=[^<>]*>)",""))

    val alternativeFlagForNonTermSymbols = listOfLists.map(_(1).contains("|"))
    // for each rule take the left part, the weight and the alternative flag together and make a list of tuple 3
    val nonTermSymsAndFlag = nonTerminalSymbols zip alternativeFlagForNonTermSymbols
    // instantiates the nodes of the graph using the three elements for each tuple of every rule
    val listOfNonTerminalNodes = nonTermSymsAndFlag.map(a => new Node(a._1,a._2,List[Edge](),false))
    listOfNonTerminalNodes
  }

  /**
    * Takes the completed rules and all the non terminal nodes for returning the alternative rules or the non alternative rules
    * depending on the field alternative
    * @param listOfLists every element being a list with the three elements of each rule (salience, right part, left part)
    * @param listOfNonTerminalNodes the list of all non terminal nodes
    * @param alternative sets the type of rules wanted
    * @return either the alternative rules or the non alternative ones.
    */
  def getRule(listOfLists: List[List[String]], listOfNonTerminalNodes: List[Node], alternative: Boolean): List[(Node,List[String])] ={
    val listOfRightPartRules = listOfLists.map(_(1))
    if(alternative){
      val alternativeNonTermNodes = listOfNonTerminalNodes.filter(_.alternative)
      val alternativeRightPartRules = listOfRightPartRules.filter(_.contains("|"))
      val rightPartAlternatives = alternativeRightPartRules.map(_.split("\\|").toList)
      val nodesAndRightPart = alternativeNonTermNodes zip rightPartAlternatives
      nodesAndRightPart
    }else{
      val nonAlternativeNonTermNodes = listOfNonTerminalNodes.filter(!_.alternative)
      val notAlternativeRightPartRules = listOfRightPartRules.filter(!_.contains("|"))
      val listOfListsNonAlternRight = notAlternativeRightPartRules.map(fromRuleToSyms)
      nonAlternativeNonTermNodes zip listOfListsNonAlternRight
    }

  }

  /**
    * takes the node on the left side, the list of symbols on the right and all the non terminal nodes
    * and returns the edges of the node
    * @param n the right symbol node
    * @param rightNodes list of right nodes
    * @param nodes list of all the non terminal nodes
    * @return edges for the node n
    */
  def createNonAlternEdges(n: Node, rightNodes: List[String], nodes: List[Node]): List[Edge] ={
    val opt = rightNodes.map(_.contains("?"))
    val allNodes = rightNodes.map(s => findNode(s.replaceAll("\\?",""),nodes))
    val nodesAndOpt = allNodes zip opt
    val edges = nodesAndOpt.map(v => new Edge(n,v._1,v._2,1.0))
    n.edges = edges
    edges
  }

  /**
    * Takes a string which represent the right part of the rule and it splits it by all whitespaces
    * in the string except the one between the non terminal delimiters "<" and ">"
    * @param s the right part of the rule
    * @return the list of strings representing the terminal and non terminal symbols
    */
  def fromRuleToSyms(s: String): List[String] ={
    // removing whitespaces in terminal description
    val whiteSpacesInDelimiters = "\\s+(?=[^<>]*>)"
    val str = s.replaceAll(whiteSpacesInDelimiters,"").trim
    str.split(" ").toList
  }

  /**
    * Given a string description of a symbol, find if there is an existing node that can be associated,
    * otherwise it creates a new node for that description
    * @param s description of the symbol
    * @param nodes all the non terminal nodes
    * @return a Node either already existing or a new one
    */
  def findNode(s: String, nodes: List[Node]): Node ={
    nodes.find(p => p.description == s) match{
      // non terminal nodes
      case Some(n) => n
      // terminal nodes
      case None =>
        new Node(s,false, List[Edge](),true)
    }
  }

  /**
    * takes a list and remove all spaces in all strings except the last one which is trim
    * if there is only one string it is treated like it is the last one
    * @param l the list of strings
    * @return the same list of strings modified with the white spaces replaced
    */
  def removeWhiteSpaces(l: List[String]): List[String] = l match {
    case List() => Nil
    case h :: Nil => h.trim() :: Nil
    case h :: t => h.replaceAll("\\s+","") :: removeWhiteSpaces(t)
  }

}
