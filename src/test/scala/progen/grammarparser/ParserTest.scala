package progen.grammarparser

import org.scalatest.FunSuite


class ParserTest extends FunSuite {


  test("removeWhiteSpaces test") {
    val listOfStr1 = ParserGen.removeWhiteSpaces(List(" b e l l e"))
    // first white space removed
    val expectedList1 = List("b e l l e")
    assert(listOfStr1 == expectedList1)

    val listOfStr2 = ParserGen.removeWhiteSpaces(List(" ci so", "be l l","b o o"))
    val expectedList2 = List("ciso","bell","b o o")
    assert(listOfStr2 == expectedList2)

  }
  test("fromRuleToSyms test"){
    val test1 = ParserGen.fromRuleToSyms("class <identifier> <super>? <class body>")
    val expected1 = List("class","<identifier>","<super>?","<classbody>")
    assert(test1 == expected1)

    val test2 = ParserGen.fromRuleToSyms("{ <class body declarations>? }")
    val expected2 = List("{","<classbodydeclarations>?","}")
    assert(test2 == expected2)
  }
  val rule1 = "1.0 ::= <class declaration> ::= class <identifier> <super>? <class body>"
  val rule2 = "1.0 ::= <class body declarations> ::= <class body declaration> | <class body declarations> <class body declaration>"
  val rules = rule1 :: rule2 :: Nil
  val listOfLists =rules.map(_.split("::=").toList) map ParserGen.removeWhiteSpaces

  test("getNonTermNodes test"){

    val nodesTest = ParserGen.getNonTermNodes(listOfLists)

    assert(nodesTest.size ==2)

    val node1 = nodesTest.head
    val node2 = nodesTest(1)
    val description1 = node1.description
    val description2 = node2.description

    assert(description1 == "<classdeclaration>")
    assert(description2 == "<classbodydeclarations>")




    val alternative1 = node1.alternative
    val alternative2 = node2.alternative

    assert( ! alternative1)
    assert(alternative2)

    val edges1 = node1.edges
    val edges2 = node2.edges

    assert(edges1.isEmpty)
    assert(edges2.isEmpty)

    val terminal1 = node1.terminal
    val terminal2 = node2.terminal

    assert( ! terminal1)
    assert( ! terminal2)


  }
  // using the same rules as above
  test("getRule test"){
    val nonTermNodes = ParserGen.getNonTermNodes(listOfLists)
    val test1 = ParserGen.getRule(listOfLists,nonTermNodes,false)
    val test2 = ParserGen.getRule(listOfLists,nonTermNodes,true)

    val nodes1 = test1.map(_._1)
    val rightRules1 = test1.map(_._2)

    val nodes2 = test2.map(_._1)
    val rightRules2 = test2.map(_._2)

    assert(nodes1.size == 1)
    assert(nodes2.size == 1)

    val node1 = nodes1.head
    val node2 = nodes2.head

    val description1 = node1.description
    val description2 = node2.description

    assert(description1 == "<classdeclaration>")
    assert(description2 == "<classbodydeclarations>")


    val alternative1 = node1.alternative
    val alternative2 = node2.alternative

    assert( ! alternative1)
    assert(alternative2)

    val edges1 = node1.edges
    val edges2 = node2.edges

    assert(edges1.isEmpty)
    assert(edges2.isEmpty)

    val terminal1 = node1.terminal
    val terminal2 = node2.terminal

    assert( ! terminal1)
    assert( ! terminal2)

    val rightRule1 = rightRules1.head
    val rightRule2 = rightRules2.head

    val expectedLst1 = List("class","<identifier>","<super>?","<classbody>")
    val expectedLst2 = List("<class body declaration> "," <class body declarations> <class body declaration>")

    assert(rightRule1 == expectedLst1)
    assert(rightRule2 == expectedLst2)

  }
  test("findNode test"){
    val node1 = new Node("<classbody>",false,List(),false)
    val node2 = new Node("<classbodydeclarations>",true,List(),false)
    val node3 = new Node("<constructordeclaration>",false,List(),false)

    val nodes = node1 :: node2 :: node3 :: Nil

    val string1 = "<classbody>"
    val string2 = "<classbodydeclarations>"
    val string3 = "<constructordeclaration>"
    val string4 = "{"

    val test1 = ParserGen.findNode(string1,nodes)
    val test2 = ParserGen.findNode(string2, nodes)
    val test3 = ParserGen.findNode(string3, nodes)
    val test4 = ParserGen.findNode(string4,nodes)

    assert(test1 == node1)
    assert(test2 == node2)
    assert(test3 == node3)

    assert(test4.description == "{")
    assert(! test4.alternative)
    assert(test4.edges.isEmpty)
    assert(test4.terminal)

  }

  test("createNonAlternEdges test"){
    val nonTermNodes = ParserGen.getNonTermNodes(listOfLists)
    val nodeAndRightRules = ParserGen.getRule(listOfLists,nonTermNodes,false)
    // this ruleTest1 is "<class declaration> ::= class <identifier> <super>? <class body>"
    val ruleTest1 = nodeAndRightRules.head
    val nonAlternEdges = ParserGen.createNonAlternEdges(ruleTest1._1,ruleTest1._2,nonTermNodes)
    // let's take all the destination nodes of each edge
    val allDestNodes = nonAlternEdges.map(_.toNode)
    val allDestNodeDescription = allDestNodes.map(_.description)
    val expectedDescr = List("class","<identifier>","<super>","<classbody>")
    assert(allDestNodeDescription == expectedDescr)

    val allDestNodeAltern = allDestNodes.map(_.alternative)
    // this is because they are all instantiated
    val expectedAltern = List(false,false,false,false)
    assert(allDestNodeAltern == expectedAltern)
  }

  test("createFictitiousNodes test"){
    val nonTermNodes = ParserGen.getNonTermNodes(listOfLists)
    val rightAlternRule = ParserGen.getRule(listOfLists,nonTermNodes,true).map(_._2).head
    val fictitiousNodes = ParserGen.createFictitiousNodes(rightAlternRule)
    assert(fictitiousNodes.size == 2)
    val fictitiousNodeDescr = fictitiousNodes.map(_.description)
    val expectedDescr = List("<class body declaration> "," <class body declarations> <class body declaration>")
    assert(fictitiousNodeDescr == expectedDescr)

  }

  test("createAlternEdges test"){
    val nonTermNodes = ParserGen.getNonTermNodes(listOfLists)
    val rightAlternRule = ParserGen.getRule(listOfLists,nonTermNodes,true).head
    val fictitiousNodes = ParserGen.createFictitiousNodes(rightAlternRule._2)
    val alternEdges = ParserGen.createAlternEdges(rightAlternRule._1,fictitiousNodes)
    // testing that all the edges have the same fromNode
    val fromNodes = alternEdges.map(_.fromNode).toSet
    assert(fromNodes.size == 1)
    assert(fromNodes.head.alternative)
    val allDestNodes = alternEdges.map(_.toNode)
    val allDestDescr = allDestNodes.map(_.description)
    val expectedDescr = List("<class body declaration> "," <class body declarations> <class body declaration>")
    assert(allDestDescr == expectedDescr)

    val allDestOpt = alternEdges.map(_.optional)
    val expectedOpt = List(false,false)
    assert(allDestOpt == expectedOpt)

  }
  test("buildGraph test"){
    val rule1 = "1.0 ::=<compilation unit> ::= <type declarations>"
    val rule2 = "1.0 ::=<type declarations> ::= <type declaration> | <type declarations> <type declaration>"
    val rule3 = "1.0 ::=<type declaration> ::= <class declaration>"
    val rule4 = "1.0 ::=<class declaration> ::= class <identifier> <super>? <class body>"
    val rule5 = "1.0 ::=<super> ::= extends <class type>"

    val rules = List(rule1,rule2,rule3,rule4,rule5)
    val graph = ParserGen.buildGraph(rules)
    println(graph)
    assert(graph.edges.size == 13)
    assert(graph.nodes.size == 12)
  }
}

