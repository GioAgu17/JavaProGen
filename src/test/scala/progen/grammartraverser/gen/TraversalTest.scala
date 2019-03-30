package progen.phase2.gen
import progen.grammarparser.Node
import progen.grammartraverser.AST
import progen.grammartraverser.utils.{IDGenerator, WeightedSampler}
import progen.symtab.SymTab


class TraversalTest(val symbolTable: SymTab, val symTabs: List[SymTab]){


    def traverse(ast: Option[AST[Node]]): Option[AST[Node]]= ast match{
      case Some(t) => {
        val node = t.node

        if (node.alternative) {
          // sampling one node from alternatives
          val edges = node.edges
          val newNode = edges(WeightedSampler.weightedSampling(edges.map(_.weight))).toNode

          if (newNode.terminal) {
            val newTree = t.add(newNode,IDGenerator.nextID)
            Option(newTree)
          }
          else {
            val branch = traverse(Option(t.add(newNode,IDGenerator.nextID))) match{
              case Some(v) =>
                // CHECK BRANCH
               // val newTree = v.checkWithSymTab(symbolTable,symTabs)
                val checkKB = true
                if(checkKB) Some(v) else None
              case None => None
            }
            branch
          }
        }
        else {
          val childrenNodes = node.toNodes
          val treesAdded = childrenNodes.map(c =>
            if (c.terminal) {

              val newTree = t.add(c,IDGenerator.nextID)
              Option(newTree)
            }else{
              // here I should put the check if it is optional
              val branch = traverse(Option(t.add(c,IDGenerator.nextID)))
//              branch match{
//                case Some(v) => println(v.getSubTree(v))
//                case None => println("BELLAMERDA")
//
//              }
              // CHECK BRANCH
              val check = true
              if(check) branch else None
            })
          // if the list has some None in the elements
          val c = treesAdded.count(_.isDefined)  == treesAdded.size
          // if the childrenNodes are not empty
          val b = childrenNodes.nonEmpty
          val d = if(b&&c) ast else None
          d
        }
      }
      case None => None
    }



}
