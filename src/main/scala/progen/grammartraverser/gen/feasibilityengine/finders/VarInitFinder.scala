package progen.grammartraverser.gen.feasibilityengine.finders
import progen.grammarparser.Node
import progen.grammartraverser.AST
import progen.grammartraverser.fill.FillerUtils
import progen.grammartraverser.utils.{IDGenerator, IdentifierHandler}
import progen.peg.entities.GlobalTable
import progen.prolog.ClientRpc
import progen.symtab.SymTab

class VarInitFinder extends Finder{
  override def findCompletedAST(ast: AST[Node], tab: SymTab, clientRpc: ClientRpc, possibleIds: List[String], context: GlobalTable): Option[AST[Node]] = {
    val localVarDeclTree = ast.findRootTree("<localvariabledeclaration>",ast)
    val typeTree = localVarDeclTree.children.head
    val expected = FillerUtils.findTrees(typeTree,"<identifier>").head.children.head.node.description
    ast.children.clear
    if(typeTree.children.head.node.description == "<reference type> "){
      // from <variableinitializer> to <class instance creation expression> to <classinstancecreationexpression>
      val classInstanceNode = ast.node.toNodes(1).toNodes.head
      val classInstanceTree = ast.add(classInstanceNode,IDGenerator.nextID)
      val finder = FinderFactory.getFinder(classInstanceNode.description)
      finder.findCompletedAST(classInstanceTree,tab,clientRpc,List(expected),context)
    }else{
      // from <variableinitializer> to <constant expression> to <constantexpression>
      val constExprNode = ast.node.toNodes.head.toNodes.head
      val constExprTree = ast.add(constExprNode,IDGenerator.nextID)
      val idNode = constExprNode.toNodes.head
      val idTree = constExprTree.add(idNode,IDGenerator.nextID)
      val newNodeDescr = IdentifierHandler.randomConstantExpression(expected)
      val newNode = new Node(newNodeDescr,false,List(),true)
      val newTree = idTree.add(newNode,IDGenerator.nextID)
      Option(ast)
    }
  }
}
