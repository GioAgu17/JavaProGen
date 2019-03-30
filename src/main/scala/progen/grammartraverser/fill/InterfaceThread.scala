package progen.grammartraverser.fill

import java.io.{BufferedWriter, File, FileWriter}

import progen.grammarparser.Node
import progen.grammartraverser.AST
import progen.grammartraverser.utils.IDGenerator
import progen.peg.entities.{GlobalTable, Interface, MethodSignature}

class InterfaceThread(val interface: Interface, val globalTable: GlobalTable,val typeDeclTree: AST[Node]) extends Runnable {

  override def run():Unit={
    val interfDeclNode = typeDeclTree.node.toNodes(1).toNodes.head
    val interfDeclTree = typeDeclTree.add(interfDeclNode,IDGenerator.nextID)
    val interfFilled = handleInterfaceInfo(interfDeclTree)
    val interfaceDeclTree = interfFilled.findRootTree("<interfacedeclaration>",interfFilled)
    val fileName = interfaceDeclTree.children(1).children.head.node.description
    val file = new File("C:\\Users\\giova\\Documents\\Thesis_Project\\Trials\\src\\main\\java\\generated\\"+fileName+".java")
    val bw  = new BufferedWriter(new FileWriter(file))
    bw.write("package generated;\n"+interfFilled.getInterfRep)
    bw.close()
  }

  def handleInterfaceInfo(interfDeclTree: AST[Node]): AST[Node]={
    val interInfoTree = interfDeclTree.node.toNodes.map(n => handleInterf(n,interfDeclTree))
    interInfoTree.head
  }

  def handleInterf(nodeChild: Node, interfDeclTree: AST[Node]): AST[Node]= nodeChild.description match{
    case "interface" => interfDeclTree.add(nodeChild,IDGenerator.nextID)
    case "<identifier>" => interfDeclTree.add(nodeChild,IDGenerator.nextID).add(new Node(interface.name,false,List(),true),IDGenerator.nextID)
    case "<interfacebody>" => handleInterfaceBody(nodeChild,interfDeclTree)
    case _ => sys.error("something went wrong when handling interface info")
  }

  def handleInterfaceBody(interfBodyNode: Node, interfDeclTree: AST[Node]): AST[Node]={
    val interBodyTree = interfDeclTree.add(interfBodyNode,IDGenerator.nextID)
    val braceNode = interfBodyNode.toNodes.head
    val braceTree = interBodyTree.add(braceNode,IDGenerator.nextID)
    val methodHeadersNode = interfBodyNode.toNodes(1)
    val methodHeadersTree = interBodyTree.add(methodHeadersNode,IDGenerator.nextID)

    def loop(node: Node, no:Int, tree: AST[Node], signatures: List[MethodSignature]): AST[Node]=no match{
      case 1 =>
        val methodHeaderColonNode = node.toNodes.head
        val methodHeaderColonTree = tree.add(methodHeaderColonNode,IDGenerator.nextID)
        val methodHeaderNode = methodHeaderColonNode.toNodes.head
        val methodHeaderTree = methodHeaderColonTree.add(methodHeaderNode,IDGenerator.nextID)
        val methodHeadFilledTree = FillerUtils.handleMethodHeader(methodHeaderTree,signatures.head,globalTable,true)
        val colonNode = methodHeaderColonNode.toNodes(1)
        val colonTree = methodHeaderColonTree.add(colonNode,IDGenerator.nextID)
        colonTree
      case x =>
        val methodHeadersCommaMethodHeaderNode = node.toNodes(1)
        val methodHeadersCommaMethodHeaderTree = tree.add(methodHeadersCommaMethodHeaderNode,IDGenerator.nextID)
        val methodHeadersNode = methodHeadersCommaMethodHeaderNode.toNodes.head
        val methodHeadersTree = methodHeadersCommaMethodHeaderTree.add(methodHeadersNode,IDGenerator.nextID)
        val treeFilled = loop(methodHeadersNode,x-1,methodHeadersTree,signatures.tail)
        val methodHeaderNode = methodHeadersCommaMethodHeaderNode.toNodes(1)
        val methodHeaderTree = methodHeadersCommaMethodHeaderTree.add(methodHeaderNode,IDGenerator.nextID)
        val methodHeadFilledTree = FillerUtils.handleMethodHeader(methodHeaderTree,signatures.head,globalTable,true)
        val colonNode = methodHeadersCommaMethodHeaderNode.toNodes(2)
        val colonTree = methodHeadersCommaMethodHeaderTree.add(colonNode,IDGenerator.nextID)
        colonTree

    }

    val signatures = interface.methods
    val methodHeadersFilledTree = loop(methodHeadersNode,signatures.size,methodHeadersTree,signatures.reverse)
    val braceNode1 = interfBodyNode.toNodes(2)
    val braceTree1 = interBodyTree.add(braceNode1,IDGenerator.nextID)
    methodHeadersFilledTree

  }
}

