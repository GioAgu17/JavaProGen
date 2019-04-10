package progen.grammartraverser.fill

import progen.grammarparser.Node
import progen.grammartraverser.AST
import progen.grammartraverser.utils.IDGenerator
import progen.peg.entities.Interface
import progen.symtab.SymTab

class ClassInfoFiller(val symTab: SymTab) {

  // calls a recursive function on every child of <class declaration>
  def fillTree(tree: AST[Node]): AST[Node] ={
    val toNodes = tree.node.toNodes
    def loop(ls: List[Node],acc: AST[Node]): AST[Node] = ls match{
      case Nil => acc
      case h::t => loop(t,addClassInfo(h,acc))
    }
    loop(toNodes,tree)
  }
  // handles the ast construction depending on the type of node
  def addClassInfo(node: Node, t: AST[Node]): AST[Node] = node.description match{
    case "<identifier>" =>
      val className = symTab.symTabEntry.name match{
        case Some(n) => n
        case None => sys.error("symtab of class doesn't have name")
      }
      val b = t.add(node,IDGenerator.nextID).add(new Node(className,false,List(),true),IDGenerator.nextID)
      t

    case "<super>" =>
      val d = symTab.symTabEntry.superClass match{
        case Some(s) =>
          handleSuper(t.add(node,IDGenerator.nextID),s)
        case None => None
      }
      t
    case "<interfaces>" =>
      val c = symTab.symTabEntry.interfaces match{
        case Some(list) => handleInterfaces(list,t.add(node,IDGenerator.nextID))
        case None => None
      }
      t
    case "<classbody>" =>
      val s = t.add(node,IDGenerator.nextID)
      val b = handleClassBody(s)
      b


    case _ =>
      val r = t.add(node,IDGenerator.nextID)
      t

  }

  def handleClassBody(classBodyTree: AST[Node]): AST[Node]={

    val fields = symTab.symTabEntry.fields match{
      case Some(f) => f
      case None => sys.error("no fields in class")
    }
    val fieldsNotInherited = fields.filter(!_.inherited)
    val mapByType = fieldsNotInherited.groupBy(_.stype)
    val noOfFieldDecls = mapByType.size
    val constructors = symTab.symTabEntry.constructors match{
      case Some(c) => c
      case None => sys.error("no constructors in class")
    }
    val methods = symTab.symTabEntry.methods match{
      case Some(m) => m
      case None => sys.error("no methods in class")
    }
    val noOfCBD = constructors.size +noOfFieldDecls + methods.size
    val f = classBodyTree.node.toNodes.map(n => n.description match{
      case "{" => classBodyTree.add(n,IDGenerator.nextID)
      case "<classbodydeclarations>" => if(noOfCBD == 0) classBodyTree else handleCBDS(n,noOfCBD,classBodyTree)
      case "}" => classBodyTree.add(n,IDGenerator.nextID)
    })
    classBodyTree
  }
  def handleCBDS(cbdsNode: Node, noOfCBDS: Int, classBodyTree: AST[Node]): AST[Node]={
    def loop(node: Node, tree:AST[Node], noOfCBDS: Int): AST[Node] = noOfCBDS match{
      case 1 => {
        // <classbodydeclaration> node
        val classBDCNode = node.toNodes.head.toNodes.head
        tree.add(node,IDGenerator.nextID).add(classBDCNode,IDGenerator.nextID)
      }
      case x => {
        // <class body declarations> <class body declaration> node
        val cbdsCbdNode = node.toNodes(1)
        val cbdsCbdTree = tree.add(node,IDGenerator.nextID).add(cbdsCbdNode,IDGenerator.nextID)
        loop(cbdsCbdNode.toNodes.head,cbdsCbdTree,x-1)
        cbdsCbdTree.add(cbdsCbdNode.toNodes(1),IDGenerator.nextID)
      }
    }
    loop(cbdsNode,classBodyTree,noOfCBDS)
  }
  // handles the <interfaces> node with the information from symbol table and global table
  def handleInterfaces(interfaces: List[Interface], interfTree: AST[Node]): AST[Node]={
    val rand = interfTree.node.toNodes.map(n => n.description match{
      case "implements" => interfTree.add(n,IDGenerator.nextID)
      case "<interfacelist>" =>  handleInterfaceList(n,interfaces.reverse,interfTree)
    })
    interfTree
  }
  // handles the <interface list> node with a recursive function to add all interfaces that a class implements
  def handleInterfaceList(node: Node,interfaces: List[Interface],tree: AST[Node]): AST[Node]={
    def loop(node: Node,no: Int,interfaces: List[Interface],t: AST[Node]): AST[Node] = no match{
      case 1 => FillerUtils.visitUntilDFS("<identifier>",node.toNodes.head,t).add(new Node(interfaces.head.name,false,List(),true),IDGenerator.nextID)
      case x => {
        // <interface list> , <interface type>
        val newNode = node.toNodes(1)
        val newTree = t.add(newNode,IDGenerator.nextID)
        val afterNewTree = newTree.add(newNode.toNodes.head,IDGenerator.nextID)
        val nextTree = loop(newNode.toNodes.head,x-1,interfaces.tail,afterNewTree)

        // node ,
        val nodeComma = newNode.toNodes(1)
        newTree.add(nodeComma,IDGenerator.nextID)
        val nodeInterfaceType = newNode.toNodes(2)
        val interfTypeTree = newTree.add(nodeInterfaceType,IDGenerator.nextID)
        FillerUtils.visitUntilDFS("<identifier>",nodeInterfaceType,interfTypeTree).add(new Node(interfaces.head.name,false,List(),true),IDGenerator.nextID)
      }
    }
    loop(node,interfaces.size,interfaces,tree)
  }
  // handles the <super> node adding the nodes with the name of the class that this class extends, if applicable
  def handleSuper(superTree: AST[Node], superClass: String): AST[Node] ={
    val ret = superTree.node.toNodes.map(n => n.description match{
      case "extends" => superTree.add(n,IDGenerator.nextID)
      case "<classtype>" => FillerUtils.visitUntilDFS("<identifier>",n,superTree).add(new Node(superClass,false,List(),true),IDGenerator.nextID)
    })
    superTree
  }

}

