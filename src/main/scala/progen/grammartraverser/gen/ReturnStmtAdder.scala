package progen.grammartraverser.gen

import progen.grammarparser.Node
import progen.grammartraverser.AST
import progen.grammartraverser.utils.{IDGenerator, IdentifierHandler, WeightInitializer}
import progen.peg.entities.{ConstructorSignature, GlobalTable, MethodSignature}
import progen.symtab.{SymTab, TypeHandler}

object ReturnStmtAdder {
  /**
    * Function that adds a new statement for putting a return statement at the end of the body
    * of a method. First new block statements are created and then the weights of the grammar are
    * changed such that the only possible traversal arrives to the <returnstatement> node. The tree is filled
    * and finally the wights are reset to previous configuration
    * @param blockTree the tree with the <block> node
    * @param retType the return type to add to the <returnstatement> node
    */
  def addRetStmt(bodyTree: AST[Node],retType: String,symTab:SymTab,bodyGenerator: BodyGenerator):Unit ={
    val context = bodyGenerator.context
    val grammarGraph = bodyGenerator.grammarGraph

    if(!enoughResources(retType,symTab,context._1)) {
      val possibleTypes: List[String] = TypeHandler.allPossibleTypes(symTab)
      val methodParamsAvailable : List[List[String]] = methodsWithRetType(retType,symTab).map(m => m.formalParameters.map(_._1))
      val constructorParamsAvailable: List[List[String]] = constructorsWithRetType(retType,symTab).map(m => m.formalParameters.map(_._1))
      val parameters = methodParamsAvailable ++ constructorParamsAvailable
      val parametersAndNumberOfReqTypes = parameters.map(p => (p, p.size - possibleTypes.intersect(p).size))
      val typesToCreate = parametersAndNumberOfReqTypes.minBy(_._2)._1.filter(t => !possibleTypes.contains(t) && !context._1.primitiveTypes.contains(t))
      addLocalVariableDeclarations(bodyTree,typesToCreate,symTab,bodyGenerator)
    }
    val blockStmtTree  = createNewBlockStatement(bodyTree)
    WeightInitializer.updateWeightsForRetStmt(grammarGraph.nodes.toList)
    WeightInitializer.returnConfigOn = true
    bodyGenerator.traverseBody(Option(blockStmtTree),List(),symTab)
    WeightInitializer.returnConfigOn = false
    WeightInitializer.setWeightsToConfiguration(grammarGraph.nodes.toList)
  }

  def enoughResources(retType: String, tab: SymTab,globalTable:GlobalTable): Boolean ={
    if(!globalTable.primitiveTypes.contains(retType)){
      val allPossibleTypes = TypeHandler.allPossibleTypes(tab)
      val methodsAvailable = methodsWithRetType(retType,tab)

      val constructorsAvailable = constructorsWithRetType(retType,tab)
      if(constructorsAvailable.exists(c => c.formalParameters.isEmpty))
        true
      else {
        val canImplementMethodInvocations = methodsAvailable.exists(m => m.formalParameters.map(_._1).forall(allPossibleTypes.contains))
        val canImplementClassInstance = constructorsAvailable.exists(c => c.formalParameters.map(_._1).forall(allPossibleTypes.contains))
        canImplementMethodInvocations || canImplementClassInstance
      }
    }else
      true
  }
  def methodsWithRetType(retType: String,tab: SymTab): List[MethodSignature] ={
    val methName = tab.symTabEntry.name match{
      case Some(n) => n
      case None => sys.error("cannot find name of method in sym tab")
    }
    val methodsAvailable = tab.lookUpMethods match{
      case Some(ms) => ms.filter(m => m.name != methName && m.returnType == retType)
      case None => sys.error("cannot find methods in sym tab")
    }
    methodsAvailable
  }

  def constructorsWithRetType(retType:String,tab: SymTab): List[ConstructorSignature] ={
    tab.lookUpConstructors(retType) match{
      case Some(cs) => cs
      case None => List()
    }
  }
  def addLocalVariableDeclarations(bodyTree: AST[Node],typesToCreate: List[String],tab: SymTab, bodyGenerator: BodyGenerator): AST[Node] ={
    val typesDistinct = typesToCreate.distinct
    typesDistinct.foreach(t => addLocalVariableDecl(bodyTree,t,tab,bodyGenerator))
    bodyTree
  }
  def addLocalVariableDecl(bodyTree: AST[Node], typeToCreate: String, tab: SymTab,bodyGenerator: BodyGenerator): AST[Node] ={
    val newBlockStmt = createNewBlockStatement(bodyTree)
    val locVarDeclStmtNode = newBlockStmt.node.toNodes.head
    val locVarDeclStmtTree = newBlockStmt.add(locVarDeclStmtNode,IDGenerator.nextID)
    val locVarDeclStmtN = locVarDeclStmtTree.node.toNodes.head
    val locVarDeclStmtTreeN = locVarDeclStmtTree.add(locVarDeclStmtN,IDGenerator.nextID)
    val locVarDeclNode = locVarDeclStmtN.toNodes.head
    val locVarDeclTree = locVarDeclStmtTreeN.add(locVarDeclNode,IDGenerator.nextID)
    val semiColonNode = locVarDeclStmtN.toNodes(1)
    val semiColonTree = locVarDeclStmtTreeN.add(semiColonNode,IDGenerator.nextID)
    val typeNode = locVarDeclNode.toNodes.head
    val typeTree = locVarDeclTree.add(typeNode,IDGenerator.nextID)
    val refTypeNode = typeNode.toNodes.head
    val refTypeTree = typeTree.add(refTypeNode,IDGenerator.nextID)
    val rfTpNode = refTypeNode.toNodes.head
    val rfTpTree = refTypeTree.add(rfTpNode,IDGenerator.nextID)
    val classTypeNode = rfTpNode.toNodes.head
    val classTypeTree = rfTpTree.add(classTypeNode,IDGenerator.nextID)
    val clTpNode = classTypeNode.toNodes.head
    val clTpTree = classTypeTree.add(clTpNode,IDGenerator.nextID)
    val typeNameNode = clTpNode.toNodes.head
    val typeNameTree = clTpTree.add(typeNameNode,IDGenerator.nextID)
    val idNode = typeNameNode.toNodes.head
    val idTree = typeNameTree.add(idNode,IDGenerator.nextID)
    val newNode = new Node(typeToCreate,false,List(),true)
    val newTree = idTree.add(newNode,IDGenerator.nextID)
    TypeHandler.nodeType = typeToCreate
    val varDeclsNode = locVarDeclNode.toNodes(1)
    val varDeclsTree = locVarDeclTree.add(varDeclsNode,IDGenerator.nextID)
    val varDeclNode = varDeclsNode.toNodes.head
    val varDeclTree = varDeclsTree.add(varDeclNode,IDGenerator.nextID)
    val varDN = varDeclNode.toNodes.head
    val varDT = varDeclTree.add(varDN,IDGenerator.nextID)
    val varIdVarInitNode = varDN.toNodes(1)
    val varIdVarInitTree = varDT.add(varIdVarInitNode,IDGenerator.nextID)
    val context = bodyGenerator.context
    val possibleNames = IdentifierHandler.handleIdentifier(varIdVarInitTree,tab,context,List())
    bodyGenerator.traverseBody(Option(varIdVarInitTree),possibleNames,tab)
    varDeclsTree
  }

  def createNewBlockStatement(bodyTree: AST[Node]): AST[Node] ={
    val blockTree = bodyTree.children.head

    val blkTree = blockTree.children.head
    val blockStmtsTree = blkTree.children(1)
    if(blockStmtsTree.node.description != "<blockstatements>"){
      // if here, it is because <blockstatements> node does not exist
      // delete the empty node and the third child of <block> ast,
      // then fill it with <blockstatements> node and after reput the third child node into <block> children
      blkTree.children -= blockStmtsTree
      val bracesTree = blkTree.children(1)
      val bracesNode = bracesTree.node
      blkTree.children -= bracesTree
      val blkStmtsNode = blkTree.node.toNodes(1)
      val blkStmtsTree = blkTree.add(blkStmtsNode,IDGenerator.nextID)
      blkTree.add(bracesNode,IDGenerator.nextID)
      val block_stmtNode = blkStmtsNode.toNodes.head.toNodes.head
      blkStmtsTree.add(block_stmtNode,IDGenerator.nextID)
    }else {
      val blockStmtOrBlockStmtsBlockStmtTree = blockStmtsTree.children.head
      if (blockStmtOrBlockStmtsBlockStmtTree.node.description == "<block statement> ") {
        val blockStmtNode = blockStmtOrBlockStmtsBlockStmtTree.node
        val blockStmtTree = blockStmtsTree.add(blockStmtNode,IDGenerator.nextID)
        val blkStNode = blockStmtNode.toNodes.head
        val blkStTree = blockStmtTree.add(blkStNode,IDGenerator.nextID)
        blkStTree
      } else {
        val blockStmtNode = blockStmtOrBlockStmtsBlockStmtTree.node.toNodes(1)
        blockStmtOrBlockStmtsBlockStmtTree.add(blockStmtNode, IDGenerator.nextID)
      }
    }
  }
}
