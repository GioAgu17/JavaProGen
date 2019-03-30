package progen.peg

import progen.ConfigurationRetriever
import progen.peg.entities._
import grizzled.slf4j.Logging
import progen.prolog.ClientRpc

import scala.annotation.tailrec
import scala.util.Random



class ClassGenerator(override val configurationRetriever: ConfigurationRetriever, val clientRpc: ClientRpc) extends Generator with MethodGenerator with Logging {
  /**
    * Generates skeletons of classes, assigning interfaces to them, creating field members and method signatures
    * @param globalTable the table  in which there are all the possible types for generating classes
    * @return a list of generated classes
    */
  def genClasses(globalTable: GlobalTable): (List[entities.Class],Int) ={
    // create inheritances
    info("................INHERITANCE CHAINS GENERATION STARTED.................")
    val inheritanceChains = createInheritances(globalTable.classNames)
    info("................INHERITANCE CHAINS GENERATED.................")
    // assign interfaces to classes
    info("................INTERFACE ASSIGNMENTS STARTED.................")
    val interfaceMap = assignInterfaces(globalTable)
    info("................INTERFACES ASSIGNED TO CLASSES...............")
    // find all the classes and put them also in single chains
    val totChains = detTotChains(inheritanceChains,globalTable)
    // for every chain, there is a list of list of fields for each class in the chain
    // now we have to find the correct
    info("................FIELDS GENERATION STARTED.................")

    val fieldMap = genFields(totChains,globalTable)
    info("................FIELDS GENERATED................")

    info(".............CONSTRUCTORS GENERATION STARTED.................")
    val offSet = globalTable.interfaces.flatMap(_.methods).size
    val (constructorsMap, offSetId) = genConstructors(globalTable,fieldMap,offSet)
    info(".............CONSTRUCTORS GENERATED.........")

    info(".............METHODS GENERATION STARTED.................")
    val (methodsMap,lastOffSet) =genMethSign(totChains,globalTable,interfaceMap,offSetId-offSet+1)
    info(".............METHODS GENERATED.........")

    val classes = globalTable.classNames.map(c => new entities.Class(c,interfaceMap(c),fieldMap(c),constructorsMap(c),methodsMap(c),findSuperClass(c,totChains)))
    (classes,lastOffSet)
  }
  // generates constructor signatures from the global table and the fields of every class
  def genConstructors(globalTable: GlobalTable,fieldMap: Map[String,List[Field]],offSetId:Int): (Map[String,List[ConstructorSignature]],Int) ={
      val classes = globalTable.classNames
      val constructorGenerator = new ConstructorSignGenerator(configurationRetriever,clientRpc)
      val noOfConstrForClass = classes.map(c => constructorGenerator.detNoOfConstr(fieldMap(c)))
    // Done for handling prolog IDs for constructor and meth signatures
      val offSets = noOfConstrForClass.scanLeft(offSetId)(_+_).init
      val classesNoOfConstrOffsets = classes zip noOfConstrForClass zip offSets map {
        case ((a,b),c) => (a,b,c)
      }
      val classesAndConstructors = classesNoOfConstrOffsets.map(c => (c._1, constructorGenerator.genConstrSign(c._1,fieldMap(c._1),c._2,c._3)))
      val offSet = classesAndConstructors.last._2.last.id
      var constrMap = Map[String,List[ConstructorSignature]]()
      classesAndConstructors.foreach(cc => constrMap += (cc._1 -> cc._2))
      (constrMap,offSet)
  }
  /**
    * Determines all the chains, including also the classes which have not inheritance relations, so as to fasten the generation
    * @param chains the inheritance chains
    * @return the list of all the inheritance chains, even the single ones
    */
  def detTotChains(chains: List[InheritanceChain], globalTable: GlobalTable): List[InheritanceChain] ={
    val classesInChain = chains.flatMap(c => c.chain ).toSet
    val classesNotInChain = globalTable.classNames.toSet.diff(classesInChain).toList
    val singleChains = classesNotInChain.map(c => new InheritanceChain(List(c)))
    val totChains = chains ++ singleChains
    totChains
  }
  /**
    * Finds superclasses by looking into every inheritance chain
    * @param className the name of the class whose superclass is looked up
    * @param totChains all the chains made by classes
    * @return either the name of the superclass or None if no superclass is found
    */
  def findSuperClass(className: String, totChains: List[InheritanceChain]): Option[String] = {
    val allListsOfChains = totChains.map(_.chain)
    val targetList = allListsOfChains.filter(l => l.contains(className)).head
    val index = targetList.indexOf(className)
    if(index == targetList.size -1)
      None
    else
      Some(targetList(index+1))
  }

  /**
    * Generates fields, given all the classes. For every chain calls the method on the object.
    * @return a map from a class name to its fields, just generated
    */
  def genFields(totChains: List[InheritanceChain],globalTable: GlobalTable): Map[String,List[Field]] ={
    val fieldsList = totChains.map(chain => chain.createFields(configurationRetriever,globalTable.classNames))
    // create methods
    val chainsAndFields = totChains zip fieldsList
    // see createFields test in InheritanceChainTest class
    var fieldMap = Map[String,List[Field]]()
    val classesAndFields = chainsAndFields.flatMap(cf => cf._1.chain zip cf._2)
    classesAndFields.foreach(cf => fieldMap += (cf._1 -> cf._2))
    fieldMap
  }
  /**
    * Assigns interfaces by creating an instanc of Interface Assigner
    * @return a map from a class name to its optional list of interfaces
    */
  def assignInterfaces(globalTable: GlobalTable): Map[String,Option[List[Interface]]] ={
    val interfaceAssigner = new InterfaceAssigner(configurationRetriever)
    val res =  globalTable.classNames zip interfaceAssigner.assignInterfaces(globalTable.classNames,globalTable.interfaces)
    val resWithMethodValidated = res.map(c =>
      (c._1,c._2 match{
      case Some(list) => Some(list.filter(i => i.methods.forall(m => clientRpc.validateMethodSignature(c._1,m))))
      case None => None
    })
    )
    var interfaceMap = Map[String,Option[List[Interface]]]()
    resWithMethodValidated.foreach(ci => interfaceMap +=  (ci._1 -> ci._2))
    interfaceMap
  }


  /**
    * Generates method signatures for every class calling a method on every class, not on every chain this time
    * @return a map from a class name to its list of method signatures
    */
    def genMethSign(totChains: List[InheritanceChain], globalTable: GlobalTable, interfaceMap: Map[String,Option[List[Interface]]],idOffSet:Int): (Map[String,List[MethodSignature]],Int) ={
      var methodsMap = Map[String,List[MethodSignature]]()
      // here we take the number of indices and of methods for each chain and then we have to launch method generator
      val indicesAndNoOfMethodsPerChain = totChains.map(_.detMethNumbers(configurationRetriever,globalTable))
      val chainsAndNumbers = totChains zip indicesAndNoOfMethodsPerChain
      val classesAndNumbers = chainsAndNumbers.flatMap(c => c._1.chain zip c._2)
      val classesAndNoOfMethods = classesAndNumbers.map(c => (c._1,c._2._1))
      val noOfMethodsInInterfaces = globalTable.interfaces.flatMap(_.methods).size
      val offSets = classesAndNoOfMethods.map(_._2).scanLeft(noOfMethodsInInterfaces)(_+_).init
      val classesAndRealNumbers = classesAndNoOfMethods zip offSets
      val possibleTypes = globalTable.classNames ++ globalTable.primitiveTypes
      val classesAndMethods = classesAndRealNumbers.map(c => (c._1._1,genMethodSignatures(c._1._2,c._2,possibleTypes,false,idOffSet)))
      val classesAndMethodsValidated :List[(String,List[MethodSignature])]= classesAndMethods.map(cm => validateMethods(cm))
      val methodsValidated: List[MethodSignature] = classesAndMethodsValidated.flatMap(cm => cm._2)
      val lastOffSet = methodsValidated.last.id
      classesAndMethodsValidated.foreach(cm => interfaceMap(cm._1) match{
        case Some(l) => methodsMap += (cm._1 -> (cm._2 ++ l.flatMap(i => i.methods)))
        case None => methodsMap += (cm._1 -> cm._2)
      })

      (methodsMap,lastOffSet)
    }

  /**
    * Creates inheritances between classes, helpful for determining the fields and the methods for each class
    * @param classNames all the class names
    * @return a list of inheritance chains, whose elements are class names
    */
  def createInheritances(classNames: List[String]): List[InheritanceChain]={
    val noOfInheritanceChains = configurationRetriever.getInheritanceChains
    // takes noOfInheritanceChains class names as being superclasses
    val superClasses = scala.util.Random.shuffle(classNames).take(noOfInheritanceChains)
    // for every superclass generate an inheritance chain
    val classTypeList = classNames.filter(s => !superClasses.contains(s))

    val inheritanceChains = superClasses.map(createChain(_,classTypeList))

    inheritanceChains
  }
  /**
    * Creates an inheritance chain starting from a superclass. Retrieves the number of subclasses
    * from the configuration params (i.e. inheritanceDepth), randomly selects a class from the possible
    * classes and asks the Prolog KB if it is possible to do so
    * @param superClass the superclass from which the inheritance chain is created
    * @return an inheritance chain of classes: the chain is a list from the bottom to the top of the inheritance hierarchy
    */
  def createChain(superClass: String, classTypes: List[String]): InheritanceChain ={

    @tailrec
    def loop(depth: Int, classTypes: List[String], acc: List[String]): List[String] = depth match{
      case 0 => acc
      case _ =>

        Random.shuffle(classTypes).find(clientRpc.validateExtension(_,acc.head))
                         match{
                          case Some(c) => loop(depth-1,classTypes,c::acc)
                          case None => loop(depth,classTypes,acc)
                          }


    }
    val inhList = loop(configurationRetriever.getInheritanceDepth,classTypes,List(superClass))

    new InheritanceChain(inhList)
  }

  /**
    * calls the Prolog KB to validate the generated method signatures for every class
    * @param classAndMethods represents a class along with its generated method signatures
    * @return a class with the validated method signatures
    */
  def validateMethods(classAndMethods: (String,List[MethodSignature])): (String,List[MethodSignature]) ={
    val list = classAndMethods._2.map(meth => (classAndMethods._1,meth))
    val validated = list.filter(cm => clientRpc.validateMethodSignature(cm._1,cm._2))
    val result = validated.map(v => v._2)
    (classAndMethods._1,result)
  }





}
