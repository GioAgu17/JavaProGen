package progen.peg

import progen.ConfigurationRetriever
import progen.peg.entities.Interface

import scala.collection.mutable
import scala.util.Random

class InterfaceAssigner(configurationRetriever: ConfigurationRetriever) {

  /**
    * assign interfaces to each class, calling the assign function on the entire interface list or just a portion of it,
    * depending on a configuration parameter
    * @param classNames all the names of the generated classes
    * @param interfaces all the generated interfaces
    * @return a list of options corresponding to the interface assignment to each class
    */
  def assignInterfaces( classNames: List[String], interfaces: List[Interface]): List[Option[List[Interface]]] ={
    var interfacesAssignedToEachClass = List[Option[List[Interface]]]()
    val implementAllInterfaces = configurationRetriever.getImplementAllInterfaces
    val interfaceSet = interfaces.toSet

    if(implementAllInterfaces){
      // every class has a List[Option[Interface]]
     var interfacesAssignedToEachClass = assign(classNames,interfaces)
      var interfaceSelectedSet = interfacesAssignedToEachClass.flatten.flatten.toSet
      while(! (interfaceSet == interfaceSelectedSet)){

        val remainingInterfaces = (interfaceSet diff interfaceSelectedSet).toList
        val interfacesRemainedAssignedToEachClass = assign(classNames,remainingInterfaces)
        val unionOfInterfacesAssignedToEachClass = (interfacesAssignedToEachClass zip interfacesRemainedAssignedToEachClass).map(i => i._1 match{
          case Some(list) => i._2 match{
            case Some(list1) => Some(list ++ list1)
            case None =>  Some(list)
          }
          case None => i._2 match{
            case Some(list1) => Some(list1)
            case None => None
          }
        })
        interfacesAssignedToEachClass = unionOfInterfacesAssignedToEachClass
        interfaceSelectedSet = interfacesAssignedToEachClass.flatten.flatten.toSet
      }
      interfacesAssignedToEachClass
    }else{
      var randomNoOfInterfacesImplemented = Random.nextInt(interfaces.size)
      // condition on configuration parameters and a condition on the number of interfaces implemented that
      // has to be greater than the number of classes divided by two: this condition is used to reduce
      // the risk of not having a sufficient number of interfaces to be assigned to classes
      while(!(randomNoOfInterfacesImplemented <= configurationRetriever.maxNoOfInterfacesImplemented && randomNoOfInterfacesImplemented >= configurationRetriever.minNoOfInterfacesImplemented && randomNoOfInterfacesImplemented>=classNames.size/2)){
        // TODO solve the problem of infinite loop here because of combinations of numbers

        randomNoOfInterfacesImplemented = Random.nextInt(interfaces.size)
      }
      val interfacesLimited = Random.shuffle(interfaces).take(randomNoOfInterfacesImplemented)
      interfacesAssignedToEachClass = assign(classNames,interfacesLimited)
      interfacesAssignedToEachClass
    }
  }

  /**
    * the function that actually assigns the interface to each class
    * @param classNames all the class names
    * @param interfaces all the interfaces
    * @return a list for each class with a list of interfaces if assigned, None if no interface is assigned
    */
  def assign(classNames: List[String], interfaces: List[Interface]): List[Option[List[Interface]]] = {
    val noOfInterfacesPerClass = classNames.map(c => genRandomInterfacesNoAndCheck(configurationRetriever.maxNoOfInterfacesImplementedPerClass))

    // for each number n of interfaces to be implemented per class, take n randomly selected interfaces
    // and assign them to the list assignment
    val occurences = mutable.Map[Interface,Int]()
    interfaces.foreach(i => occurences += ( i -> 0))
    val assignment = noOfInterfacesPerClass.map(n => takeNInterfacesAndCheck(n,occurences))

    val resList = assignment.map(list => if(list.isEmpty) None else Some(list))
    resList
  }

  /**
    * generates a random number of interfaces and then check against a configuration parameter
    * @param upperBound the maximum random number allowed
    * @return an integer corresponding to the random number, accordingly to the configuration parameter
    */
  def genRandomInterfacesNoAndCheck(upperBound: Int): Int ={
    var rand = Random.nextInt(upperBound+1)
    var check = checkOnConfig(rand,"C3")
    while(!check){
      rand = Random.nextInt(upperBound)
      check = checkOnConfig(rand,"C3")
    }
    rand
  }

  /**
    * Depending on the type of check, performs a different check on the configuration file through
    * the configuration parameter
    * @param i the number to be checked
    * @param param the type of check to be performed
    * @return true if the check ends well, false otherwise
    */
  def checkOnConfig(i: Int, param: String): Boolean ={
    if(param == "C3") {
      val greaterEqual = i >= configurationRetriever.minNoOfInterfacesImplementedPerClass
      val lessEqual = i <= configurationRetriever.maxNoOfInterfacesImplementedPerClass
      greaterEqual && lessEqual
    }else{
      val greaterEqual = i >= configurationRetriever.minNoOfImplementationPerInterface
      val lessEqual = i <= configurationRetriever.maxNoOfImplementationPerInterface

      greaterEqual && lessEqual
    }

  }

  /**
    * Takes n random interfaces among all available interfaces and check against
    * the number of occurrences of each interface if they can be assigned
    * @param n the number of interfaces to be drawn
    * @param interMap the map of the occurences of each assignment for each interface
    * @return a list of interfaces assigned to a class
    */
  def takeNInterfacesAndCheck(n: Int,interMap: mutable.Map[Interface,Int]): List[Interface] ={
    if(n==0)
      List()
    else if(interMap.values.map(i => checkOnConfig(i+1,"C4")).forall(_ == false) || interMap.values.sum >= interMap.size)
      List()
    else {
      val interfaces = interMap.keys.toList
      var selectedInterf = Random.shuffle(interfaces).take(n)

      var check = selectedInterf.map(i => checkOnConfig(interMap(i) + 1, "C4")).forall(_ == true)

      while (!check) {

        selectedInterf = Random.shuffle(interfaces).take(n)
        check = selectedInterf.map(i => checkOnConfig(interMap(i) + 1, "C4")).forall(_ == true)
      }
      selectedInterf.foreach(i => interMap(i) = interMap(i) + 1)
      selectedInterf
    }
  }
}
