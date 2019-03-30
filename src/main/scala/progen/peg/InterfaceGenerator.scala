package progen.peg

import progen.ConfigurationRetriever
import progen.peg.entities.Interface
import progen.prolog.ClientRpc

class InterfaceGenerator(override val configurationRetriever: ConfigurationRetriever,val clientRpc: ClientRpc) extends Generator with MethodGenerator{

  /**
    * generates interfaces with parameters from configuration file
    * @return a list of interfaces with each one a method signature
    */
  def genInterfaces(interfaceNames: List[String], allowedTypes: List[String],classNames: List[String]): List[Interface] ={
    val possibleTypes = allowedTypes ++ classNames
    val methodsPerInterface = interfaceNames.map(_ => configurationRetriever.getMethodsPerInterface)
    val indexList = InterfaceGenerator.trackIndex(methodsPerInterface,0)
    val methodsAndIndex = methodsPerInterface zip indexList
    List.tabulate[Interface](interfaceNames.length)(i =>
      new Interface(interfaceNames(i),genMethodSignatures(methodsAndIndex(i)._1,methodsAndIndex(i)._2,possibleTypes,true,0)))
  }

}
object InterfaceGenerator{
  /**
    * Helper method for keeping track of the identifier for methods in different interfaces.
    * This is done to avoid two methods have the same name
    * @param ls list whose each element represents the number of methods in an interface
    * @param acc accumulator for keeping track the identifier
    * @return a list of integers
    */
  def trackIndex(ls: List[Int],acc: Int): List[Int] = ls match{
    case List() => Nil
    case h :: t => acc :: trackIndex(t,acc+h)
  }
}

