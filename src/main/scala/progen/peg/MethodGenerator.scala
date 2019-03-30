package progen.peg

import progen.peg.entities.{Interface, MethodSignature}

import scala.util.Random

trait MethodGenerator extends Generator{
  /**
    * Generates method signatures for every interface or class
    * @param noOfMethods number of methods in an interface or a class
    * @param index the number for the name of the methods
    * @param possibleTypes all the possible types
    * @return a list of method signatures
    */
  def genMethodSignatures(noOfMethods: Int, index: Int, possibleTypes: List[String],fromInterface: Boolean,offSet:Int): List[MethodSignature]={
    val prefix = configurationRetriever.getMethodPrefix
    List.tabulate[MethodSignature](noOfMethods)(i => if(fromInterface)
      new MethodSignature(genRetType(possibleTypes),genMethodName(prefix,index+i),genParameters(possibleTypes),fromInterface,index+i)
    else  new MethodSignature(genRetType(possibleTypes),genMethodName(prefix,index+i),genParameters(possibleTypes),fromInterface,index+i+offSet))
  }
  /**
    * generates a String representing a return type
    * @param possibleTypes list of all possible types
    * @return
    */
  def genRetType(possibleTypes: List[String]):String ={
    val random = new Random
    possibleTypes(random.nextInt(possibleTypes.length))
  }

  /**
    * Concatenates a string with a number added by one and returns a String, namely a method name
    * @param prefix the prefix
    * @param n the number to be concatenated with the string
    * @return the concatenation between prefix and number
    */
  def genMethodName(prefix: String,n: Int): String ={
    prefix + (n+1)
  }
  /**
    * Generates parameters for a method taking the possible types of the program and the prefix for parameters
    * @param possibleTypes possible types to assign to each parameter
    * @return a list of tuples where each tuple contains the name of the param and its type
    */
  def genParameters(possibleTypes: List[String]): List[(String,String)]={
    val possTypes = possibleTypes.filter(_ != "void")
    val noOfParameters = configurationRetriever.getParametersPerMethod
    val prefix = configurationRetriever.getParamPrefix
    val random = new Random
    List.tabulate[(String,String)](noOfParameters)(i => (possTypes(random.nextInt(possTypes.length)),prefix+(i+1)))
  }




  /**
    * Checks if a class has some interfaces assigned and if yes, generates a number between the number of methods
    * already present in its interfaces and the maximum number of methods per class. Otherwise, generates
    * a number between min and max number of methods per class
    * @param maybeInterfaces an optional list of interfaces assigned to a class
    * @return an integer representing the number of methods to generate per class
    */
  def genNoOfMethods(maybeInterfaces: Option[List[Interface]]): Int = maybeInterfaces match{
    case Some(l) => configurationRetriever.getNumberInBetween(l.map(i => i.methods.size).sum, configurationRetriever.maxMethodsPerClass) - l.map(i => i.methods.size).sum
    case None => configurationRetriever.getNoOfMethodsPerClass
  }

}
