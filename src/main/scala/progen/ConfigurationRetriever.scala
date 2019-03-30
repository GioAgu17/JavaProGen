package progen

import com.typesafe.config.Config

import scala.collection.JavaConverters._

class ConfigurationRetriever(val config: Config) {



  val maxInterface: Int = config.getInt("progen.noOfInterfaces.max")
  val minInterface: Int = config.getInt("progen.noOfInterfaces.min")

  def getInterfaces: Int = {
    getNumberInBetween(minInterface,maxInterface)
  }

  val maxClasses: Int = config.getInt("progen.noOfClasses.max")
  val minClasses: Int = config.getInt("progen.noOfClasses.min")
  def getClasses: Int = {
    getNumberInBetween(minClasses,maxClasses)
  }

  val classPrefix: String = config.getString("progen.classNamePrefix")
  def getClassPrefix: String = {
    classPrefix
  }

  val interfacePrefix: String = config.getString("progen.interfaceNamePrefix")
  def getInterfacePrefix: String = {
    interfacePrefix
  }
  val maxMethodsPerClass: Int = config.getInt("progen.noOfMethodsPerClass.max")
  val minMethodsPerClass: Int = config.getInt("progen.noOfMethodsPerClass.min")
  def getNoOfMethodsPerClass : Int = {
    getNumberInBetween(minMethodsPerClass,maxMethodsPerClass)
  }
  val maxMethodsPerInterface: Int = config.getInt("progen.noOfMethodsPerInterface.max")
  val minMethodsPerInterface: Int = config.getInt("progen.noOfMethodsPerInterface.min")
  def getMethodsPerInterface: Int = {

    getNumberInBetween(minMethodsPerInterface,maxMethodsPerInterface)
  }
  val maxConstructorsPerClass: Int = config.getInt("progen.noOfConstructorsPerClass.max")
  val minConstructorsPerClass: Int = config.getInt("progen.noOfConstructorsPerClass.min")
  def getConstructorsPerClass: Int ={
    getNumberInBetween(minConstructorsPerClass,maxConstructorsPerClass)
  }

  val maxParamsPerMethod: Int = config.getInt("progen.noOfParametersPerMethod.max")
  val minParamsPerMethod: Int = config.getInt("progen.noOfParametersPerMethod.min")
  def getParametersPerMethod: Int = {
    getNumberInBetween(minParamsPerMethod,maxParamsPerMethod)
  }

  val methodPrefix: String = config.getString("progen.methodNamePrefix")
  def getMethodPrefix: String ={
    methodPrefix
  }

  val parameterPrefix: String = config.getString("progen.parameterNamePrefix")
  def getParamPrefix: String ={
    parameterPrefix
  }

  val fieldNamePrefix: String = config.getString("progen.fieldNamePrefix")
  def getFieldNamePrefix: String ={
    fieldNamePrefix
  }

  val allowedTypes: List[String] = config.getStringList("progen.allowedTypes").asScala.toList
  def getAllowedTypes: List[String] ={
    allowedTypes
  }

  val minInheritanceChains: Int = config.getInt("progen.noOfInheritanceChains.min")
  val maxInheritanceChains: Int = config.getInt("progen.noOfInheritanceChains.max")
  def getInheritanceChains: Int = {
    getNumberInBetween(minInheritanceChains,maxInheritanceChains)
  }

  val minInheritanceDepth: Int = config.getInt("progen.inheritanceDepth.min")
  val maxInheritanceDepth : Int= config.getInt("progen.inheritanceDepth.max")
  def getInheritanceDepth: Int ={
    getNumberInBetween(minInheritanceDepth,maxInheritanceDepth)
  }
  val minNoOfInterfacesImplemented: Int = config.getInt("progen.noOfInterfacesImplemented.min")
  val maxNoOfInterfacesImplemented: Int = config.getInt("progen.noOfInterfacesImplemented.max")
  def getNoOfInterfacesImplemented: Int = {
    getNumberInBetween(minNoOfInterfacesImplemented,maxNoOfInterfacesImplemented)
  }

  val minNoOfInterfacesImplementedPerClass: Int = config.getInt("progen.noOfInterfacesImplementedPerClass.min")
  val maxNoOfInterfacesImplementedPerClass: Int = config.getInt("progen.noOfInterfacesImplementedPerClass.max")
  def getNoOfInterfacesImplementedPerClass :Int ={
    getNumberInBetween(minNoOfInterfacesImplementedPerClass,maxNoOfInterfacesImplementedPerClass)
  }

  val implementAllInterfaces: Boolean = config.getBoolean("progen.implementAllInterfaces")
  def getImplementAllInterfaces: Boolean ={
    implementAllInterfaces
  }
  val minNoOfImplementationPerInterface: Int = config.getInt("progen.noOfImplementationPerInterface.min")
  val maxNoOfImplementationPerInterface: Int = config.getInt("progen.noOfImplementationPerInterface.max")
  def getNoOfImplementationPerInterface: Int ={
    getNumberInBetween(minNoOfImplementationPerInterface,maxNoOfImplementationPerInterface)
  }
  val minNoOfClassFields: Int = config.getInt("progen.noOfClassFields.min")
  val maxNoOfClassFields: Int = config.getInt("progen.noOfClassFields.max")
  def getNoOfClassFields: Int ={
    getNumberInBetween(minNoOfClassFields,maxNoOfClassFields)
  }
  def getNumberInBetween(i: Int, i1: Int): Int = {
    val rnd = new scala.util.Random
    if(i1<i)
      i1 + rnd.nextInt((i-i1)+1)
    else
      i + rnd.nextInt((i1-i)+1)
  }





}
