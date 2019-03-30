package progen.peg

import progen.ConfigurationRetriever
import progen.peg.entities.{ConstructorSignature, Field}
import progen.prolog.ClientRpc

import scala.util.Random


class ConstructorSignGenerator(configurationRetriever: ConfigurationRetriever,clientRpc: ClientRpc) {

  def genConstrSign(className: String, fields: List[Field],noOfConstr:Int, offSet: Int): List[ConstructorSignature] ={


      val constructors = List.tabulate[ConstructorSignature](noOfConstr)(i => new ConstructorSignature(className,genParams(fields),i+offSet))
      val constrValidated = constructors.filter(validateConstrSign)
      constrValidated
  }
  // from the fields generates the number of parameters
  def genParams(fields: List[Field]): List[(String,String)] ={
        val random = new Random
        // generates a random number whose max is the number of fields of the class
        val noOfParams = random.nextInt(fields.size+1)
        // takes noOfParams fields from the list of field in a random way
        val fieldsSelected = random.shuffle(fields).take(noOfParams)
        val paramTypes = fieldsSelected.map(f => f.stype)
        val paramPrefix = configurationRetriever.getParamPrefix
        val paramNames = List.tabulate[String](noOfParams)(i => paramPrefix + i)
        paramTypes zip paramNames
  }

  def detNoOfConstr(fields: List[Field]): Int ={
    val maxNo = fields.size
    def loop(i: Int, acc: Int): Int = i match{
      case `maxNo` => acc + 1
      case _ => acc + loop(i+1,fields.combinations(i).size)
    }
    val maxCombinations = loop(1,0) + 1
    val noOfConstrConfiguration = configurationRetriever.getConstructorsPerClass
    if(maxCombinations<noOfConstrConfiguration)
      maxCombinations
    else
      noOfConstrConfiguration

  }
  // validates the generated constructor signature by calling the
  def validateConstrSign(constructorSignature: ConstructorSignature): Boolean ={
        clientRpc.validateConstrSignature(constructorSignature)
  }
}
