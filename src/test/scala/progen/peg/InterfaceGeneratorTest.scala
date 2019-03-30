package progen.peg

import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.FunSuite
import progen.ConfigurationRetriever
import progen.prolog.ClientRpc

class InterfaceGeneratorTest extends FunSuite{

    val interfaceNames = List("Inter1", "Inter2", "Inter3", "Inter4", "Inter5")
    val allowedTypes = List("int", "char", "byte", "short", "int")
    val classTypes = List("TENKLOC1","TENKLOC2","TENKLOC3","TENKLOC4","TENKLOC5")
    val possibleTypes = allowedTypes ++ classTypes
    val config: Config = ConfigFactory.load("my_app.conf")
    val configurationRetriever = new ConfigurationRetriever(config)
    val interfaceGenerator = new InterfaceGenerator(configurationRetriever,new ClientRpc)



    test("genMethodName test"){
      val prefix = "m"
      val n = 5
      val test = interfaceGenerator.genMethodName(prefix,n)
      val expected = "m6"
      assert(test == expected)
    }

    test("genRetType test"){
      val retType = interfaceGenerator.genRetType(possibleTypes)
      assert(possibleTypes.contains(retType))
    }

  test("trackIndex test"){
    val noOfMethodsPerInterface = List(2,3,1,4)
    val indexLst = InterfaceGenerator.trackIndex(noOfMethodsPerInterface,0)
    val expectedIndexLst = List(0,2,5,6)
    assert(indexLst == expectedIndexLst)
  }

  test("genParameters test"){
    val paramPrefix = configurationRetriever.getParamPrefix
    val params = interfaceGenerator.genParameters(possibleTypes)
    val types = params.map(_._1)
    types.foreach(t => assert(possibleTypes.contains(t)))
    val names = params.map(_._2)
    names.foreach(s => assert(s.contains(paramPrefix)))
  }
  test("genMethodSignatures test"){
    val noOfMethods = 4
    val prefix = configurationRetriever.getMethodPrefix
    val index = 0
    val methodSignatures = interfaceGenerator.genMethodSignatures(noOfMethods,index,possibleTypes,true,0)
    assert(methodSignatures.size == 4)
    // check if every return type is in the possible types
    methodSignatures.foreach(m => assert(possibleTypes.contains(m.returnType)))
    // check that the prefix is contained in every method name and the suffix goes from index+1 to the list size+1
    methodSignatures.view.zipWithIndex.foreach(m => assert(m._1.name == prefix+(m._2+index+1)))
  }
  test("genInterfaces test"){
    val interfaces = interfaceGenerator.genInterfaces(interfaceNames,allowedTypes,classTypes)
    val prefix = configurationRetriever.getInterfacePrefix
    // check that all interfaces have different names (as in genMethodSignatures test)
    interfaces.view.zipWithIndex.foreach(i => assert(i._1.name == prefix+(i._2+1)))

  }

}
