package progen.peg

import com.typesafe.config.ConfigFactory
import org.scalatest.FunSuite
import progen.ConfigurationRetriever
import progen.peg.entities.{Interface, MethodSignature}

class MethodGeneratorTest extends FunSuite {
  val configurationRet = new ConfigurationRetriever(ConfigFactory.load("my_app.conf"))
  val methodGenerator = new MethodGenerator(){
    override val configurationRetriever: ConfigurationRetriever = configurationRet
  }

  test("test genNoOfMethods function"){
    val methodSign1 = new MethodSignature("int","m1", List(("int","a"),("char","c")),false,0)
    val inter1 = new Interface("inter1",List(methodSign1))
    val optionListInterfaces = Option(List(inter1))
    val expectedMinSize1 = 0
    val expectedMaxSize1 = configurationRet.maxMethodsPerClass - 1
    val noOfMethods1 = methodGenerator.genNoOfMethods(optionListInterfaces)
    assert(noOfMethods1<=expectedMaxSize1 && noOfMethods1>=expectedMinSize1)
    val noOfMethods2 = methodGenerator.genNoOfMethods(None)
    val expectedMinSize2 = configurationRet.minMethodsPerClass
    val expectedMaxSize2 = configurationRet.maxMethodsPerClass
    assert(noOfMethods2<=expectedMaxSize2 && noOfMethods2>=expectedMinSize2)
  }

}
