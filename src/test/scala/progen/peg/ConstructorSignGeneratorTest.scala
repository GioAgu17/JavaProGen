package progen.peg

import com.typesafe.config.ConfigFactory
import network.server.ServerRPC
import org.scalatest.FunSuite
import progen.ConfigurationRetriever
import progen.peg.entities.{ConstructorSignature, Field}
import progen.prolog.ClientRpc

import scala.io.Source

class ConstructorSignGeneratorTest extends FunSuite {

  // SETUP
  val bufferedSrc = Source.fromFile("src/main/resources/prolog.txt")
  val rules = bufferedSrc.getLines().toList
  bufferedSrc.close()
  val configurationRetriever = new ConfigurationRetriever(ConfigFactory.load("my_app.conf"))
  val clientRpc = new ClientRpc
  val ok = clientRpc.setRules(rules)
  assert(ok)
  ServerRPC.start()
  val constructorSignGenerator = new ConstructorSignGenerator(configurationRetriever,clientRpc)



  // TEST 1
  test("validateConstrSign function"){
    val constrSign1 = new ConstructorSignature("B",List(("int","param1")),0)
    val resp1 = constructorSignGenerator.validateConstrSign(constrSign1)
    assert(resp1)
    val constrSign2 = new ConstructorSignature("B",List(("int","param2")),1)
    val resp2 = constructorSignGenerator.validateConstrSign(constrSign2)
    assert(!resp2)
    val constrSign3 = new ConstructorSignature("B",List(("int","param3"),("char","param4")),2)
    val resp3 = constructorSignGenerator.validateConstrSign(constrSign3)
    assert(resp3)
  }

  // TEST 2
  test("detMaxNoOfConstr function"){
    val fields = List.tabulate[Field](3)(i => new Field("int","field"+i,false))
    val detTotNoActual = constructorSignGenerator.detNoOfConstr(fields)
    val detTotNoExpected = 7
    assert(detTotNoActual == detTotNoExpected)
  }

  // TEST 3
  test("genParams function"){
    val fields = List.tabulate[Field](3)(i => new Field("int","field"+i,false))
    val params = constructorSignGenerator.genParams(fields)
    val fieldsSize = fields.size
    assert(params.size <= fieldsSize)
    params.map(p => p._1).foreach(typ => assert(typ == "int"))
  }

  // TEST 4
  test("genConstrSign function"){
    val className1 = "C"
    val fields1 = List.tabulate[Field](5)(i => new Field("long","field"+i,false))
    val constructor1 = constructorSignGenerator.genConstrSign(className1,fields1,1,0)

  }
}
