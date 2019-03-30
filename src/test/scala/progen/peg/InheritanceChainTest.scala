package progen.peg

import com.typesafe.config.ConfigFactory
import org.scalatest.FunSuite
import progen.ConfigurationRetriever
import progen.peg.entities.InheritanceChain


class InheritanceChainTest extends FunSuite{
  // possible types in the testing phase
  val possibleTypes = List("A","B","C","D","int","char","boolean","short")
  // parameter types
  val prefix = "field"
  // an inheritance chain with three classes: M, N and O
  val inheritanceCh1 = new InheritanceChain(List("M","N","O"))
  val inheritanceCh2 = new InheritanceChain(List("P","Q","R"))
  val classTypes = List("A","B","C","D","E","F")
  val chains = inheritanceCh1 :: inheritanceCh2 :: Nil
  val configurationRetriever = new ConfigurationRetriever(ConfigFactory.load("my_app.conf"))
  test("createFields function"){

      val fieldList = inheritanceCh1.createFields(configurationRetriever,classTypes)
      val listTest = fieldList.flatten.filter(f => !f.inherited)
      val namesList = listTest.map(f => f.name)
      // testing that the names are always different for each non inherited field
      assert(namesList == namesList.distinct)
      val fields = chains.map(chain => chain.createFields(configurationRetriever,classTypes))
      val chainsAndFields = chains zip fields
      // I have for each chain (list of classes) a list of list of fields - now I want to come to the class level
      val classesAndFields = chainsAndFields.map(cf => cf._1.chain zip cf._2)
      val allClassesAndFields = classesAndFields.flatten
      allClassesAndFields.foreach(a => println("class "+a._1+" has the following fields:\n"+a._2.toString()) )
    }

  test("createFieldList function"){
      val listOfFields = inheritanceCh1.createFieldList(3,1,possibleTypes,prefix)
      val sizeExpected = 3
      val indexListExpected = List("field1","field2","field3")
      assert(listOfFields.size == sizeExpected)
      val names = listOfFields.map(f => f.name)
      assert(names == indexListExpected)
    }


  test("verify function"){
    val init = List(3,4,5)
    val listVerified = inheritanceCh1.verify(init)
    val expectedList = List(3,1,1)
    assert(expectedList == listVerified)
    val init1 = List(3,2,1)


  }
  test("create fields, methods and constructors with single class chain"){
    val singleClassChain = new InheritanceChain(List("A"))
    val fields = singleClassChain.createFields(configurationRetriever,classTypes)
    assert(fields.size == 1)
    fields.flatten.foreach(println)
  }



}
