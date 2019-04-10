package progen.peg

import com.typesafe.config.ConfigFactory
import org.scalatest.FunSuite
import progen.ConfigurationRetriever
import progen.peg.entities.Interface
import progen.prolog.ClientRpc

import scala.collection.mutable

class InterfaceAssignerTest extends FunSuite{
  val configurationRetriever = new ConfigurationRetriever(ConfigFactory.load("my_app.conf"))
  val interfaceAssigner = new InterfaceAssigner(configurationRetriever)
  val classTypes = List("A","C","F","G","H","I","L","M","N","O","P","Q","R","S","T","U","V","Z","W","Y","X")
  val interfaces = new InterfaceGenerator(configurationRetriever,new ClientRpc(configurationRetriever.getUseServerRPC)).genInterfaces(List("Inter1","Inter2","Inter3","Inter4","Inter5","Inter6","Inter7","Inter8","Inter9","Inter10","Inter11","Inter12"),classTypes,classTypes)
  test("test of checkOnConfig function"){
    // based on the actual configuration parameters value, the assertions should be right
    val expected1 = false
    val actual1 = interfaceAssigner.checkOnConfig(3,"C3")
    assert(expected1 == actual1)
    val expected2 = true
    val actual2 = interfaceAssigner.checkOnConfig(2,"C3")
    assert(expected2 == actual2)
    val expected3 = false
    val actual3 = interfaceAssigner.checkOnConfig(6,"C4")
    assert(expected3 == actual3)
    val expected4 = true
    val actual4 = interfaceAssigner.checkOnConfig(5,"C4")
    assert(expected4,actual4)
  }

  test("test of genRandomInterfacesNoAndCheck function"){
    val upperBound = configurationRetriever.getInterfaces
    val min = configurationRetriever.minNoOfInterfacesImplementedPerClass
    val max = configurationRetriever.maxNoOfInterfacesImplementedPerClass
    val res = interfaceAssigner.genRandomInterfacesNoAndCheck(upperBound)
    assert(res>=min && res<=max)
  }

  test("test takeNInterfacesAndCheck function"){
    val map = mutable.Map[Interface,Int]()
    interfaces.foreach(i => map += (i -> 0))
    val sizeExpected = 2
    val interfacesSelected1 = interfaceAssigner.takeNInterfacesAndCheck(sizeExpected,map)
    val interfaceSet = interfaces.toSet
    interfacesSelected1.foreach(i => assert(interfaceSet contains i))
    assert(interfacesSelected1.size == sizeExpected)
    val interfacesSelected2 = interfaceAssigner.takeNInterfacesAndCheck(0,map)
    assert(interfacesSelected2.isEmpty)
  }

  test("test assign function"){
    val interfaceSet = interfaces.toSet
    val assignedInterfaces = interfaceAssigner.assign(classTypes,interfaces)
    assert(classTypes.size == assignedInterfaces.size)
    assignedInterfaces.foreach(_ match{
      case Some(l) => l.foreach(i => assert(interfaceSet contains i))
      case _ =>
    })
  }

  test("test assignInterfaces function"){
    val interfaceSet = interfaces.toSet
    val implementAllInterfaces = configurationRetriever.getImplementAllInterfaces
    val interfacesAssigned = interfaceAssigner.assignInterfaces(classTypes,interfaces)
    val listWithoutOpt = interfacesAssigned.flatten.flatten.toSet
    assert(interfacesAssigned.size == classTypes.size)
    if(implementAllInterfaces)
      assert(interfaceSet == listWithoutOpt)
    else
      listWithoutOpt.foreach(i => assert(interfaceSet.contains(i)))


  }

}
