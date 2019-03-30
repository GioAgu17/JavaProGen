package network

import org.scalatest.FunSuite
import progen.prolog.PrologConverter

class PrologConverterTest extends FunSuite {
  test("parameterConversion function"){
    val params = List(("int","a"),("char","b"),("short","c"))
    val types = PrologConverter.paramsTypeConversion(params)
    println(types)
  }

}
