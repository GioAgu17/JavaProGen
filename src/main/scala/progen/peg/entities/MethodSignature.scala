package progen.peg.entities

import grizzled.slf4j.Logging

class MethodSignature(val returnType: String,val name: String,val formalParameters: List[(String,String)], val fromInterface: Boolean, val id: Int)extends Logging {
  info("method signature generated with ID: "+id)
  override def toString: String = {
   val params = formalParameters.map(a => " "+ a._1 +" "+ a._2).mkString(",")
    returnType + " " + name + "( " + params + " );"
  }
}
