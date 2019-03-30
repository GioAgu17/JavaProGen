package progen.peg.entities

import grizzled.slf4j.Logging

class ConstructorSignature(val className: String, val formalParameters: List[(String,String)],val id: Int ) extends Logging{
  info("constructor signature generated with ID: "+id+" for class "+className)
  override def toString: String = {
    val params = formalParameters.map(a => " "+ a._1 +" "+ a._2).mkString(",")
    className + "( "+params+" );"
  }
}
