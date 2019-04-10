package progen.peg.entities

import progen.ConfigurationRetriever
import progen.peg.InterfaceGenerator

import scala.annotation.tailrec
import scala.util.Random

class InheritanceChain(val chain: List[String]){

    def createFields(configurationRetriever: ConfigurationRetriever, classTypes: List[String]): List[List[Field]] = {
      val chainReversed = chain.reverse
      val noOfFields = verify(chainReversed.map(c => configurationRetriever.getNoOfClassFields))
      val primitiveTypes = configurationRetriever.getAllowedTypes.filter(t => t!="void")
      val possibleTypes = Random.shuffle(classTypes++primitiveTypes)
      val prefixName = configurationRetriever.getFieldNamePrefix
      val index = InterfaceGenerator.trackIndex(noOfFields,0)
      val noAndIndices = noOfFields zip index
      val fields = genFields(noAndIndices,Nil,possibleTypes,prefixName)
      fields.reverse
    }

  /**
    * Takes a list of numbers corresponding to the number of fields for each class
    * and creates a new list considering that the fields of a superclass are inherited
    * by its subclasses
    * @param l list whose element is the number of field for each class of the inheritance chain
    * @return a list whose element correspond to the real number of fields to be generated
    */
  def verify(l: List[Int]): List[Int] ={
        if(l == l.sorted ) {
          val res = countNewNoOfMembers(l,List(l.head))
          res
        }
        else{
          @tailrec
          def loop(acc: List[Int],ls: List[Int]) : List[Int] = acc match{
            case Nil=> {
              //what do you do when this happens?
              System.exit(1)
              null
            }
            case h :: Nil => ls.reverse
            case h1::h2::t => if(h2<h1) loop(h1::t,h1::ls) else loop(h2::t,h2::ls)
          }
          val list = loop(l,List(l.head))
          countNewNoOfMembers(list,List(list.head))
        }
      }

  /**
    * takes a list of numbers and an accumulator and returns a list where each element represents the real number of
    * members to be generated
    * @param ls list of numbers
    * @param acc accumulator, initially empty
    * @return a reshaped list of numbers
    */
  @tailrec
  private def countNewNoOfMembers(ls: List[Int], acc: List[Int]) : List[Int] = ls match{
    case Nil => {
      //what do you do when this happens?
      System.exit(1)
      null
    }
    case h::Nil => acc.reverse
    case h1::h2::t => countNewNoOfMembers(h2::t,(h2-h1) :: acc)
  }
  @tailrec
  private def genFields(ls: List[(Int,Int)],acc: List[List[Field]],possibleTypes: List[String],prefix: String): List[List[Field]] = ls match{
    case Nil => acc.reverse
    case h::t => if(acc.isEmpty) genFields(t, createFieldList(h._1,h._2,possibleTypes,prefix) :: acc,possibleTypes,prefix) else genFields(t,(createFieldList(h._1,h._2,possibleTypes,prefix)++acc.head.map(f => new Field(f.stype,f.name,true))) :: acc,possibleTypes,prefix)
  }
  def createFieldList(no:Int, index: Int, possibleTypes: List[String], prefix: String): List[Field] ={
    val res = List.tabulate(no)(i => new Field(Random.shuffle(possibleTypes).head,prefix + (index+i),false))
    res
  }

  //TODO: method to test and comment
  def detMethNumbers(configurationRetriever: ConfigurationRetriever, globalTable: GlobalTable): List[(Int,Int)]={
      val chainReversed = chain.reverse
      val noOfMethods = verify(chainReversed.map(c => configurationRetriever.getNoOfMethodsPerClass))
      val noOfMethodsInGeneral = globalTable.interfaces.map(i => i.methods.size).sum
      val index = InterfaceGenerator.trackIndex(noOfMethods,noOfMethodsInGeneral)
      val noAndIndices = noOfMethods zip index
      noAndIndices.reverse
  }

  override def toString: String = {
    chain.toString()
  }

}
