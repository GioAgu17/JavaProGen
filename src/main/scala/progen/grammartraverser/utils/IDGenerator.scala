package progen.grammartraverser.utils

object IDGenerator{


  var id : Int= 0

    def nextID: Int ={
      synchronized{
      id+=1
      id
    }
  }
}
