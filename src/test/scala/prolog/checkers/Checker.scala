package prolog.checkers

import scala.util.Random

trait Checker {
  // implemented by instance classes
    def parseAndCheckKB(source: String): Boolean

  /**
    * util method to create a Prolog list from an array
    * @param arr the array given in input, which is usually an array of formal parameters or expressions
    * @tparam T the type of the elements of the array
    * @return a String representing a Prolog List following Prolog syntax
    */
    def createParamLst[T](arr: Array[T]): String ={
        var paramLst = "['"
        for ((x, i) <- arr.view.zipWithIndex) {
            if (i == arr.length - 1)
                paramLst = paramLst + x + "']"
            else
                paramLst = paramLst + x + "','"
        }
        paramLst
    }

  /**
    * generates three random parameters to test some Prolog rules
    * @return
    */
  def randomTreeParams(rnd: Random):(Int,Int,Int) ={
    val id = rnd.nextInt(100)
    val parId = id-1
    var depth = rnd.nextInt(100)
    while(depth == id || depth==parId)
      depth = rnd.nextInt(100)
    (id,parId,depth)
  }
}
