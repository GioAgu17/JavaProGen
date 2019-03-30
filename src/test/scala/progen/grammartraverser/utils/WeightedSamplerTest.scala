package progen.grammartraverser.utils

import org.scalatest.FunSuite

class WeightedSamplerTest extends FunSuite {
    test("weight test"){
      val weights = List(0.0,0.0,1.0)
      val randDouble = 0.54
      def loop(ls: List[Double],acc: Int): Int = ls match{
        case h1::h2::t =>
          if(h1<=randDouble && h2>randDouble)
            acc
          else
            loop(h2::t,acc+1)

        case _ =>
          sys.error("did not find index, see logging for more details")
      }
      println(loop(weights,0))

    }


}
