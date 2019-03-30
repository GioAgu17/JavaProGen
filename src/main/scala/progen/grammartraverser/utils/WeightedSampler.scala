package progen.grammartraverser.utils

import grizzled.slf4j.Logging

import scala.util.Random

object WeightedSampler extends Logging{

  // given a list of weights summing to 1, sample and returns the index of the chosen one
  def weightedSampling(weights: List[Double]): Int ={
    // check if obs.sum == 1
    if(weights.sum == 1.0) {
      val sampleList = weights.scanLeft(0.0)(_ + _)

      val randDouble = Random.nextDouble()
      def loop(ls: List[Double],acc: Int): Int = ls match{
            case h1::h2::t =>
              if(h1<=randDouble && h2>randDouble)
                acc
              else
                loop(h2::t,acc+1)

            case _ =>
              error("did not find the index of list "+sampleList+" with random number: "+randDouble)
              sys.error("did not find index, see logging for more details")
          }
      loop(sampleList,0)
    }
    else sys.error("list of weights doesn't sum up to 1")

  }
}
