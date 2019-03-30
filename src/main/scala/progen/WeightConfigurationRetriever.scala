package progen

import com.typesafe.config.{Config, ConfigException}

import scala.collection.JavaConverters._
class WeightConfigurationRetriever(val config: Config) {

  def getWeightConfiguration(nodeDescription: String, size: Int): List[Double] ={
    try {
      val weights = config.getDoubleList("progen.weights." + nodeDescription).asScala.toList
      weights.map(w => w.doubleValue())
    }catch{
      case missing: ConfigException =>
        val w = 1.0/size
        List.fill[Double](size)(w)
      case ec: Throwable => sys.error("got some other exception when retrieving weights: "+ec.getMessage)
    }

  }
}
