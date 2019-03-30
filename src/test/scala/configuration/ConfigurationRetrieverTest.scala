package configuration

import com.typesafe.config.{Config, ConfigFactory}
import progen.ConfigurationRetriever
import org.scalatest.FunSuite

class ConfigurationRetrieverTest extends FunSuite{
  val config: Config = ConfigFactory.load("my_app.conf")
  val configRet = new ConfigurationRetriever(config)
  test("getNoOfInterfaces"){
    val test = configRet.getInterfaces

  }
  test("getNumberInBetween"){
    val min = 2
    val max = 10
    val test = configRet.getNumberInBetween(2,10)
    assert(test>=min && test<=max )
  }

}
