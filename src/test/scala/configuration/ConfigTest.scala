package configuration

import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.FunSuite

import scala.collection.JavaConverters._

class ConfigTest extends FunSuite {
  val config: Config = ConfigFactory.load("my_app.conf")
  test("configuration file reads the file and gets the parameters"){


    // getting an integer value
    val maxNoOfClasses = Option(config.getInt("progen.noOfClasses.max"))
    val expectedMaxNoOfClasses = 10
    maxNoOfClasses match {
      case Some(a) => assert(expectedMaxNoOfClasses == a)
      case None => sys.error("config does not contain maximum number of Classes")
    }

    // getting a boolean value
    val allowMethodChain = Option(config.getBoolean("progen.allowMethodChain"))
    val expectedAllowMethodChain = false
    allowMethodChain match{
      case Some(a) => assert(expectedAllowMethodChain == a)
      case None => sys.error("config does not contain allow method chain")
    }

    // getting a string value
    val classNamePrefix = Option(config.getString("progen.classNamePrefix"))
    val expectedClassNamePrefix = "TenKLOC"
    classNamePrefix match{
      case Some(a) => assert(expectedClassNamePrefix == a)
      case None => sys.error("config does not contain class Name prefix")
    }

    // getting a list of String values

    val allowedTypes = Option(config.getStringList("progen.allowedTypes").asScala.toList)

    val expectedallowedTypes = List("char","byte","short","int","long","float","double","boolean","void")
    allowedTypes match{
      case Some(a) => assert(expectedallowedTypes.sorted == a.sorted)
      case None => sys.error("config does not contain allowed Types")
    }
  val test = config.getStringList("progen.allowedTypes").asScala.toList
    test.foreach(println)
  }
}
