import com.typesafe.config.{Config, ConfigFactory}
import progen.ConfigurationRetriever
import progen.grammarparser.ParserGen
import progen.peg.ContextGenerator
import grizzled.slf4j.Logger
import org.slf4j.LoggerFactory
import progen.grammartraverser.TreeBuilder
import progen.prolog.ClientRpc

import scala.io.Source


object Main extends App {
  disableWarning()
  val logger = LoggerFactory.getLogger(Main.getClass)
  logger.info("Main started")

  logger.info("......reading grammar file......")
  val gramBufferedSrc = Source.fromFile("src/main/resources/featherweight_java.txt")
  val gramFile = gramBufferedSrc.getLines().toList
  gramBufferedSrc.close()
  logger.info("......grammar file closed........")

  logger.info(".......reading configuration file.....")
  val config: Config = ConfigFactory.load("my_app.conf")
  logger.info("...... configuration file loaded......")


  logger.info("......reading Prolog Rules file........")
  val prologBufferedSrc = Source.fromFile("src/main/resources/prolog.txt")
  val prologFile = prologBufferedSrc.getLines().toList
  prologBufferedSrc.close()
  logger.info(".......Prolog Rules file closed......")

  logger.info("......initializing rules for the RPC Server.....")
  val clientRpc = new ClientRpc()
  val rulesPassed = clientRpc.setRules(prologFile)



  val configurationRetriever = new ConfigurationRetriever(config)

  // starting RPC Server
  ServerRPC.start()

  logger.info("....First phase generation is started......")
  val contextGenerated  = new ContextGenerator(configurationRetriever).firstPhaseGeneration(clientRpc)
  logger.info(".....First phase generation is ended.....")


  logger.info(".....Start building grammar graph from grammar rules.....")
  val grammarGraph = ParserGen.buildGraph(gramFile)
  logger.info("......grammar graph built.....")

  val ast = new TreeBuilder(configurationRetriever,clientRpc).secondPhaseGeneration(grammarGraph, contextGenerated)







  def disableWarning(): Unit = {
    System.err.close()
    System.setErr(System.out)
  }




}
