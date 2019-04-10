import com.typesafe.config.{Config, ConfigFactory}
import progen.ConfigurationRetriever
import progen.grammarparser.{Node, ParserGen}
import progen.peg.ContextGenerator
import org.slf4j.LoggerFactory
import progen.grammartraverser.{AST, TreeBuilder}
import progen.grammartraverser.utils.GlobalVariables
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

  val configurationRetriever = new ConfigurationRetriever(config)


  logger.info("......initializing client for Prolog and Prolog rules.....")
  val useRpc: Boolean = configurationRetriever.getUseServerRPC
  val clientRpc = new ClientRpc(useRpc)
  val rulesPassed = clientRpc.setRules(prologFile)
  logger.info("............finished initializing client for Prolog and Prolog rules")




  logger.info("...........if useRPC enabled starting Server RPC........")
  if(useRpc)
    ServerRPC.start()
  logger.info("..........if use RPC enabled finished starting Server RPC")



  logger.info("....First phase generation is started......")
  val contextGenerator = new ContextGenerator(configurationRetriever)
  val contextGenerated  = contextGenerator.firstPhaseGeneration(clientRpc)
  logger.info(".....First phase generation is ended.....")


  logger.info(".....Start building grammar graph from grammar rules.....")
  val grammarGraph = ParserGen.buildGraph(gramFile)
  logger.info("......Grammar graph built.....")


  logger.info(".....Second phase generation is started...... ")
  val treeBuilder = new TreeBuilder(configurationRetriever,clientRpc)
  val ast = treeBuilder.secondPhaseGeneration(grammarGraph, contextGenerated)
  logger.info(".........Second phase generation is ended.....")


  logger.info(".......Checking if the generated LOC are enough...")
  val wantedLoc: Int = configurationRetriever.getLocRange
  var newAst: AST[Node] = ast
  while(GlobalVariables.LOC < wantedLoc){
    val (newContextGen,newSymTab) = contextGenerator.addClass(clientRpc,contextGenerated)
    newAst = treeBuilder.addClass(grammarGraph,newContextGen,ast,newSymTab)
  }
  logger.info(".............Generated LOC are enough.........")

  println(newAst.getTreeRep(newAst))

  def disableWarning(): Unit = {
    System.err.close()
    System.setErr(System.out)
  }




}
