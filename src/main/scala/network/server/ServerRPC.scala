package network.server

import grizzled.slf4j.Logger
import io.grpc.{Server, ServerBuilder}
import validation._

import scala.concurrent.{ExecutionContext, Future}


object ServerRPC {

  private val logger = Logger[this.type]
  def start(): Future[Unit]= Future{
    val server = new ServerRPC(ExecutionContext.global)
    server.start()
    server.blockUntilShutdown()
  }(ExecutionContext.global)
  private val port = 50051
}

class ServerRPC(executionContext: ExecutionContext) { self =>
  private[this] var server: Server = null

  private def start(): Unit = {
    server = ServerBuilder.forPort(ServerRPC.port).addService(PrologValidationGrpc.bindService(new PrologValidationService, executionContext)).build.start
    ServerRPC.logger.info("Server started, listening on " + ServerRPC.port)
    sys.addShutdownHook {
      ServerRPC.logger.info("*** shutting down gRPC server since JVM is shutting down")
      self.stop()
      ServerRPC.logger.info("*** server shut down")
    }
  }

  private def stop(): Unit = {
    if (server != null) {
      server.shutdown()
    }
  }

  private def blockUntilShutdown(): Unit = {
    if (server != null) {
      server.awaitTermination()
    }
  }


  /**
    * Class which uses the PrologValidation Service defined in the validation.proto file
    */
  class PrologValidationService extends PrologValidationGrpc.PrologValidation{
    /**
      * Pass the Request instance received by the protobuffer to the TuPrologHandler
      * to ask the Knowledge Base
      * @param request an instance of Request from the Client
      * @return the response of the Prolog Knowledge Base
      */
    override def addFactNode(request: Node): Future[Response] = synchronized{
      val res = TuPrologHandler.handleNodeRequest(request)
      val response = Response(res)
      Future.successful(response)
    }

    override def validateConstructorSign(request: ConstrSign): Future[Response] = synchronized{
      val res = TuPrologHandler.handleConstrSign(request)
      val response = Response(res)
      Future.successful(response)
    }

    override def validateExtension(request: Extend): Future[Response] = synchronized{
      val res = TuPrologHandler.handleExtendRequest(request)
      val response = Response(res)
      Future.successful(response)
    }

    override def validateMethodSign(request: MethodSign): Future[Response] = synchronized{
      val res = TuPrologHandler.handleMethodSign(request)
      val response = Response(res)
      Future.successful(response)
    }

    override def validateRetStmt(request: RetStmt): Future[Response] = ???

    override def validateThisInvocation(request: ThisInvocation): Future[Response] = synchronized{
      val res = TuPrologHandler.handleThisInvocation(request)
      val response = Response(res)
      Future.successful(response)
    }
    override def validateMethodInvocation(req: MethodInvocation): Future[Response] =synchronized{
      val res = TuPrologHandler.handleMethodInvocation(req)
      val response = Response(res)
      Future.successful(response)
    }

    override def validateClassInstanceCreation(request: ClassCreation): Future[Response] = synchronized {
      val res = TuPrologHandler.handleClassCreation(request)
      val response = Response(res)
      Future.successful(response)
    }
    override def validateUseRetStmt(request: UseRetStmt): Future[Response] = synchronized{
      val res = TuPrologHandler.handleReturnStmt(request)
      val response = Response(res)
      Future.successful(response)
    }

    override def addType(typeName: Type): Future[Response] =synchronized {
      val res = TuPrologHandler.addType(typeName)
      val response = Response(res)
      Future.successful(response)
    }

    override def validateSuperInvocation(request: SuperInvocation): Future[Response] = synchronized {
      val res = TuPrologHandler.handleSuperInvocation(request)
      val response = Response(res)
      Future.successful(response)
    }
    override def getType(request: Type): Future[TypeResponse] = synchronized{
      val res = TuPrologHandler.getType(request)
      val response = TypeResponse(res)
      Future.successful(response)
    }

    override def validateVariableInitialization(request: VariableInitialization): Future[Response] = {
      val res = TuPrologHandler.handleVarInitialization(request)
      val response = Response(res)
      Future.successful(response)
    }
  }
}
