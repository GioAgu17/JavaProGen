package progen.prolog

import io.grpc.netty.NettyChannelBuilder
import network.server.TuPrologHandler
import progen.peg.entities.{ConstructorSignature, MethodSignature}
import validation._

import scala.concurrent.Future
class ClientRpc(val useRPC: Boolean) {
  val channel =
    NettyChannelBuilder
      .forAddress("127.0.0.1", 50051)
      .usePlaintext(true)
      .build()
  /**
    * Set the Prolog rules in the Prolog engine
    * @param rules the rules from the file of the KB
    * @return true if everything is ok
    */
    def setRules(rules: List[String]): Boolean ={
          val rulesStr = rules.mkString
          TuPrologHandler.setPrologRules(rulesStr)
    }


  def addNodeToProlog(description: String, id: Int, par_id: Int, depth: Int): Future[Boolean] = synchronized({
    val message = Node(description, id, par_id, depth)
    if(useRPC) {
      val blockingStub = PrologValidationGrpc.blockingStub(channel)
      val response: Response = blockingStub.addFactNode(message)
      val result = Future.successful(response.value)
      result
    }else{
     val response =  TuPrologHandler.handleNodeRequest(message)
      val result = Future.successful(response)
      result
    }
  })

  def validateRetStmt(retType: String, par_id: Int, depth: Int): Boolean =synchronized({
    val message = UseRetStmt(retType, par_id, depth)
    if(useRPC) {
      val blockingStub = PrologValidationGrpc.blockingStub(channel)
      val response: Response = blockingStub.validateUseRetStmt(message)
      response.value
    }else{
      val response = TuPrologHandler.handleReturnStmt(message)
      response
    }
  })
  /**
    * Validates the inheritance between two classes
    */
  def validateExtension(class1: String,class2: String): Boolean = synchronized({

    val message = Extend(class1,class2)
    if(useRPC) {
      val blockingStub = PrologValidationGrpc.blockingStub(channel)
      val response: Response = blockingStub.validateExtension(message)
      response.value
    }else{
      val response = TuPrologHandler.handleExtendRequest(message)
      response
    }
  })


  def addType(typeName: String, id: Int):Boolean =synchronized({
    val message = Type(typeName,id)
    if(useRPC) {
      val blockingStub = PrologValidationGrpc.blockingStub(channel)
      val response: Response = blockingStub.addType(message)
      response.value
    }else{
      val response = TuPrologHandler.addType(message)
      response
    }
  })

  def getType(typeName: String, id: Int): String =synchronized({
    val request = Type(typeName,id)
    if(useRPC) {
      val blockingStub = PrologValidationGrpc.blockingStub(channel)
      val response: TypeResponse = blockingStub.getType(request)
      response.`type`
    }else{
      val response = TuPrologHandler.getType(request)
      response
    }
  })

  def validateSuperInvocation(args: List[String],parId: Int, depth:Int): Boolean = synchronized({
    val prologListForArgs = PrologConverter.argsConversion(args)
    val message = SuperInvocation(prologListForArgs,parId,depth)
    if(useRPC) {
      val blockingStub = PrologValidationGrpc.blockingStub(channel)
      val response: Response = blockingStub.validateSuperInvocation(message)
      response.value
    }else{
      val response = TuPrologHandler.handleSuperInvocation(message)
      response
    }
  })
  /**
    * validates the method signature generated
    */
  def validateMethodSignature(className: String, methodSign: MethodSignature):Boolean =synchronized({
    val name = methodSign.name
    val retType = methodSign.returnType
    val params = PrologConverter.paramsTypeConversion(methodSign.formalParameters)
    val message = MethodSign(name,className,retType,params,methodSign.id)
    if(useRPC) {
      val blockingStub = PrologValidationGrpc.blockingStub(channel)
      val response: Response = blockingStub.validateMethodSign(message)
      response.value
    }else{
      val response = TuPrologHandler.handleMethodSign(message)
      response
    }
  })

  def validateConstrSignature(constrSign: ConstructorSignature): Boolean = synchronized( {
    val name = constrSign.className
    val params = PrologConverter.paramsTypeConversion(constrSign.formalParameters)
    val message = ConstrSign(name,constrSign.id,params)
    if(useRPC) {
      val blockingStub = PrologValidationGrpc.blockingStub(channel)
      val response: Response = blockingStub.validateConstructorSign(message)
      response.value
    }else{
      val response = TuPrologHandler.handleConstrSign(message)
      response
    }
  })
  def validateMethInvocation(name: String,className: String, args: List[String]):Boolean =synchronized({
    val prologListForArgs = PrologConverter.argsConversion(args)
    val message = MethodInvocation(name,className,prologListForArgs)
    if(useRPC) {
      val blockingStub = PrologValidationGrpc.blockingStub(channel)
      val response: Response = blockingStub.validateMethodInvocation(message)
      response.value
    }else{
      val response = TuPrologHandler.handleMethodInvocation(message)
      response
    }
  })
  def validateClassCreation(className: String, args: List[String]): Boolean =synchronized({
    val prologListForArgs = PrologConverter.argsConversion(args)
    val message = ClassCreation(className,prologListForArgs)
    if(useRPC) {
      val blockingStub = PrologValidationGrpc.blockingStub(channel)
      val response: Response = blockingStub.validateClassInstanceCreation(message)
      response.value
    }else{
      val response = TuPrologHandler.handleClassCreation(message)
      response
    }
  })
  def validateThisInvocation(args: List[String], parId: Int, depth: Int): Boolean = synchronized({
    val prologListForArgs = PrologConverter.argsConversion(args)
    val message = ThisInvocation(prologListForArgs,parId,depth)
    if(useRPC) {
      val blockingStub = PrologValidationGrpc.blockingStub(channel)
      val response: Response = blockingStub.validateThisInvocation(message)
      response.value
    }else{
      val response = TuPrologHandler.handleThisInvocation(message)
      response
    }
  })

  def validateVarInit(expected: String, actual: String): Boolean =synchronized({
    val message = VariableInitialization(expected,actual)
    if(useRPC) {
      val blockingStub = PrologValidationGrpc.blockingStub(channel)
      val response: Response = blockingStub.validateVariableInitialization(message)
      response.value
    }else{
      val response = TuPrologHandler.handleVarInitialization(message)
      response
    }
  })
}
