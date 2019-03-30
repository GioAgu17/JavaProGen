package progen.prolog

import io.grpc.netty.NettyChannelBuilder
import network.server.TuPrologHandler
import progen.peg.entities.{ConstructorSignature, MethodSignature}
import validation._

import scala.concurrent.Future
class ClientRpc {
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


  def addNodeToProlog(description: String, id: Int, par_id: Int, depth: Int): Future[Boolean] ={
    val message = Node(description,id,par_id,depth)
    val blockingStub = PrologValidationGrpc.blockingStub(channel)
    val response: Response = blockingStub.addFactNode(message)
    val result = Future.successful(response.value)
    result
  }
  def validateRetStmt(retType: String, par_id: Int, depth: Int): Boolean ={
    val message = UseRetStmt(retType,par_id,depth)
    val blockingStub = PrologValidationGrpc.blockingStub(channel)
    val response: Response = blockingStub.validateUseRetStmt(message)
    response.value
  }
  /**
    * Validates the inheritance between two classes
    */
  def validateExtension(class1: String,class2: String): Boolean = {

    val message = Extend(class1,class2)
    val blockingStub = PrologValidationGrpc.blockingStub(channel)
    val response: Response = blockingStub.validateExtension(message)
    response.value
  }
  def addType(typeName: String, id: Int):Boolean ={
    val message = Type(typeName,id)
    val blockingStub = PrologValidationGrpc.blockingStub(channel)
    val response: Response = blockingStub.addType(message)
    response.value
  }

  def getType(typeName: String, id: Int): String ={
    val request = Type(typeName,id)
    val blockingStub = PrologValidationGrpc.blockingStub(channel)
    val response: TypeResponse = blockingStub.getType(request)
    response.`type`
  }

  def validateSuperInvocation(args: List[String],parId: Int, depth:Int): Boolean ={
    val prologListForArgs = PrologConverter.argsConversion(args)
    val message = SuperInvocation(prologListForArgs,parId,depth)
    val blockingStub = PrologValidationGrpc.blockingStub(channel)
    val response: Response = blockingStub.validateSuperInvocation(message)
    response.value
  }
  /**
    * validates the method signature generated
    */
  def validateMethodSignature(className: String, methodSign: MethodSignature):Boolean ={
    val name = methodSign.name
    val retType = methodSign.returnType
    val params = PrologConverter.paramsTypeConversion(methodSign.formalParameters)
    val message = MethodSign(name,className,retType,params,methodSign.id)
    val blockingStub = PrologValidationGrpc.blockingStub(channel)
    val response: Response = blockingStub.validateMethodSign(message)
    response.value
  }

  def validateConstrSignature(constrSign: ConstructorSignature): Boolean = {
    val name = constrSign.className
    val params = PrologConverter.paramsTypeConversion(constrSign.formalParameters)
    val message = ConstrSign(name,constrSign.id,params)
    val blockingStub = PrologValidationGrpc.blockingStub(channel)
    val response: Response = blockingStub.validateConstructorSign(message)
    response.value
  }
  def validateMethInvocation(name: String,className: String, args: List[String]):Boolean ={
    val prologListForArgs = PrologConverter.argsConversion(args)
    val message = MethodInvocation(name,className,prologListForArgs)
    val blockingStub = PrologValidationGrpc.blockingStub(channel)
    val response: Response = blockingStub.validateMethodInvocation(message)
    response.value
  }
  def validateClassCreation(className: String, args: List[String]): Boolean ={
    val prologListForArgs = PrologConverter.argsConversion(args)
    val message = ClassCreation(className,prologListForArgs)
    val blockingStub = PrologValidationGrpc.blockingStub(channel)
    val response: Response = blockingStub.validateClassInstanceCreation(message)
    response.value
  }
  def validateThisInvocation(args: List[String], parId: Int, depth: Int): Boolean ={
    val prologListForArgs = PrologConverter.argsConversion(args)
    val message = ThisInvocation(prologListForArgs,parId,depth)
    val blockingStub = PrologValidationGrpc.blockingStub(channel)
    val response: Response = blockingStub.validateThisInvocation(message)
    response.value
  }

  def validateVarInit(expected: String, actual: String): Boolean ={
    val message = VariableInitialization(expected,actual)
    val blockingStub = PrologValidationGrpc.blockingStub(channel)
    val response: Response = blockingStub.validateVariableInitialization(message)
    response.value
  }
}
