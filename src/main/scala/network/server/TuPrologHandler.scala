package network.server

import java.io.{File, PrintWriter}

import alice.tuprolog.{InvalidTheoryException, NoSolutionException, Prolog, Theory}
import grizzled.slf4j.Logging
import validation._


object TuPrologHandler extends Logging {




  //starts a new Prolog engine
  val engine = new Prolog()

  // puts the knowledge base into the Prolog engine as a theory

  def setPrologRules(rules: String):Boolean ={
    try {
      val theory = new Theory(rules)
      engine.setTheory(theory)
      true
    }catch{
      case ite: InvalidTheoryException => sys.error("invalid theory exception")
      case e: Exception => sys.error("unknown exception occured in method setPrologRules of class "+this.getClass.getCanonicalName)
    }
  }
  /**
    * Handles the result of the Prolog Knowledge Base: if the response is true, it adds a fact to the KB. Otherwise,
    * it simply returns the response
    * @param request a Request instance received by the Protobuf protocol
    * @return true if the Prolog validates the request, false otherwise
    */
  def handleNodeRequest(request: Node): Boolean ={
    try{
      engine.addTheory(createNodeFact(request))
      true
    }
    catch{
      case inv: InvalidTheoryException => sys.error("invalid theory exception for node "+request.description)
    }
  }
  def handleVarInitialization(request: VariableInitialization): Boolean = askVarInitProlog(request)

  def handleReturnStmt(request: UseRetStmt): Boolean ={
    askReturnStmtProlog(request)
  }
  def addType(typeName: Type): Boolean ={
    try {
      engine.addTheory(createTypeFact(typeName))
      true
    }catch{
      case inv: InvalidTheoryException => sys.error("invalid theory exception for type "+typeName.`type`)
    }
  }
  def handleExtendRequest(request: Extend) : Boolean = askExtendProlog(request) match{
    case true => {
      engine.addTheory(createExtendFact(request))
      true
    }
    case false => false
  }
  def handleConstrSign(request: ConstrSign): Boolean = askConstrProlog(request) match{
    case true =>{
      engine.addTheory(createConstrFact(request))
      true
    }
    case false => false
  }
  def handleMethodSign(request: MethodSign): Boolean = askMethodProlog(request) match{
    case true => {
      engine.addTheory(createMethodFact(request))
      true
    }
    case false => false
  }
  def handleMethodInvocation(request: MethodInvocation): Boolean={
    val query = createMethInvocationQuery(request)
    val response = engine.solve(query)
    response.isSuccess
  }
  def handleClassCreation(classCreation: ClassCreation): Boolean ={
    val query = createClassCreationQuery(classCreation)
    val res = engine.solve(query)
    res.isSuccess
  }
  def getType(req: Type): String ={
    try {
      val info = engine.solve(createTypeQuery(req))
      val typeName = info.getVarValue(req.`type`)
      typeName.toString
    }catch{
      case ecc: Exception => "void"
    }
  }
  def handleThisInvocation(request: ThisInvocation): Boolean = askThisRequest(request)

  def handleSuperInvocation(request: SuperInvocation): Boolean = askSuperRequest(request)

  def askVarInitProlog(value: VariableInitialization): Boolean ={
    val query = createVarInitQuery(value)
    val res = engine.solve(query)
    res.isSuccess
  }
  def askConstrProlog(sign: ConstrSign): Boolean ={
    val query = createConstrQuery(sign)
    val res  = engine.solve(query)
    res.isSuccess
  }

  def askReturnStmtProlog(stmt: UseRetStmt): Boolean ={
    val query = createReturnStmtQuery(stmt)
    val res = engine.solve(query)
    res.isSuccess
  }
  def askMethodProlog(methodSign: MethodSign): Boolean ={
    val query = createMethodQuery(methodSign)
    val res = engine.solve(query)
    res.isSuccess
  }

  def askExtendProlog(extend: Extend): Boolean ={
    val query = createExtendQuery(extend)
    val res = engine.solve(query)
    res.isSuccess
  }
  def askThisRequest(invocation: ThisInvocation): Boolean ={
    val query = "useThisInvocation("+invocation.paramLst+","+invocation.id+","+invocation.depth+")."
    //logQuery(query)
    val res = engine.solve(query)
    res.isSuccess
  }
  def askSuperRequest(invocation: SuperInvocation): Boolean ={
    val query = "useSuperInvocation("+invocation.paramLst+","+invocation.parId+","+invocation.depth+")."
        //logQuery(query)
    val res = engine.solve(query)
    res.isSuccess
  }
  def createReturnStmtQuery(stmt: UseRetStmt): String ={
    val query = "useRetStmt('"+stmt.retType+"',"+ stmt.id+","+stmt.depth+")."
    //logQuery(query)
    query
  }
  def createVarInitQuery(value: VariableInitialization): String ={
    val expectedType = value.expectedType
    val actualType = value.actualType
    val query  = "canInitializeVar('"+expectedType+"','"+actualType+"')."
    //logQuery(query)
    query
  }
  def createConstrQuery(sign:ConstrSign): String ={
    val query = "canUseConstructor('"+sign.name+"',"+ sign.params+")."
    //logQuery(query)
    query

  }
  def createClassCreationQuery(creation: ClassCreation): String ={
    val query = "useClassInstanceCreation('"+creation._class+"',"+creation.args+")."
    // logQuery(query)
        query
  }
  def createMethInvocationQuery(invocation: MethodInvocation):String={
    val query = "useMethodInvocation('"+invocation.name+"','"+invocation._class+"',"+invocation.args+")."
    //logQuery(query)
    query
  }
  def createMethodQuery(sign: MethodSign): String ={
    val query = "useMethod('"+ sign.name+"','"+sign.className+"','"+sign.returnType+"',"+sign.paramLst+")."
    //logQuery(query)
    query
  }
  def createExtendQuery(extend: Extend): String ={
    val query = "canExtend('"+extend.typeName+"','"+ extend.superName + "')."
    //logQuery(query)
    query
  }
  def createTypeQuery(req: Type): String ={
    val query = "type("+req.`type`+","+req.id+")."
   // logQuery(query)
    query
  }
  def createTypeFact(typeName: Type): Theory ={
    val fact = "type('"+typeName.`type`+"',"+typeName.id+")."
    //logFact(fact)
    new Theory(fact)
  }
  def createConstrFact(sign: ConstrSign): Theory ={
    val fact = "constructor('"+sign.name+"',"+sign.id+","+sign.params+")."
    //logFact(fact)
    new Theory(fact)
  }

  def createMethodFact(sign: MethodSign): Theory ={
    val fact = "method('"+sign.name+"',"+sign.id+",'"+sign.className+"','"+sign.returnType+"',"+sign.paramLst+")."
   // logFact(fact)
    new Theory(fact)
  }
  def createExtendFact(extend: Extend): Theory ={
    val fact = "extends('"+extend.typeName+"','"+extend.superName+"')."
    //logFact(fact)
    new Theory(fact)
  }

  /**
    * Creates a fact from the request information and returns a theory to be added
    * @param req  a Request instance received by the Protobuf protocol
    * @return a Theory instance to be added to the engine
    */
  def createNodeFact(req: Node): Theory ={
    val fact = "node('" + req.description + "'," + req.id+ "," + req.parId+ ","+req.depth+")."
    //logFact(fact)
    new Theory(fact)
  }
  def getTheory:String = {
    engine.getTheory.toString
  }

  def logFact(fact: String):Unit ={
    val strBuilder = new StringBuilder
    strBuilder.append("created fact: ")
    strBuilder.append(fact)
    info(strBuilder.mkString)
  }
  def logQuery(query:String):Unit ={
    val strBuilder = new StringBuilder
    strBuilder.append("created query: ")
    strBuilder.append(query)
    info(strBuilder.mkString)
  }
}

