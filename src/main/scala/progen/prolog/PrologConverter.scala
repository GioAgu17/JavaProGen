package progen.prolog

object PrologConverter {
  // converts a list of parameters of a MethodSignature object into a list in Prolog style of parameter types
  def paramsTypeConversion(parameters: List[(String,String)]): String ={
    val types = parameters.map(p => p._1)
    val res = "['" + types.mkString("','") + "']"
    res
  }
  def argsConversion(arguments: List[String]): String ={
    "['"+arguments.mkString("','")+"']"
  }

}
