package progen.peg.entities

class Class(val name: String, val interfaces: Option[List[Interface]],val fields: List[Field], val constructors: List[ConstructorSignature],val methods: List[MethodSignature],val superClass: Option[String]){
  override def toString: String = {

    val n = superClass match{
      case Some(s) => " extends "+s
      case None => " "
    }
    val r = interfaces match{
      case Some(i) => " implements "+i.map(interf => interf.name).mkString(", ") + " {\n"
      case None => " {\n"
    }
    val res = "class "+name + n + r + "\t"+fields.mkString("\n\t") + "\n\t"+ constructors.mkString("\n\t")+"\n\t"+methods.mkString("\n\t") + "\n" + "}"
    res
  }
}
