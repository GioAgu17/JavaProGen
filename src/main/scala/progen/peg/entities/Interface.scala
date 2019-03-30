package progen.peg.entities

class Interface(val name: String,val methods: List[ MethodSignature]) {
  override def toString: String = {
    "interface "+ name +" { \n "+ methods.map(_.toString).mkString("\n") + "\n}"
  }
}
