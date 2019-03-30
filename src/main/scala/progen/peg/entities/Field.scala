package progen.peg.entities

class Field (val stype: String, val name: String,val inherited: Boolean){
  override def toString: String = {
    val s = stype +" " + name + ";"
    if(inherited) {
      val r = s +"(inherited)"
      return r
    }
    s
  }
}
