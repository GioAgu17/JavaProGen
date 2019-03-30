package progen.symtab

import progen.grammarparser.Node
import progen.peg.entities.{ConstructorSignature, MethodSignature}
import SymTabEntryKind.SymTabEntryKind

import scala.annotation.tailrec


trait SymTab{
  val symTabEntry: SymTabEntry
  val prev: Option[SymTab]
  var next: Option[List[SymTab]]
  def enterScope(tab:SymTab,node: Node) : SymTab
  def leaveScope(tab:SymTab,node: Node): SymTab
  def visitSymTab(kind: SymTabEntryKind, sm: SymTab) : Option[SymTab]
  def lookUpFields: Option[List[(String,String)]]
  def lookUpConstructors(name: String): Option[List[ConstructorSignature]]
  def lookUpMethods: Option[List[MethodSignature]]
  def getRetType(tab: SymTab): Option[String]
  def lookUpType(name:String): Option[String]
  def lookUpClassTab(name: String, tab: SymTab): Option[SymTab]
  def insertLocVar(sType:String,name:String,initialized: Boolean): SymTab
  def lookUpLocVars(tab:SymTab,acc:List[LocalVariable]): List[LocalVariable]
}

object SymTab {

  case class SymTabSimple(override val symTabEntry: SymTabEntry, override val prev: Option[SymTab],override var next: Option[List[SymTab]]) extends SymTab  {

    override def enterScope(tab: SymTab,node: Node): SymTab ={
      if(node.description == "<block>") {
        val symTabEntry = new SymTabEntry(SymTabEntryKind.BLOCK, None, None, None, None, None, None, None, None, None)
        val newTab = SymTabSimple(symTabEntry, Option(tab), None)
        tab.next match {
          case Some(l) => tab.next = Option(newTab :: l)
          case None => tab.next = Option(List(newTab))
        }
        newTab
      }else
        tab
    }

    override def leaveScope(symTab: SymTab,node: Node): SymTab ={
      if(node.description == "<block>") {
       symTab.prev match {
          case Some(t) => t
          case None => sys.error("cannot leave scope because this is the outermost scope")
        }
      }else
        symTab
    }

    @tailrec
    override final def visitSymTab(kind: SymTabEntryKind, sm: SymTab): Option[SymTab] =sm.symTabEntry.kind match{
      case `kind` => Option(sm)
      case _ => sm.prev match{
        case Some(s) => visitSymTab(kind,s)
        case None => None
      }

    }

    override def lookUpFields: Option[List[(String, String)]] ={

      val classSymTab = visitSymTab(SymTabEntryKind.CLASS,this)
      val fields = classSymTab match{
        case Some(st) => st.symTabEntry.fields match{
          case Some(fs) => Option(fs.map(field => (field.stype,field.name)))
          case None => None
        }
        case None => None
      }
      fields
    }

    /**
      * looks up all the methods of the class that is enclosing the current scope
      * @return a list of method signatures
      */
    override def lookUpMethods: Option[List[MethodSignature]] = {
      val classSymTab =  visitSymTab(SymTabEntryKind.CLASS,this)
      val methods = classSymTab match{
        case Some(st) => st.symTabEntry.methods
        case None => None
      }
      methods
    }

    // calls @lookPreviousScopesForType for finding the type of @name
    override def lookUpType(name: String): Option[String] ={
      val possibleType = lookPreviousScopesForType(this,name)
      possibleType
    }

    /**
      * Looks up in the scopes all the constructors of a given class
      * @param className the name of the class
      * @return the list of constructor signatures
      */
    override def lookUpConstructors(className: String): Option[List[ConstructorSignature]]={
      val targetClassTab = lookUpClassTab(className,this)
      targetClassTab match{
        case Some(c) => c.symTabEntry.constructors
        case None => None
      }
    }
    override def lookUpClassTab(name: String, tab: SymTab): Option[SymTab] = {
      val symTabCU = visitSymTab(SymTabEntryKind.COMPILATIONUNIT,tab)
      symTabCU match{
        case Some(t) => t.next match{
          case Some(classes) => classes.find(c => c.symTabEntry.name match{
            case Some(n) => n == name
            case None => false
          })
          case None => sys.error("tab of compilation unit does not have class tabs as next attributes")
        }
        case None => sys.error("tab of compilation unit not found")
      }
    }

    /**
      * Given a name of an expression (i.e. local variable, parameter or field), look at its declared type
      * by searching from the innermost scope to the outermost
      * @param symTab the scope in which the search starts
      * @param name the name of the expression
      * @return the declared type of the expression
      */
    def lookPreviousScopesForType(symTab: SymTab, name:String): Option[String] = symTab.symTabEntry.kind match{
      case SymTabEntryKind.CLASS =>
        val fields = symTab.symTabEntry.fields match{
          case Some(f) =>
            val check = f.map(fi => (fi.stype,fi.name)).filter(c => c._2 == name)
            if(check.isEmpty)
              None
            else
              Option(check.head._1)
          case None => None
        }
        fields
      case _ =>
        val locVars = symTab.symTabEntry.locVars match{
          case Some(lv) => lv
          case None => List()
        }
        val params = symTab.symTabEntry.params match{
          case Some(pv) => pv
          case None => List()
        }
        val localVars = locVars.map(lv => (lv.sType,lv.name))
        val locVarsAndParams = localVars ++ params
        val check = locVarsAndParams.filter(c => c._2 == name)

        if(check.isEmpty || locVarsAndParams.isEmpty){
          symTab.prev match{
            case Some(p) => lookPreviousScopesForType(p,name)
            case None => None
          }
        }else{
          val res = Option(check.head._1)
          res
        }

    }
    override def getRetType(tab: SymTab): Option[String] ={
      val retType = tab.visitSymTab(SymTabEntryKind.METHOD,tab) match{
        case Some(t) => t.symTabEntry.retType
        case None => sys.error("cannot find a sym tab of kind METHOD in return statement")
      }
      retType
    }
    override def insertLocVar(sType:String,name:String,initialized: Boolean):SymTab ={
      val locVar = new LocalVariable(sType,name,initialized)
      val sEntry = this.symTabEntry
      sEntry.locVars match{
        case Some(list) => sEntry.locVars = Option(locVar::list)
        case None => sEntry.locVars = Option(List(locVar))
      }
      this
    }
    override def lookUpLocVars(tab:SymTab,acc: List[LocalVariable]): List[LocalVariable] ={
      tab.symTabEntry.kind match{
        case SymTabEntryKind.BLOCK =>
          tab.prev match{
            case Some(p) =>
              val locVars = tab.symTabEntry.locVars match{
                case Some(l) => l
                case None => List()
              }
              lookUpLocVars(p,acc++locVars)
            case None =>
              sys.error("block has no previous scope, impossible")
          }
        case SymTabEntryKind.METHOD =>
          tab.symTabEntry.locVars match{
            case Some(l) => l  ++ acc
            case None => acc
          }
        case SymTabEntryKind.CONSTRUCTOR =>
          tab.symTabEntry.locVars match{
            case Some(l) => l  ++ acc
            case None => acc
          }
        case _ =>
          sys.error("scope different than block and method found in lookUpLocVars")
      }
    }
  }

}
