package progen.grammartraverser.utils

import com.mifmif.common.regex.Generex
import progen.grammarparser.Node
import progen.grammartraverser.AST
import progen.peg.entities.GlobalTable
import progen.symtab.{LocalVariable, SymTab, SymTabEntryKind, TypeHandler}

import scala.util.Random

object IdentifierHandler {

  def handleIdentifier(tree: AST[Node], accessible: SymTab, context: (GlobalTable,List[SymTab]), previousList: List[String]): List[String]= tree.node.description match{
    case "<classtype>" => context._1.classNames
    case "<interfacetype>" => context._1.interfaces.map(_.name)
    case "<primitivetype>" => context._1.primitiveTypes.filter(_ != "void")
    case "<methodname>" => retMethodNames(accessible)
    case "<argumentlist>" =>
      tree.parent match{
        case Some(p) => p.node.description match{
          case "<methodinvocation>" => retAllNames(accessible)
          case "<classinstancecreationexpression>" => retAllNames(accessible)
          case "this ( <argument list>? ) ; " => retLocVarsAndParams(accessible)
          case " super ( <argument list>? ) ;" => retLocVarsAndParams(accessible)
          case _ => previousList
        }
        case None => previousList
      }
    case "<constantexpression>" =>
      List(randomConstantExpression(TypeHandler.nodeType))

    case "<fieldaccess>" => accessible.lookUpFields match{
        case Some(fields) => fields.map(_._2)
        case None => List()
      }

    case "<variabledeclaratorid>" =>
      val localVars = accessible.lookUpLocVars(accessible,List())
      // looking whether the variable declaration will be initialized or not
      val parent = tree.parent match{
        case Some(p) => p
        case None => sys.error("<variabledeclaratorid> node has no parent")
      }
      val initialized = parent.node.description match{
        case "<variable declarator id> " => false
        case " <variable declarator id> = <variable initializer>" => true
        case _ => sys.error("<variabledeclaratorid> has a wrong parent")
      }
      List(generateLocVar(localVars,accessible,initialized))
    case "<methodinvocation>" => retMethodNames(accessible)
    case "<classinstancecreationexpression>" => context._1.classNames
    case _ => previousList

  }

  def retMethodNames(accessible: SymTab): List[String] = accessible.lookUpMethods match{
    case Some(ms) => ms.map(_.name)
    case None => List()
  }
  def generateLocVar(locVars:List[LocalVariable],tab:SymTab, initialized: Boolean): String ={
    val existingNames = locVars.map(_.name)
    val generex = new Generex("[a-z]")
    val pickedUp = generex.random
    if(existingNames.contains(pickedUp))
      generateLocVar(locVars,tab,initialized)
    else{
      tab.insertLocVar(TypeHandler.nodeType,pickedUp,initialized)
      pickedUp
    }
  }

  def retAllNames(accessible: SymTab): List[String] ={
    val classFields = accessible.lookUpFields match{
      case Some(fields) => fields.map(_._2)
      case None => List()
    }
    retLocVarsAndParams(accessible) ++ classFields
  }

  def retLocVarsAndParams(accessible: SymTab): List[String]={
    val locVars = accessible.lookUpLocVars(accessible,List()).filter(_.initialized).map(_.name)
    val methSymTab = accessible.visitSymTab(SymTabEntryKind.METHOD,accessible)
    val constrSymTab = accessible.visitSymTab(SymTabEntryKind.CONSTRUCTOR,accessible)


    val paramsFromMeth = methSymTab match{
      case Some(st) => st.symTabEntry.params match{
        case Some(ps) => ps.map(_._2)
        case None => List()
      }
      case None => List()
    }
    val paramsFromConstr = constrSymTab match{
      case Some(st) => st.symTabEntry.params match{
        case Some(ps) => ps.map(_._2)
        case None => List()
      }
      case None => List()
    }

    (locVars ++ paramsFromConstr ++ paramsFromMeth).distinct
  }
  def randomConstantExpression(primitiveType: String): String = primitiveType match{
    case "int" =>
      Random.nextInt(100).toString
    case "char" =>
      "'"+Random.nextPrintableChar().toString+"'"
    case "short" =>
      "(short)"+Random.nextInt(100).toShort.toString
    case "boolean" =>
      Random.nextBoolean().toString
    case "long" =>
      Random.nextInt(10000).toString
    case "float" =>
      Random.nextFloat().toString + "f"
    case "double" =>
      Random.nextDouble().toString
    case "byte" =>
      "(byte)0"
    case _ =>
      "0"
  }



}
