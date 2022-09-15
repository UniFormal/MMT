package info.kwarc.mmt.stex.parsing

import info.kwarc.mmt.api.Level
import info.kwarc.mmt.api.Level.Level
import info.kwarc.mmt.stex.STeXError

import scala.collection.mutable

trait TeXTokenLike {
  val startoffset : Int
  val endoffset: Int
  def iterate(f : TeXTokenLike => Unit) = f(this)
  val attributes = mutable.HashMap.empty[String,Any]
  var errors : List[STeXError] = Nil
  def addError(msg : String,extramsg : Option[String] = None,lvl : Level = Level.Error) = {
    errors ::= new STeXError(msg, extramsg, Some(lvl))
  }
  def =?=(that : TeXTokenLike) : Boolean = false
}

class PlainText(val str : String,val startoffset:Int,val endoffset:Int) extends TeXTokenLike {
  override def toString: String = str
}

class PlainMacro(val name:String, val startoffset:Int, val endoffset:Int) extends TeXTokenLike {
  override def toString: String = "\\" + name + " "
}

class MacroApplication(val plain:PlainMacro,val children:List[TeXTokenLike],val rule:TeXRule) extends TeXTokenLike {
  val startoffset = plain.startoffset
  val endoffset = children.lastOption.map(_.endoffset).getOrElse(plain.endoffset)

  override def iterate(f: TeXTokenLike => Unit): Unit = {
    f(this)
    children.foreach(_.iterate(f))
  }

  override def toString: String = "Macro[" + plain.toString + ", " + children.mkString + "]"
}

class SimpleMacroApplication(plain:PlainMacro,children:List[TeXTokenLike],val starred:Boolean,val args:List[TeXTokenLike], rule:TeXRule) extends MacroApplication(plain,children,rule)


class Group(val content:List[TeXTokenLike],val startoffset:Int,val endoffset:Int) extends TeXTokenLike {
  override def iterate(f: TeXTokenLike => Unit): Unit = {
    f(this)
    content.foreach(_.iterate(f))
  }
  override def toString: String = "{" + content.mkString + "}"
}

case class Comment(str : String,startoffset:Int,endoffset:Int) extends TeXTokenLike {
  override def toString: String = "%" + str
}

case class Math(content:List[TeXTokenLike],startdelim:TeXTokenLike,enddelim:TeXTokenLike) extends TeXTokenLike {
  val startoffset = startdelim.startoffset
  val endoffset = enddelim.endoffset

  override def toString: String = startdelim.toString + content.mkString + enddelim.toString

  override def iterate(f: TeXTokenLike => Unit): Unit = {
    f(this)
    startdelim.iterate(f)
    content.foreach(_.iterate(f))
    enddelim.iterate(f)
  }
}

class Environment(val begin:MacroApplication,val end:TeXTokenLike,val children:List[TeXTokenLike],val rule:Option[EnvironmentRule]) extends TeXTokenLike {
  val startoffset = begin.startoffset
  val endoffset = end.endoffset
  var beginname : Option[TeXTokenLike] = None
  var endname : Option[TeXTokenLike] = None

  override def toString: String = begin.toString + children.mkString + end.toString
  override def iterate(f: TeXTokenLike => Unit): Unit = {
    f(this)
    begin.iterate(f)
    children.foreach(_.iterate(f))
    end.iterate(f)
  }
}