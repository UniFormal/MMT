package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.ontology.FormalAlignment
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.api.{GlobalName, LocalName, MPath}

import scala.collection.mutable
import scala.util.Try

case class Consthash(name:GlobalName, hash: List[Any], pars: List[GlobalName],
                     isProp: Boolean, optDef : Option[(Int,List[GlobalName])]) {

  //def matches(l:List[(GlobalName,GlobalName)])(that:Consthash):Boolean = newPairs(l)(that).isEmpty
  override def toString = name.toString + "[" + pars.map(_.name.toString).mkString(", ") + "]" +
    optDef.map(p => " = [" + p._2.map(_.name.toString).mkString(", ") + "]").getOrElse("")

  def <>(that : Consthash) = this.hash == that.hash && this.pars.length == that.pars.length
  def !<>(that :Consthash) = !(this <> that)
}

class Theoryhash(val path:MPath) {
  private var consts : List[Consthash] = Nil
  private var includes : List[Theoryhash] = Nil
  private var allincludes : List[Theoryhash] = Nil
  private var allconsts : List[Consthash] = Nil

  def getLocal = consts
  def getAll = allconsts
  def getincludes = includes
  def getAllIncludes = allincludes

  def addConstant(c : Consthash) = consts::=c
  def addInclude(th : Theoryhash) = includes::=th
  def init: Unit = {
    allincludes = (includes:::includes.flatMap(_.getAllIncludes)).distinct
    allconsts = consts ::: allincludes.flatMap(_.getLocal)
  }

  def <(that : Theoryhash) = that.getAllIncludes contains this

  private def toStringIndent(ind:String) : String =
    ind + path.name.toString +
      includes.map(x => "\n" + x.toStringIndent(ind + "  ")).mkString("\n") +
      consts.map(p => "\n  " + ind + p.toString).mkString("")

  override def toString = toStringIndent("")

}
