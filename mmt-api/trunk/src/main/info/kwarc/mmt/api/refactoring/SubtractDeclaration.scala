package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api.libraries.Closer
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.api.{GlobalName, LocalName}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.DeclaredTheory

/**
 * Subtracts a Declaration from a view
 */
object SubtractDeclaration {

  def getConstants(th:DeclaredTheory,a:List[GlobalName],ctrl:Controller) : Set[FinalConstant] = {
    val closer = new Closer(ctrl)
    val includes = closer.getIncludes(th,true)-th
    val consts = th.getConstants collect {case t:FinalConstant => t}

    val inclnames = includes.foldLeft(Set().asInstanceOf[Set[GlobalName]])((arg,thx) =>
      (thx.getConstants collect {case t:FinalConstant => t.path}).toSet++arg
    ).toList

    val judg = AxiomHandler.findJudgment(ctrl,th,Some(includes+th))
    val hashes = for {o <- consts} yield Consthash(o,inclnames,judg)
    val filteredindic = hashes.indices.filter(i => !a.exists(b => occursIn(b,hashes(i),hashes)))

    filteredindic.map(consts(_)).toSet
  }

  /**
   * Takes a DeclaredTheory
   * @param th and "deletes" every Declaration that depends on GlobalName/LocalName/FinalConstant
   * @param a  (must be declared in th). Returns a theory with LocalName
   * @param name if given, otherwise th.name*.
   */
  def apply(th:DeclaredTheory,a:List[GlobalName],ctrl:Controller,name:Option[LocalName]) : DeclaredTheory = {


    val newname = name match {
      case Some(x) => x
      case None => LocalName(th.name.toString()+"*")
    }

    val newth = new DeclaredTheory(th.path.doc,newname,th.meta)
    Moduleadder(newth,getConstants(th,a,ctrl))

    newth
  }

  def applyln(th:DeclaredTheory,a:List[LocalName],ctrl:Controller,name:Option[LocalName]) : DeclaredTheory
    = apply(th,a.map(b => th.get(b) match {case n:FinalConstant => n.path case _ => throw new Exception("Final Constant Expected!")}),
    ctrl,name)

  def applyfc(th:DeclaredTheory,a:List[FinalConstant],ctrl:Controller,name:Option[LocalName]) : DeclaredTheory
    = apply(th,a.map(_.path),ctrl,name)

  /**
   * Checks, whether a declaration (as Consthash)
   * @param b depends (recursively) on declaration
   * @param a (as GlobalName); using List
   * @param l for recursive checks.
   */

  def occursIn(a:GlobalName,b:Consthash,l:List[Consthash]):Boolean = {
    if (b.pars.contains(a) || b.name==a) true
    else if (b.pars.isEmpty) false
    else b.pars.exists(par => l.collectFirst{case hash if hash.name==par => hash} match {
      case None => false
      case Some(x) => occursIn(a,x,l)}
    )
  }
}
