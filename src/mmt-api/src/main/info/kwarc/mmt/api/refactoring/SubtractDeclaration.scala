package info.kwarc.mmt.api.refactoring
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.api.{GlobalName, LocalName}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.modules.Theory.{noBase, noParams}

/**
 * Subtracts a Declaration from a view
 */
object SubtractDeclaration {

  def getConstants(th:Theory,a:List[GlobalName],ctrl:Controller) : List[FinalConstant] = {
    val includes = th.getIncludes.map(ctrl.get(_)).toSet-th
    val consts = th.getConstants collect {case t:FinalConstant => t}

    val inclnames = includes.foldLeft(Set().asInstanceOf[Set[GlobalName]])((arg,thx) =>
      (thx.getDeclarations collect {case t:FinalConstant => t.path}).toSet++arg
    ).toList

    //val judg = AxiomHandler.findJudgment(ctrl,th,Some(includes+th))
    val hashes = for {o <- consts} yield Consthash(o.path, List(), List(), false, None)//TODO isProp
    val filteredindic = hashes.indices.filter(i => !a.exists(b => occursIn(b,hashes(i),hashes)))

    filteredindic.map(consts(_)).toList
  }

  /**
   * Takes a DeclaredTheory
   * @param th and "deletes" every Declaration that depends on GlobalName/LocalName/FinalConstant
   * @param a  (must be declared in th). Returns a theory with LocalName
   * @param name if given, otherwise th.name*.
   */
  def apply(th:Theory,a:List[GlobalName],ctrl:Controller,name:Option[LocalName]) : Theory = {


    val newname = name match {
      case Some(x) => x
      case None => LocalName(th.name.toString()+"*")
    }

    val newth = new Theory(th.path.doc,newname,th.meta, noParams, noBase)
    Moduleadder(newth,getConstants(th,a,ctrl))

    newth
  }

  def applyln(th:Theory,a:List[LocalName],ctrl:Controller,name:Option[LocalName]) : Theory
    = apply(th,a.map(b => th.get(b) match {case n:FinalConstant => n.path case _ => throw new Exception("Final Constant Expected!")}),
    ctrl,name)

  def applyfc(th:Theory,a:List[FinalConstant],ctrl:Controller,name:Option[LocalName]) : Theory
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
