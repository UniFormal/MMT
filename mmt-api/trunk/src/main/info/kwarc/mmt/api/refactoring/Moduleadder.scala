package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api.{ComplexStep, SimpleStep, LocalName, GlobalName}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{DeclaredTheory, DeclaredView}
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.{FinalConstant, TermContainer}

/**
 * Takes a View/Theory T and a (Set of) (Pair(s) of) Constant(s) (either as FinalConstant of GlobalName) and 
 * adds them to T consistent with the specifications (i.e. adapts the GlobalName accordingly and so on).
 * Also, if handed a theory and a Set of Constants, recursively substitutes occurences of the other members of the
 * set in types/definitions
 */
object Moduleadder {

  def apply(v:DeclaredView,a:(GlobalName,GlobalName),ctrl:Controller):Boolean = {
    val pair = (ctrl.get(a._1), ctrl.get(a._2)) match {
      case (c1: FinalConstant, c2: FinalConstant) => Some(c1, c2)
      case _ => None
    }
    pair match {case None => false case Some(c) => apply(v, c)}
  }

  def apply(th: DeclaredTheory,a:GlobalName,ctrl:Controller):Boolean = {
    val const = ctrl.get(a) match {
      case c: FinalConstant => Some(c)
      case _ => None
    }
    const match {case None => false case Some(c) => apply(th, c)}
  }

  def apply(v:DeclaredView,a:(FinalConstant,FinalConstant)):Boolean = {
    v.add(new FinalConstant(
      OMID(v.path),
      ComplexStep(a._1.path.module.toMPath) / a._1.name,
      None,
      TermContainer(a._1.tp),
      TermContainer(OMID(a._2.path)),
      None,
      NotationContainer(None)))
    true
  }

  def apply(th: DeclaredTheory,a:FinalConstant):Boolean = {
    th.add(new FinalConstant(OMID(th.path), a.name, a.alias, TermContainer(a.tp), TermContainer(a.df), a.rl,
      NotationContainer(a.not)))
    true
  }

  def apply(v:DeclaredView,list:List[(FinalConstant,FinalConstant)]):Boolean = {
    for (a <- orderp(list)) apply(v,a)
    true
  }

  def apply(th:DeclaredTheory,set:List[FinalConstant],substs:List[(GlobalName,GlobalName)]):Boolean = {
    val list = set.toList
    val nsubsts = for {a <- list} yield (GlobalName(OMID(th.path),a.name),a.path)
    for (a <- order(list)) th.add(new FinalConstant(OMID(th.path), a.name, a.alias,
      TermContainer(substitute(a.tp,nsubsts:::substs)), TermContainer(substitute(a.df,nsubsts:::substs)), a.rl,
      NotationContainer(a.not)))
    true
  }

  def apply(th:DeclaredTheory,list:List[FinalConstant]):Boolean = apply(th,list,List())

  def apply(v:DeclaredView,list:List[(GlobalName,GlobalName)],ctrl:Controller):Boolean = {
    val pairs = for {a <- list} yield (ctrl.get(a._1), ctrl.get(a._2)) match {
      case (c1: FinalConstant, c2: FinalConstant) => Some(c1, c2)
      case _ => None
    }
    if (pairs.contains(None)) false
    else apply(v,pairs.map(_.get))
  }

  def apply(th:DeclaredTheory,list:List[GlobalName],substs:List[(GlobalName,GlobalName)],ctrl:Controller):Boolean = {
    val consts = for {a <- list} yield ctrl.get(a) match {
      case c: FinalConstant => Some(c)
      case _ => None
    }
    if (consts.contains(None)) false
    else apply(th,consts.map(_.get),substs)
  }

  def apply(th:DeclaredTheory,list:List[GlobalName],ctrl:Controller):Boolean = apply(th,list,List(),ctrl)

  /**
   * For every (t1,t2) in
   * @param substs , substitutes every occurence of t2 in
   * @param victim by t1.
   * @return the fully substituted Term.
   *         Needed for makeCodomain.
   */

  def substitute(victim: Term, substs: List[(GlobalName,GlobalName)]): Term = victim match {

    case c: OMV => c

    case c: OMID => substs collectFirst {case p if c.head.get == p._2 => p} match {
      case None => c
      case Some(x) => OMID(x._1)
    }

    case c: OMBINDC => OMBINDC(c.binder, c.context, for {y <- c.scopes} yield substitute(y,substs))

    case c: OMA => OMA(substs collectFirst {case p if c.head.get == p._2 => p} match {
      case None => c.fun
      case Some(x) => OMID(x._1)
    },for {y <- c.args} yield substitute(y,substs))

    case c: OMATTR => OMATTR(substitute(c.arg,substs),
      substs collectFirst {case p if c.head.get == p._2 => p} match {
        case None => c.key
        case Some(x) => OMID(x._1)
      },substitute(c.value,substs))

    case _ => victim
  }

  def substitute(victim: Option[Term],substs: List[(GlobalName,GlobalName)]): Option[Term] = victim match {
    case Some(t) => Some(substitute(t,substs))
    case None => None
  }

  def orderp(list:List[(FinalConstant,FinalConstant)]) : List[(FinalConstant,FinalConstant)] = {
    val newlist = orderh(list.map(b => Consthash(b._2,List(),None))).reverse
    newlist.map(p => list.collectFirst{case c if c._2.path==p.name => c}.get)
  }

  def order(list:List[FinalConstant]):List[FinalConstant] = {
    val newlist = orderh(list.map(b => Consthash(b,List(),None))).reverse
    newlist.map(p => list.collectFirst{case c if c.path==p.name => c}.get)
  }

  def orderh(list:List[Consthash]):List[Consthash] = {
    if(list.isEmpty || list.tail.isEmpty) list
    else if (!list.tail.exists(hash => SubtractDeclaration.occursIn(list.head.name,hash,list)))
      list.head::orderh(list.tail)
    else orderh(list.tail:::List(list.head))
  }

}
