package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api.libraries.Closer
import info.kwarc.mmt.api.{LocalName, MPath, GlobalName}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.FinalConstant

import scala.util.{Try, Success}

/**
 * Helper class that makes matching end evaluation of FinalConstants easier.
 * @param name the GlobalName of c
 * @param hash a hashed version of the term tree making up c
 * @param pars the list of constants occuring in the type of c
 * @param isAxiom True, if c is an axiom (i.e. its type contains a [role Judgment] declaration)
 */
case class Consthash(name:GlobalName,hash:Int,pars:List[GlobalName],isAxiom:Boolean) {

  /**
   * Evaluated, whether (given a list of substitutions of parameters), this perfectly matches that
   * @param l   a list of substitutions
   * @param that
   * @return true, if this and that match
   */
  def matches(l:List[(GlobalName,GlobalName)])(that:Consthash):Boolean = {
    if (this.hash!=that.hash) false
    else if (this.pars.length!=that.pars.length) false
    else (pars zip that.pars).forall {case (p,q) => p==q ||  l.contains((p,q))}
  }

  //def matches(l:List[(GlobalName,GlobalName)])(that:Consthash):Boolean = newPairs(l)(that).isEmpty
  override def toString = this.name.toString

}

/**
 * Constructor for Consthashes
 */
object Consthash {
  /**
   * Constructs a Consthash for FinalConstant
   * @param const , while adding all occurences of constants in
   * @param enums to the hash instead of the list of parameters.
   * @param judg the GlobalName for the [role Judgment] Declaration used to check, whether a is an axiom or not.
   * @return a Consthash for a.
   */
  def apply(const:FinalConstant,enums:List[GlobalName],judg:Option[GlobalName]) : Consthash = {
    def simhashit(tp: Term, subs: Set[(OMV, Int)], current: (List[Int],List[GlobalName]),vars: Int)
    : (List[Int], List[GlobalName],Boolean) = tp match {

      case t: OMV => (
        0::(subs.collectFirst { case p if p._1 == t => p._2 } match { case None => vars case Some(x: Int) => x })::current._1,
        current._2,false)

      case t: OMID => {
        val a = t.head match {
          case Some(x: GlobalName) => x
          case _ => throw new Exception("GlobalName expected!")
        }
        val isax = if (judg.isDefined) judg.get==a else false
        if (current._2 contains a)  (1::(current._2.indexWhere(p => p==a)+enums.length)::current._1, current._2,isax)
        else if (enums contains a) (1::enums.indexWhere(p => p==a)::current._1, current._2,isax)
        else (1::current._2.length+enums.length::current._1,current._2:::List(a),isax)
      }

      case t: OMA => {
        val a = t.head match {
          case Some(x: GlobalName) => x
          case _ => throw new Exception("GlobalName expected!")
        }
        val isax = if (judg.isDefined) judg.get==a else false
        val newlist = if (current._2 contains a)
          (2::t.args.length::(current._2.indexWhere(p => p==a)+enums.length)::current._1, current._2)
        else if (enums contains a)
          (2::t.args.length::enums.indexWhere(p => p==a)::current._1, current._2)
        else (2::t.args.length::current._2.length+enums.length::current._1,current._2:::List(a))
        t.args.foldLeft((newlist._1,newlist._2,isax))((p,s) => {
          val res =simhashit(s,subs,(p._1,p._2),vars)
          (res._1,res._2,if (p._3) true else res._3)
        })
      }

      case t: OMBINDC => {
        val newsubs = subs ++ (for {o <- 0 to t.numVars - 1} yield (t.context.variables(o).toTerm, vars + o))
        val newvars = vars+t.numVars
        val a = t.head match {
          case Some(x: GlobalName) => x
          case _ => throw new Exception("GlobalName expected!")
        }
        val isax = if (judg.isDefined) judg.get==a else false
        val newlist = if (current._2 contains a)
          (3::t.numVars::vars::t.scopes.length::(current._2.indexWhere(p => p==a)+enums.length)::current._1, current._2)
        else if (enums contains a)
          (3::t.numVars::vars::t.scopes.length::enums.indexWhere(p => p==a)::current._1, current._2)
        else (3::t.numVars::vars::t.scopes.length::current._2.length+enums.length::current._1, current._2:::List(a))
        t.scopes.foldLeft((newlist._1,newlist._2,isax))((p,s) => {
          val res =simhashit(s,newsubs,(p._1,p._2),newvars)
          (res._1,res._2,if(p._3) true else res._3)
        })
      }

      case _ => // TODO implement properly !
        (tp.getClass.hashCode()::current._1,current._2,false)
    }

    const.tp match {
      case Some(t: Term) => {
        val ret = simhashit(t, Set(), (List(), List()), 0)
        Consthash(const.path, ret._1.hashCode(), ret._2,ret._3)
      }
      case _ => Consthash(const.path, List(-1).hashCode(), List(),false)
    }

  }

  /**
   * Calculates Consthashes for two theories
   * @param th1 and
   * @param th2 as flattened, while adding all constants in included theories that th1 and th2 have in common
   *            to the hash instead of the list of parameters.
   * @param ctrl
   * @return A pair of Sets of Consthash, the first component for Constants in th1, the second for th2, and a pair of
   *         (optional, depending on whether found) the respective judgment declarations.
   */
  def apply(th1: DeclaredTheory,th2: DeclaredTheory,ctrl:Controller)
  : ((Set[Consthash],Set[Consthash]),(Option[GlobalName],Option[GlobalName])) = {
    // TODO: check for compatible metatheories maybe - the way I do it might subsume that.
    val closer = new Closer(ctrl)
    val includes1 = closer.getIncludes(th1,true)
    val includes2 = closer.getIncludes(th2,true)

    val judg1 = AxiomHandler.findJudgment(ctrl,th1,Some(includes1))
    val judg2 = AxiomHandler.findJudgment(ctrl,th2,Some(includes2))

    val consts1 = (includes1 diff includes2).foldLeft(Set().asInstanceOf[Set[FinalConstant]])((arg,th) =>
      (th.getConstants collect {case t:FinalConstant => t}).toSet++arg
    )


    val consts2 = (includes2 diff includes1).foldLeft(Set().asInstanceOf[Set[FinalConstant]])((arg, th) =>
      (th.getConstants collect { case t: FinalConstant => t }).toSet++arg
    )

    val commonconsts = (includes1 intersect includes2).foldLeft(Set().asInstanceOf[Set[GlobalName]])((arg,th) =>
      (th.getConstants collect {case t:FinalConstant => t.path}).toSet++arg
    ).toList

    val hashes1 = for {o <- consts1} yield apply(o,commonconsts,judg1)
    val hashes2 = for {o <- consts2} yield apply(o,commonconsts,judg2)

    ((hashes1,hashes2),(judg1,judg2))

  }

}

/**
 * Helper Object for axiom handling
 */

object AxiomHandler {

  /**
   * Tries to recursively find a/the [role Judgment] declaration used for axioms in a theory
   * @param th .
   * @return the GlobalName for the Judgment declaration.
   */

  def findJudgment(ctrl:Controller,th:DeclaredTheory,includes:Option[Set[DeclaredTheory]]):Option[GlobalName] = {

    def findJudgmentIt(th:DeclaredTheory):Option[GlobalName] = {
      val list = for {o <- th.getConstants.filter(p => p.rl match {
        case t: Some[String] => true
        case _ => false
      }) if o.rl.get == "Judgment"} yield o match {
        case t: FinalConstant => t.path
        case _ => throw new Exception("FinalConstant Expected!")
      }
      list.headOption
    }

    val ths = includes match {
      case Some(set) => set
      case None =>
        val closer = new Closer(ctrl)
        closer.getIncludes(th,true)
    }
    ((for {o <- ths} yield findJudgmentIt(o)) collect {case Some(x) => x}).headOption

  }

  /**
   * Checks whether the FinalConstant
   * @param a is an axiom by looking for occurences of
   * @param judg in the type of a. Used in the process of calculating ConstHash.
   * @return true, if a is deemed an axiom.
   */

  def isAxiom(a:FinalConstant,judg:GlobalName):Boolean = a.tp match {
    case None => false
    case Some(tp) => isAxiomIterator(tp,judg)
  }

  /**
   * Iterator method used by isAxiom.
   * @param tp Some term.
   * @param judg as above.
   * @return true, if tp is deemed an axiom.
   */

  def isAxiomIterator(tp:Term,judg:GlobalName):Boolean = tp match {
    case t: OMV => false
    case t: OMID => t.head.get==judg
    case t: OMA => ((t.head.get==judg)::(for{o <- t.args} yield isAxiomIterator(o,judg))) contains true
    case t: OMBINDC => ((t.head.get==judg)::(for{o <- t.scopes} yield isAxiomIterator(o,judg))) contains true
    case _ => false
  }

}
