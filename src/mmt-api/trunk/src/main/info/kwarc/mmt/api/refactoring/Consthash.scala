package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api.GlobalName
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.FinalConstant

import scala.util.Success

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
   * @param a , while adding all occurences of constants in
   * @param enums to the hash instead of the list of parameters.
   * @param judg the GlobalName for the [role Judgment] Declaration used to check, whether a is an axiom or not.
   * @return a Consthash for a.
   */
  def apply(a:FinalConstant,enums:List[GlobalName],judg:Option[GlobalName]) : Consthash = {
    def simhashit(tp: Term, subs: Set[(OMV, Int)], current: (List[Int],List[GlobalName]),vars: Int)
    : (List[Int], List[GlobalName]) = tp match {

      case t: OMV => (
        0::(subs.collectFirst { case p if p._1 == t => p._2 } match { case None => vars case Some(x: Int) => x })::current._1,
        current._2)

      case t: OMID => {
        val a = t.head match {
          case Some(x: GlobalName) => x
          case _ => throw new Exception("GlobalName expected!")
        }
        if (current._2 contains a)  (1::(current._2.indexWhere(p => p==a)+enums.length)::current._1, current._2)
        else if (enums contains a) (1::enums.indexWhere(p => p==a)::current._1, current._2)
        else (1::current._2.length+enums.length::current._1,current._2:::List(a))
      }

      case t: OMA => {
        val a = t.head match {
          case Some(x: GlobalName) => x
          case _ => throw new Exception("GlobalName expected!")
        }
        val newlist = if (current._2 contains a)
          (2::t.args.length::(current._2.indexWhere(p => p==a)+enums.length)::current._1, current._2)
        else if (enums contains a)
          (2::t.args.length::enums.indexWhere(p => p==a)::current._1, current._2)
        else (2::t.args.length::current._2.length+enums.length::current._1,current._2:::List(a))
        t.args.foldLeft(newlist)((p,s) => simhashit(s,subs,p,vars))
      }

      case t: OMBINDC => {
        val newsubs = subs ++ (for {o <- 0 to t.numVars - 1} yield (t.context.variables(o).toTerm, vars + o))
        val newvars = vars+t.numVars
        val a = t.head match {
          case Some(x: GlobalName) => x
          case _ => throw new Exception("GlobalName expected!")
        }
        val newlist = if (current._2 contains a)
          (3::t.numVars::vars::t.scopes.length::(current._2.indexWhere(p => p==a)+enums.length)::current._1, current._2)
        else if (enums contains a)
          (3::t.numVars::vars::t.scopes.length::enums.indexWhere(p => p==a)::current._1, current._2)
        else (3::t.numVars::vars::t.scopes.length::current._2.length+enums.length::current._1, current._2:::List(a))
        t.scopes.foldLeft(newlist)((p,s) => simhashit(s,newsubs,p,newvars))
      }

      case _ => throw new Exception("Unclear Term Type for Hashing: " + tp.getClass)
    }

    a.tp match {
      case Some(t: Term) => {
        val ret = simhashit(t, Set(), (List(), List()), 0)
        Consthash(a.path, ret._1.hashCode(), ret._2,judg match {
          case Some(j:GlobalName) => AxiomHandler.isAxiom(a,j)
          case None => false
        })
      }
      case _ => Consthash(a.path, List(-1).hashCode(), List(),false)
    }

  }

  /**
   * Calculates Consthashes for two theories
   * @param th1 and
   * @param th2 as flattened, while adding all constants in included theories that th1 and th2 have in common
   *            to the hash instead of the list of parameters.
   * @param ctrl
   * @return A pair of Sets of Consthash, the first component for Constants in th1, the second for th2.
   */
  def apply(th1: DeclaredTheory,th2: DeclaredTheory,ctrl:Controller)
  : (Set[Consthash],Set[Consthash]) = {
    // TODO: check for compatible metatheories maybe - the way I do it might subsume that.

    val judg1 = AxiomHandler.findJudgment(ctrl,th1)
    val judg2 = AxiomHandler.findJudgment(ctrl,th2)

    val includes1 = getIncludes(th1,ctrl)
    val includes2 = getIncludes(th2,ctrl)

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

    (hashes1,hashes2)

  }

  /**
   * Recursively gets all inclusions of a theory
   * @param th , including th itself.
   * @param ctrl
   * @return All includes as Set of DeclaredTheory
   */

  def getIncludes(th: DeclaredTheory, ctrl:Controller) : Set[DeclaredTheory] = {
    (for {o <- th.getIncludes} yield ctrl.get(o)).foldLeft(Set(th))(
      (b,t) => b++(t match {
        case x:DeclaredTheory => getIncludes(x,ctrl)
        case _ => Set()
      }))
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

  def findJudgment(ctrl:Controller,th:DeclaredTheory):Option[GlobalName] = {

    def findJudgmentIt(th:DeclaredTheory):Option[GlobalName] = {
      val list = for {o <- th.getConstants.filter(p => p.rl match {
        case t: Some[String] => true
        case _ => false
      }) if o.rl.get == "Judgment"} yield o match {
        case t: FinalConstant => t.path
        case _ => throw new Exception("FinalConstant Expected!")
      }
      if (list.nonEmpty) Some(list.head)
      else {
        val list2 = for {o <- th.getIncludes if (try {ctrl.get(o) match {
          case t: DeclaredTheory => true
          case _ => false
        }} catch {case _ => false})}
          yield findJudgmentIt(ctrl.get(o) match { case t: DeclaredTheory => t case _ => throw new Exception("FinalConstant Expected!") })
        val list3=list2.collect {case Some(t) => t}
        if (list3.nonEmpty) Some(list3.head) else None
      }
    }

    findJudgmentIt(th)
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
