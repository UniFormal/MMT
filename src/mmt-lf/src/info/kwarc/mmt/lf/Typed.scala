package info.kwarc.mmt.lf
import info.kwarc.mmt.api._
import objects._
import checking._

object Typed {
   val _base = DPath(utils.URI("http", "cds.omdoc.org") / "urtheories")
   val path = _base ? "Typed"
   /** the MMT URI of type */
   val ktype = path ? "type"
   /** the MMT URI of kind */
   val kind = _base ? "Kinded" ? "kind"
}


object OfType {
   val path = Typed.path ? "oftype"
   def apply(t : Term) = OMA(OMID(path),List(t))
   def unapply(t : Term) : Option[Term] = t match {
      case OMA(OMID(this.path), List(a)) => Some(a)
      case _ => None
   }
}

/** provides apply/unapply methods for the LF equality symbol */
object LFEquality {
   /** the MMT URI of -> */
   val path = LF._path ? "equality"
   def apply(t1 : Term, t2 : Term) = OMA(OMID(path),List(t1,t2))
   def unapply(t : Term) : Option[(Term,Term)] = t match {
      case OMA(OMID(this.path), List(a,b)) => Some((a, b))
      case _ => None
   }
}

/** provides apply/unapply methods for a universes
   in particular, Univ(1), Univ(2) are type and kind, respectively
 */
object Univ {
   def apply(level : Int) : Term = if (level == 1) OMS(Typed.ktype) else OMS(Typed.kind)
   def unapply(t : Term) : Option[Int] =
      if (t == OMS(Typed.kind)) Some(2) else if (t == OMS(Typed.ktype)) Some(1) else None
}

/** the rule that makes type a valid universe */
object UniverseType extends UniverseRule(Typed.ktype) {
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack, history: History) : Boolean = tm == OMS(Typed.ktype)
}

/** the rule that makes kind a valid universe */
object UniverseKind extends UniverseRule(Typed.kind) {
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack, history: History) : Boolean = tm == OMS(Typed.kind)
}

/** the type inference rule type:kind */
object UnivTerm extends InferenceRule(Typed.ktype, OfType.path) {
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = tm match {
      case OMS(Typed.ktype) => Some(OMS(Typed.kind))
      case _ => None
   }
}

/** the type inference rule |- A : X, |- B : Y, |- X = Y ---> |- (A = B) : kind for identity kinds
 * This rule goes beyond LF but it does not harm because it only adds kinds and thus do not affect types and terms   
 */
object EqualityTerm extends InferenceRule(LFEquality.path, OfType.path) {
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = tm match {
      case LFEquality(a,b) =>
         if (covered) return Some(OMS(Typed.kind))
         val aT = solver.inferType(a)(stack, history + "infering left term")
         val bT = solver.inferType(b)(stack, history + "infering right term")
         val equalTypes = solver.check(Equality(stack,a,b,None))(history + "types must be equal")
         if (equalTypes)
            Some(OMS(Typed.kind))
         else None
      case _ => None
   }
}
