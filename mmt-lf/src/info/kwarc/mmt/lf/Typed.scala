package info.kwarc.mmt.lf
import info.kwarc.mmt.api._
import objects._

object Typed {
   val path = DPath(utils.URI("http", "cds.omdoc.org") / "urtheories") ? "Typed"
   /** the MMT URI of type */
   val ktype = path ? "type"
   /** the MMT URI of kind */
   val kind = path ? "kind"
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
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack) : Boolean = tm == OMS(Typed.ktype)
}

/** the rule that makes kind a valid universe */
object UniverseKind extends UniverseRule(Typed.kind) {
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack) : Boolean = tm == OMS(Typed.kind)
}

/** the type inference rule type:kind */
object UnivTerm extends InferenceRule(Typed.ktype) {
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack) : Option[Term] = tm match {
      case OMS(Typed.ktype) => Some(OMS(Typed.kind))
      case _ => None
   }
}