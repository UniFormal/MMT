package info.kwarc.mmt.api.checking
import info.kwarc.mmt.api._
import objects._

/** apply/unapply methods for missing terms of known type */
object Hole {
  private val cd = utils.mmt.mmtbase ? "Errors"
  val path = cd ? "missing"
  private val missing = OMID(path)
  def apply(t: Term) = OMA(missing, List(t))
  def unapply(t: Term) : Option[Term] = t match {
     case OMA(this.missing, List(t)) => Some(t)
     case _ => None
  }
}

object HoleTerm extends InferenceRule(Hole.path, Hole.path) {
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = Hole.unapply(tm)
}
