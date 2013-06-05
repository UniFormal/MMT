package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import frontend._

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

object HoleTerm extends InferenceRule(Hole.path) {
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack): Option[Term] = Hole.unapply(tm)
}

class ErrorsPlugin extends Plugin {
   val dependencies = Nil
   override def init(c: Controller, args: List[String]) {
      super.init(c, args)
      c.extman.ruleStore.add(HoleTerm)
   }
}