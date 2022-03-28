package info.kwarc.mmt.api.checking
import info.kwarc.mmt.api._
import objects._

/** apply/unapply methods for missing terms of known type */
object Errors {
  val cd = utils.mmt.mmtbase ? "Errors"
}
object Hole {
  val path = Errors.cd ? "missing"
  private val missing = OMID(path)
  def apply(t: Term) = OMA(missing, List(t))
  def unapply(t: Term) : Option[Term] = t match {
     case OMA(this.missing, List(t)) => Some(t)
     case _ => None
  }
}

object Prove {
  val path = Errors.cd ? "prove"
  private val prove = OMID(path)
  def apply(t: Term) = OMA(prove, List(t))
  def unapply(t: Term) : Option[Term] = t match {
    case OMA(this.prove, List(t)) => Some(t)
    case _ => None
  }
}

object UnknownTerm {
  val path = Errors.cd ? "unknown"
  private val prove = OMID(path)
  def apply(t: Term*) = OMA(prove, t.toList)
  def unapply(t: Term) : Option[List[Term]] = t match {
    case OMA(this.prove, ts) => Some(ts)
    case _ => None
  }
}

object HoleTerm extends InferenceRule(Hole.path, Hole.path) {
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = Hole.unapply(tm)
}

/**
  * infers prove X : X and adds an unknown _:X for an external proof obligation
  */
object InferProve extends InferenceAndTypingRule(Prove.path, Prove.path) {
  private def proofName(typeName: LocalName) = typeName / "proof"
  def apply(solver: Solver, tm: Term, tp: Option[Term], covered: Boolean)(implicit stack: Stack, history: History) = {
    tm match {
      case Prove(g) =>
        g match {
          case solver.Unknown(n, _) =>
            val pn = proofName(n)
            // type of proof obligation is unknown -> add unknown for the proof obligation if not done yet
            if (! solver.getPartialSolution.isDeclared(pn)) {
              solver.addUnknowns(OMV(pn) % g, None)
            }
        }
        val conforms = tp map {t =>
          covered || solver.check(Equality(stack, t, g, None))(history + "solving type of missing proof")
        }
        (Some(g), conforms)
      case _ => (None,None)
    }
  }
}

/**
  * unknown terms always type-check
  */
object InferUnknown extends InferenceAndTypingRule(UnknownTerm.path, UnknownTerm.path) {
  def apply(solver: Solver, tm: Term, tp: Option[Term], covered: Boolean)(implicit stack: Stack, history: History) = {
    tm match {
      case UnknownTerm(_) => (None, Some(true))
      case _ => (None,None)
    }
  }
}

