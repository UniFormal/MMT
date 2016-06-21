package info.kwarc.mmt.reflection
import info.kwarc.mmt.api._
import libraries._
import modules._
import symbols._
import objects._

object MorphApp {
  def apply(mor : Term, t : Term) : Term = apply(mor, t, 1)
  def apply(mor : Term, t : Term , n : Int) : Term = t match {
      case OMID(path) if n == 1 => mor match {
        case ExplicitMorph(r,d) => r.fields.find(localName => Some(localName._1) == path.toTriple._3) match {
          case Some(pair) => OMM(pair._2,mor)
        }
      }
      case TermEval(tm,th) if n == 1 => OMM(tm, th)
      case OMID(path) => t
      case OMV(name) => t
      case TermRefl(tm,th) => TermRefl(MorphApp(mor, tm, n+1), th)
      case TermEval(tm,th) => TermEval(MorphApp(mor, tm, n-1), th)
      case ReflType(th, tp) => ReflType(th, MorphApp(mor, tp, n+1))
      case OMM(tm, m) => apply(mor, apply(m,tm))
   }
}

object MorphAppRule extends ComputationRule(utils.mmt.morphismapplication) {
  def apply(solver: Solver)(tm: Term)(implicit stack: Stack) : Option[Term] = {
    tm match {
      case OMM(t,m) => Some(MorphApp(m,t,1))
      case _ => None
    }
  }
}