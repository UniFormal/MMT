package info.kwarc.mmt.LFX.LFRecords

import info.kwarc.mmt.api.LocalName
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.lf.{OfType, Typed}

object Common {
  /** convenience function for recursively checking the judgement |- a: type */
  def isType(solver: Solver, a: Term)(implicit stack: Stack, history: History) =
    solver.check(Typing(stack, a, OMS(Typed.ktype), Some(OfType.path)))(history + "type of bound variable must be a type")

  def pickFresh(solver: Solver, x: LocalName)(implicit stack: Stack) =
    Context.pickFresh(solver.constantContext ++ solver.getPartialSolution ++ stack.context, x)
}

/** Formation: the type inference rule |-x1:A1:type , ... , |-xn:An:type  --->  |{ x1:A1,...,xn:An}| : type
  * */
object RecordTypeTerm extends FormationRule(Rectype.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
    tm match {
      case Rectype(fields) if fields.forall(p => p.vd.tp.isDefined) =>
        if ((fields map (v => Common.isType(solver,v.vd.tp.get))).forall(p => p))
        Some(OMS(Typed.ktype)) else None
      case _ => None // should be impossible
    }
  }
}

/** Introduction: the type inference rule |-t1=d1:A1, ... |-tn=dn:An  --->  |[d1,...,dn|]:|{t1:A1,...,tn:An}|
  * */
object RecordExpTerm extends IntroductionRule(Recexp.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
    tm match {
      case Recexp(fields) if fields.forall(p => p.vd.df.isDefined) =>
        history += "Inferring record type of "+solver.presentObj(tm)
        val names = fields.map(v => v.vd.name)
        val defs = fields.map(v => v.vd.df.get)
        val tps = fields.map(v => {
          val tpdec = v.vd.tp
          val tpinf = solver.inferType(v.vd.df.get)
          if (tpdec.isEmpty) tpinf else
          if (tpinf.isEmpty) tpdec else
          if (solver.check(Equality(stack, tpdec.get, tpinf.get, Some(OMS(Typed.ktype))))) tpinf
          else None
        })
        if (tps.forall(_.isDefined)) Some(Rectype(fields.indices map (i => OML(VarDecl(names(i),Some(tps(i).get),Some(defs(i)),None))) :_*))
        else {
          history += "Couldn't infer type of record field element!"
          None
        }
      case _ => None // should be impossible
    }
  }
}

/** Elimination: the type inference rule t : |{a:A,...}|  --->  t.a:A */
// TODO extend to dependent types ?
object GetfieldTerm extends EliminationRule(Getfield.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = tm match {
    case Getfield(rec,v) =>
      val tp = solver.safeSimplifyUntil(solver.inferType(rec).getOrElse(return None))(Rectype.unapply)._1
      history += (solver.presentObj(rec)+" must have type "+solver.presentObj(tp))
      tp match {
        case Rectype(fields) =>
          val tpv = fields.collectFirst{case OML(p) if p.name==v.vd.name => p}
          if (tpv.isEmpty || tpv.get.tp.isEmpty) None else Some(tpv.get.tp.get)
        case _ => None
      }
    case _ => None // should be impossible
  }
}

/** type-checking: the type checking rule |-t.a1:A1 ... |-t.an:An  --->  t : |{a1:A1,...,an:An}| */
// TODO extend to dependent types ?
object RecTypeCheck extends TypingRule(Rectype.path) {
  def apply(solver:Solver)(tm:Term, tp:Term)(implicit stack: Stack, history: History) : Boolean = tp match {
      case Rectype(fields) =>
        val tpmatches = fields map (f =>
            if (f.vd.tp.isDefined)
              solver.check(Typing(stack,Getfield(tm,f),f.vd.tp.get))
            else false
          )
        tpmatches forall (p => p)
      case _ => false
    }
}

/** Equality for Record Type
  */
object RecTypeEquality extends TermHeadBasedEqualityRule(Nil, Rectype.path, Rectype.path) {
  def apply(checker: CheckingCallback)(tm1: Term, tm2: Term, tp: Option[Term])(implicit stack: Stack, history: History) = {
    (tm1,tm2) match {
      case (Rectype(omls1), Rectype(omls2)) =>
        Some(Continue{
          (omls1 map (v => (v.vd.name,v.vd.tp))).toSet == (omls2 map (v => (v.vd.name,v.vd.tp))).toSet
        })
      case _ => None
    }
  }
}

/** equality-checking:  |- t1.a1 = t2.a1 : A1 ... |- t1.an = t2.an : An  --->  t1 = t2 : |{ a1:A1,...,an:An }| */
object RecEquality extends TypeBasedEqualityRule(Nil, Rectype.path) {
  def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack, history: History): Option[Boolean] =
    tp match {
      case Rectype(fields) =>
        val tpmatches1 = fields map (f =>
          if (f.vd.tp.isDefined)
            solver.check(Typing(stack,Getfield(tm1,f),f.vd.tp.get))
          else false
          )
        if (tpmatches1 exists (p => !p)) return Some(false)
        val tpmatches2 = fields map (f =>
          if (f.vd.tp.isDefined)
            solver.check(Typing(stack,Getfield(tm1,f),f.vd.tp.get))
          else false
          )
        if (tpmatches2 exists (p => !p)) return Some(false)
        val eqmatches = fields map (f =>
            solver.check(Equality(stack,Getfield(tm1,f),Getfield(tm2,f),f.vd.tp))
        )
        Some(eqmatches forall (p => p))
      case _ => Some(false)
    }
}

/** computation: the rule |[a1=t,...]|.a1 = t
  */

object GetFieldComp extends ComputationRule(Getfield.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term]
    = tm match {
    case Getfield(Recexp(fields),f) =>
      fields collectFirst {case OML(g) if g.name==f.vd.name && g.df.isDefined => g.df.get}
    case _ => None
  }
}