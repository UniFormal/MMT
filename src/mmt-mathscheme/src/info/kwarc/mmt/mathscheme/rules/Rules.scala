package info.kwarc.mmt.mathscheme.rules

import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api._
import info.kwarc.mmt.lf.{OfType, Typed}
import scala.language.reflectiveCalls

object MSTheory {
  val _base = DPath(utils.URI("http", "test.org") / "mathscheme")
  val thpath = _base ? "Meta"
}

class sym(s : String) {

  import MSTheory._

  val path = thpath ? s
  val tm = OMS(path)
}
class appsym(s : String) extends sym(s) {

  def apply(ls : Term*) = OMA(tm,ls.toList)
  def unapply(t : Term) : Option[List[Term]] = t match {
    case OMS(`path`) => Some(Nil)
    case OMA(`tm`,ls) => Some(ls)
    case _ => None
  }

}

private[rules] class uasym(s: String) extends sym(s) {
  def unapply(t: Term): Option[(Term, List[OML])] = t match {
    case OMA(`tm`, th :: args) if args.forall(_.isInstanceOf[OML]) => Some((th, args.map(_.asInstanceOf[OML])))
    case _ => None
  }
}

// object Extends extends StructuralFeatureRule("extend")
// object Renaming extends StructuralFeatureRule("RenamingOf")
// object Combine extends StructuralFeatureRule("combine")

private[rules] abstract class ExtendsEI(val extend: uasym) extends TheoryExpRule(extend.path,OfType.path)

object Extends extends ExtendsEI(new uasym("extends")) {
  def apply(tm: Term, covered: Boolean)(implicit solver : Solver, stack: Stack, history: History): Boolean = tm match {
    case extend(th,ls) =>
      solver.check(IsTheory(stack,th))
      val thcont : Context = ??? // solver.elaborateModuleExpr(th,stack.context)
      ls.foldLeft((stack.context ++ thcont,true))((p,oml) => {
        val checks = oml.tp.forall(tp => solver.check(Inhabitable(Stack(p._1),tp))) && oml.df.forall(df => {
          if (oml.tp.isDefined) solver.check(Typing(Stack(p._1),df,oml.tp.get)) else true
        })
        (p._1,p._2 && checks)
      })._2
    case _ => false
  }

  def elaborate(prev : Context, df : Term)(implicit elab : (Context,Term) => Context) : Context = df match {
    case extend(th, ls) =>
      elab(prev,th) ::: ls.map(_.vd)
  }
}

private[rules] abstract class RenamingEI(val rename: uasym) extends TheoryExpRule(rename.path,OfType.path)

object Renaming extends RenamingEI(new uasym("renaming")) {
  def apply(tm: Term, covered: Boolean)(implicit solver : Solver, stack: Stack, history: History): Boolean = tm match {
    case rename(th,ls) =>
      solver.check(IsTheory(stack,th))
      ls forall {
        case OML(name,toOpt,Some(OMS(p)),_,_) => true // TODO
        case _ =>
          false
      }
    case _ => false
  }

  def elaborate(prev : Context, df : Term)(implicit elab : (Context,Term) => Context) : Context = df match {
    case rename(th,ls) => ???
    // case _ => Nil
  }
}

private[rules] abstract class CombineEI(val combine: appsym) extends TheoryExpRule(combine.path,OfType.path)

object Combine extends CombineEI(new appsym("combine")) {
  def apply(tm: Term, covered: Boolean)(implicit solver : Solver, stack: Stack, history: History): Boolean = tm match {
    case combine(ls) =>
      ls.forall(p => solver.check(IsTheory(stack,p)))
    case _ => false
  }

  def elaborate(prev : Context, df : Term)(implicit elab : (Context,Term) => Context) : Context = df match {
    case combine(ls) =>
      ls.flatMap(elab(prev,_))
    // case _ => Nil
  }
}

private[rules] abstract class LabcontEI(val compth: appsym) extends TheoryExpRule(compth.path,OfType.path)

object Labcont extends LabcontEI(new appsym("LabCont")) {
  def apply(tm: Term, covered: Boolean)(implicit solver : Solver, stack: Stack, history: History): Boolean = tm match {
    case compth(ls) =>
      true
    case _ => false
  }

  def elaborate(prev : Context, df : Term)(implicit elab : (Context,Term) => Context) : Context = df match {
    case compth(ls) => ls.collect {
      case o:OML => o.vd
    }
    case _ => throw ImplementationError("expected LabCont to elabortate")
    // case _ => Nil
  }

}
