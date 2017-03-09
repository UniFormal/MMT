package info.kwarc.mmt.mathscheme.rules

import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api._
import info.kwarc.mmt.lf.{OfType, Typed}

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

// object Extends extends StructuralFeatureRule("extend")
// object Renaming extends StructuralFeatureRule("RenamingOf")
// object Combine extends StructuralFeatureRule("combine")



object Extends extends {
  val extend = new sym("extends") {
    def unapply(t : Term) : Option[(Term,List[OML])] = t match {
      case OMA(`tm`,th :: args) if args.forall(_.isInstanceOf[OML]) => Some((th,args.map(_.asInstanceOf[OML])))
      case _ => None
    }
  }
} with TheoryExpRule(extend.path,OfType.path) {
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



object Renaming extends {
  val rename = new sym("renaming") {
    def unapply(t : Term) : Option[(Term,List[OML])] = t match {
      case OMA(`tm`,th :: args) if args.forall(_.isInstanceOf[OML]) => Some((th,args.map(_.asInstanceOf[OML])))
      case _ => None
    }
  }
} with TheoryExpRule(rename.path,OfType.path) {
  def apply(tm: Term, covered: Boolean)(implicit solver : Solver, stack: Stack, history: History): Boolean = tm match {
    case rename(th,ls) =>
      solver.check(IsTheory(stack,th))
      ls forall {
        case OML(name,toOpt,Some(OMS(p))) => true // TODO
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


object Combine extends {
  val combine = new appsym("combine")
} with TheoryExpRule(combine.path,OfType.path) {
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

object Labcont extends {
  val compth = new appsym("LabCont")
} with TheoryExpRule(compth.path,OfType.path) {
  def apply(tm: Term, covered: Boolean)(implicit solver : Solver, stack: Stack, history: History): Boolean = tm match {
    case compth(ls) =>
      true
    case _ => false
  }

  def elaborate(prev : Context, df : Term)(implicit elab : (Context,Term) => Context) : Context = df match {
    case compth(ls) => ls.map {
      case o:OML => o.vd
    }
    // case _ => Nil
  }

}