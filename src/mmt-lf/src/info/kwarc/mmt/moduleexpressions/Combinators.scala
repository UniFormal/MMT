package info.kwarc.mmt.moduleexpressions

import info.kwarc.mmt.api._
import checking._
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.symbols.{Constant, PlainInclude}
import objects._
import uom._
import info.kwarc.mmt.lf._

object Combinators {
  val _path = ModExp._base ? "Combinators"
}

/* The rules below compute the results of theory combinators.
 * Each rule is applicable if the arguments have been computed already.
 *   
 * The rules also throw typing errors if they encounter any.
 * Open question: Should they be required to find all errors? Maybe only all structural errors?
 */

object Extends extends FlexaryConstantScala(Combinators._path, "extends")

// TODO all rules must preserve and reflect typing errors
// name clashes can be checked at very end on flat theory
// declaration merging can also happen at very end

object ComputeExtends extends ComputationRule(Extends.path) {
   def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) = {
      val Extends(thy,wth@_*) = tm
      thy match {
        case AnonymousTheory(mt, decls) =>
          wth match {
            case OMLList(omls) => Some(AnonymousTheory(mt, decls ::: omls))
            case _ => None
          }
        case OMMOD(th) =>
          val meta = solver.lookup(th) match {
            case Some(th : DeclaredTheory) => th.meta
            case _ => None
          }
          wth match {
            case OMLList(omls) =>
              Some(AnonymousTheory(meta,IncludeOML(th,Nil) :: omls)) //TODO should be IncludeOML
            case _ => None
          }
        case _ => None
      }
   }
}

object Combine extends FlexaryConstantScala(Combinators._path, "combine")

object ComputeCombine extends ComputationRule(Combine.path) {
   def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = {
      val Combine(thys@_*) = tm
      var mts: List[MPath] = Nil
      var decls: List[OML] = Nil
      thys.foreach {
        case AnonymousTheory(mtO, ds) =>
          mtO.foreach {mts ::= _}
          decls = decls ::: ds
        case OMMOD(mp) =>
          val mtO = solver.lookup(mp) match {
            case Some(th : DeclaredTheory) => th.meta
            case _ => None
          }
          mtO foreach {mts ::= _}
          decls = decls ::: IncludeOML(mp,Nil) :: Nil
        case _ =>
          return None
      }
      val declsD = decls.distinct
      val mt = mts.distinct match {
        case Nil => None
        case hd::Nil => Some(hd)
        case _ => return None
      }
      Some(AnonymousTheory(mt, declsD))
   }
}

object Rename extends FlexaryConstantScala(Combinators._path, "rename")

object ComputeRename extends ComputationRule(Rename.path) {
   def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = {
      val Rename(thy,rens@_*) = tm
      val at = solver.getTheory(thy).getOrElse(return None)
     rens.foreach {
       case OML(nw, None, Some(OML(old, None,None,_,_)),_,_) =>
         at.rename(old,nw)
       case OML(nw,None,Some(OMS(old)),_,_) =>
         at.rename(old.name,nw)
       case r => solver.error("not a renaming " + r)
     }
     Some(at.toTerm)
   }
}

/**
 * Translate(m,T) and Expand(m,T) form a pushout along an inclusion as follows:
 * 
 * m : A -> B
 * inclusion from A to T
 * Expand(m,T): T -> Translate(m,T)
 * inclusion from B to Translate(m,T)
 */
object Translate extends BinaryConstantScala(Combinators._path, "translate")

object ComputeTranslate extends ComputationRule(Translate.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = {
    val Translate(mor, thy) = tm
    val res = solver.getTheory(thy).getOrElse(return None)
    res.decls.foreach { case OML(n, t, d, _, _) =>
      // skip all includes of theories that are already include in domain of mor
      // check for name clashes: n may not be defined in the codomain of mor
      val tT = t map {
        OMM(_, mor)
      }
      val dT = d map {
        OMM(_, mor)
      }
      val declT = OML(n, tT, dT)
      res.add(declT)
    }
    Some(res.toTerm)
  }
}

// TODO better name
/** see [[Translate]] */
object Expand extends BinaryConstantScala(Combinators._path, "expand")

object ComputeExpand extends ComputationRule(Expand.path) {
   def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = {
      val Expand(mor, thy) = tm
      thy match {
        case AnonymousTheory(mt, ds) =>
          val res = new AnonymousTheory(mt, Nil) //TODO this should be an AnonymousMorphism; same as AnonymousTheory but no dependency
          // add include of mor
          ds.foreach {case OML(n,t,d,_,_) =>
            // skip all includes of theories that are already include in domain of mor
            val ass = OML(n,None,Some(OML(n,None,None)))
            res.add(ass)
          }
          Some(res.toTerm)
        case _ => None
      }
   }
}


/*
  val extend = new sym("extends") {
    def unapply(t : Term) : Option[(Term,List[OML])] = t match {
      case OMA(`tm`,th :: args) if args.forall(_.isInstanceOf[OML]) => Some((th,args.map(_.asInstanceOf[OML])))
      case _ => None
    }
  }
} with TheoryExpRule {
  def apply(tm: Term, covered: Boolean)(implicit solver : Solver, stack: Stack, history: History): Boolean = tm match {
    case extend(th,ls) =>
      solver.check(IsTheory(stack,th))
      val thcont : Context = solver.elaborateModuleExpr(th,stack.context)
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
    case compth(ls) if ls.forall(_.isInstanceOf[OML]) => ls.map{
      case OML(vname,vtp,vdf) =>
        VarDecl(vname,vtp,vdf,None)
    }
    // case _ => Nil
  }

}
*/