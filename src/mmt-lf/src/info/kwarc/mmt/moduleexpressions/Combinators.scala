package info.kwarc.mmt.moduleexpressions

import info.kwarc.mmt.api._
import checking._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import objects._
import uom._
import info.kwarc.mmt.lf._

object Combinators {
  val _path = ModExp._base ? "Combinators"
}

object Common {
  /** turns a declared theory into an anonymous one by dropping all global qualifiers (only defined if names are still unique afterwards) */
  def anonymize(solver: CheckingCallback, namedTheory: DeclaredTheory)(implicit stack: Stack, history: History): AnonymousTheory = {
    // collect included theories
    val includes = namedTheory.getIncludesWithoutMeta.flatMap {i => solver.lookup(i) match {
      case Some(dt: DeclaredTheory) =>
        List(dt)
      case Some(se) =>
        solver.error("ignoring include of " + se.path)
        Nil
      case None =>
        Nil
    }}
    // code for translating OMS's to OML references
    var names: List[GlobalName] = Nil
    val trav = OMSReplacer {p =>
      if (names contains p) Some(OML(p.name)) else None
    }
    def translate(tm: Term) = trav(tm, stack.context)
    // turn all constants into OML's
    val omls = (includes:::List(namedTheory)).flatMap {th =>
      th.getDeclarationsElaborated.flatMap {
        case c: Constant =>
          val cT = OML(c.name,  c.tp map translate, c.df map translate, c.not)
          if (names.exists(p => p.name == c.name))
            solver.error("theory has duplicate name: " + c.name)
          names ::= c.path
          List(cT)
        case _ => Nil
      }
    }
    val real = RealizeOML(namedTheory.path, None) // the theorem that the anonymous theory realizes namedTheory
    new AnonymousTheory(namedTheory.meta, omls ::: List(real))
  }

  /** provides the base case of the function that elaborates a theory expression (in the form of an [[AnonymousTheory]]) */
  def asAnonymousTheory(solver: CheckingCallback, thy: Term)(implicit stack: Stack, history: History): Option[AnonymousTheory] = {
    thy match {
      // named theories
      case OMMOD(p) =>
        solver.lookup(p) match {
          case Some(th: DeclaredTheory) =>
            lazy val default = anonymize(solver, th)
            val at = th.df match {
              case Some(df) =>
                val dfS = solver.simplify(df)
                dfS match {
                  case AnonymousTheory(mt, ds) => new AnonymousTheory(mt,ds)
                  case _ => default
                }
              case None => default
            }
            Some(at)
          case Some(_) =>
            solver.error("not a theory: " + p)
            None
          case None =>
            solver.error("unknown name: " + p)
            None
        }
      // explicit anonymous theories
      case AnonymousTheory(mt, OMLList(ds)) => Some(new AnonymousTheory(mt,ds))
      case _ => None
    }
  }
}

/* The rules below compute the results of theory combinators.
 * Each rule is applicable if the arguments have been computed already.
 *
 * The rules also throw typing errors if they encounter any.
 * Open question: Should they be required to find all errors? Maybe only all structural errors?
 */

object Extends extends FlexaryConstantScala(Combinators._path, "extends")

// TODO all rules must preserve and reflect typing errors
// declaration merging must happen somewhere

object ComputeExtends extends ComputationRule(Extends.path) {
   def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = {
      val Extends(thy,wth@_*) = tm
      val thyAnon = Common.asAnonymousTheory(solver, thy).getOrElse {return None}
      wth match {
        case OMLList(extDecls) =>
          // replace OMS-references to declarations in thy with OML-references to declarations in thyAnon
          val trav = OMSReplacer {p =>
            if (thyAnon isDeclared p.name) Some(OML(p.name)) else None
          }
          val extDeclsR = extDecls map {oml => trav(oml,stack.context).asInstanceOf[OML]}
          val extAnon = new AnonymousTheory(thyAnon.mt, thyAnon.decls ::: extDeclsR)
          Some(extAnon.toTerm)
        case _ => return None
      }
   }
}

object Combine extends FlexaryConstantScala(Combinators._path, "combine")

object ComputeCombine extends ComputationRule(Combine.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = {
      val Combine(thys@_*) = tm
      val thysAnon = thys map {thy => Common.asAnonymousTheory(solver, thy).getOrElse(return None)}
      var mts: List[MPath] = Nil
      var decls: List[OML] = Nil
      thysAnon.foreach {at =>
          at.mt.foreach {mts ::= _}
          decls = decls ::: at.decls
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
    val thyAnon = Common.asAnonymousTheory(solver, thy).getOrElse {return None}
    // perform the renaming
    val oldNew = rens.flatMap {
      case OML(nw, None, Some(OML(old, None,None,_,_)),_,_) =>
        List((old,nw))
      case OML(nw,None,Some(OMS(old)),_,_) =>
        List((old.name,nw))
      case r =>
        solver.error("not a renaming " + r)
        Nil
    }
    thyAnon.rename(oldNew:_*)
    // remove all invalidated realizations, i.e., all that realized a theory one of whose symbols was renamed
    // TODO more generally, we could keep track of the renaming necessary for this realization, but then realizations cannot be implicit anymore
    val removeReals = thyAnon.decls.flatMap {
      case oml @ RealizeOML(p, _) =>
        solver.lookup(p) match {
          case Some(dt: DeclaredTheory) =>
            if (oldNew.exists {case (old,nw) => dt.declares(old)})
              List(oml)
            else
              Nil
          case _ => Nil
        }
      case _ => Nil
    }
    thyAnon.decls = thyAnon.decls diff removeReals
    Some(thyAnon.toTerm)
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

// TODO does not work yet
object ComputeTranslate extends ComputationRule(Translate.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = {
    val Translate(mor, thy) = tm
    val dom = Morph.domain(mor)(???).getOrElse{return None}
    val cod = Morph.codomain(mor)(???).getOrElse{return None}
    val List(thyAnon,domAnon,codAnon) = List(thy,dom,cod).map {t => Common.asAnonymousTheory(solver, t).getOrElse(return None)}
    // translate all declarations of thy that are not from dom via mor and add them to cod
    def translate(t: Term): Term = ???
    thyAnon.decls.foreach {
      case RealizeOML(p,_) =>
        // these may also be translatable, but they are optional anyway
      case oml =>
        if (! domAnon.isDeclared(oml.name)) {
          val omlT = translate(oml).asInstanceOf[OML]
          codAnon.add(omlT)
        }
    }
    Some(codAnon.toTerm)
  }
}

// TODO better name
/** see [[Translate]] */
object Expand extends BinaryConstantScala(Combinators._path, "expand")

// TODO does not work yet
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
