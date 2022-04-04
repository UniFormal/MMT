package info.kwarc.mmt.api.symbols

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.uom._

/**
 * a general purpose term translator
 *
 * There are a number of desirable properties that Translator can have.
 * In particular: preservation of typing, equality; commute with substitution.
 *
 * The functions applyXXX for Def, Type, and Plain are split because many translators are defined via
 * * a compositional applyPlain
 * * non-compositional top-level steps on top of applyPlain that differ between applyDef and applyType
 * A term may occur on both sides of the typing judgment, in which case applyDef and applyType may translate it differently
 */
abstract class Translator {self =>
   /** map terms that occur on the left side of MMT's typing judgment */
   def applyDef(context: Context, tm: Term): Term
   /**
    * map terms that occur on the right side of MMT's typing judgment (i.e., types)
    */
   def applyType(context: Context, tm: Term): Term

   /**
    * map terms that occur in other positions, e.g., arguments to parametric theories
    */
   def applyPlain(context: Context, tm: Term): Term

   /** maps module references, identity by default, can be overridden */
   def applyAtomicModule(p: MPath): MPath = p
   def applyStructure(p: GlobalName): GlobalName = p

   def applyVarDecl(context: Context, vd: VarDecl) = {
     def tr(t: Term) = vd match {
       case IncludeVarDecl(_) => applyModule(context,t)
       case _ => applyPlain(context, t)
     }
     vd.copy(tp = vd.tp map tr, df = vd.df map tr)
   }

   def applySub(context: Context, sb: Sub) = {
     sb match {
       case IncludeSub(p,t) => IncludeSub(p,applyModule(context, t))
       case Sub(n,t) => Sub(n, applyPlain(context,t))
     }
   }

   def applyContext(context: Context, con: Context): Context = con.mapVarDecls {case (c, vd) =>
     val nc = context ++ c
     applyVarDecl(nc, vd)
   }
   def applySubstitution(context: Context, sub: Substitution): Substitution = sub.mapTerms(s => applyPlain(context,s))

   def applyModule(context: Context, tm: Term): Term = {
     tm match {
       case OMPMOD(p, args) => OMPMOD(applyAtomicModule(p), args.map(a => applyPlain(context, a)))
       case OMS(p) => OMS(applyStructure(p))
       case ComplexTheory(cont) => ComplexTheory(applyContext(context, cont))
       case ComplexMorphism(sub) => ComplexMorphism(applySubstitution(context, sub))
       case OMIDENT(t) => OMIDENT(applyModule(context, t))
       case OMCOMP(ms) => OMCOMP(ms.map(a => applyModule(context,a)))
       case OMINST(f,t,args) => OMINST(applyAtomicModule(f), applyAtomicModule(t), args.map(a => applyPlain(context,a)))
       case OMStructuralInclude(f,t) => OMStructuralInclude(applyAtomicModule(f), applyAtomicModule(t))
     }
   }

   /**
    * not all rules can be translated generically
    * this method implements only those cases for which a generic translation is possible
    * implementing classes should override this method if they can translate more rules
    */
   def applyRule(r: Rule) = r match {
     case r: RealizedType => new RealizedType(applyType(Context.empty, r.synType), r.semType)
     case _ => throw GeneralError("untranslatable rule")
   }

   /** diagrammatic composition (first this, then that) */
   def compose(that: Translator) = new Translator {
     def applyPlain(con: Context, tm: Term) = that.applyPlain(self.applyContext(Context.empty, con), self.applyPlain(con, tm))
     def applyDef(con: Context, tm: Term) = that.applyDef(self.applyContext(Context.empty, con), self.applyDef(con, tm))
     def applyType(con: Context, tm: Term) = that.applyType(self.applyContext(Context.empty, con), self.applyType(con, tm))
     override def applyAtomicModule(p: MPath) = that.applyAtomicModule(self.applyAtomicModule(p))
   }
}

/** a translator that maps all terms in the same way (i.e., applyDef and applyType are the same) */
abstract class UniformTranslator extends Translator {
   def applyType(context: Context, tm: Term) = applyPlain(context, tm)
   def applyDef(context: Context, tm: Term) = applyPlain(context, tm)
   // We add the apply method in this class only to avoid accidentally calling the wrong method on a general Translator.
   def apply(context: Context, tm: Term) = applyPlain(context, tm)
}

/** identity (non-traversing) */
object IdentityTranslator extends UniformTranslator {
  def applyPlain(context: Context, tm: Term) = tm
}

/** a translator obtained from a traverser */
abstract class TraversingTranslator extends UniformTranslator {
  val trav: StatelessTraverser
  def applyPlain(context: Context, tm: Term) = trav(tm, context)
}

object TraversingTranslator {
  def apply(t: StatelessTraverser) = new TraversingTranslator {val trav = t}
}

/** a translator that applies a morphism (lazily) */
case class ApplyMorphismLazy(morph: Term) extends UniformTranslator {
   def applyPlain(context: Context, tm: Term) = OMM(tm, morph)
}

/** a translator that applies a morphism */
case class ApplyMorphism(lup: Lookup, morph: Term) extends UniformTranslator {
   def applyPlain(context: Context, tm: Term) = lup.ApplyMorphs(tm, morph)
}

/** a translator that performs substitution */
case class ApplySubs(subs: Substitution) extends UniformTranslator {
  def applyPlain(context: Context, tm: Term) = tm ^? subs
}

/** replaces all naked OML's; for convenience a substitution is used even though we are replacing OML's not OMV's */
class OMLReplacer(replace: LocalName => Option[Term]) extends StatelessTraverser {
  def traverse(t: Term)(implicit con : Context, state : State): Term = t match {
    case OML(n, None, None, None, None) =>
      replace(n) match {
        case None => t
        case Some(r) => r
      }
    case t => Traverser(this,t)
  }
}

object OMLReplacer {
  def apply(replacements: Substitution) = new OMLReplacer(r => replacements(r))
}

/**
  * A traverser to replace references to [[GlobalName]]s by a custom term
  */
abstract class OMSReplacer extends StatelessTraverser {
  /**
    * replace(p) = Some(t): OMS(p) ---> t; otherwise OMS(p) ---> OMS(p)
    */
  def replace(p: GlobalName): Option[Term]
  def traverse(t: Term)(implicit con : Context, state : State): Term = t match {
    case OMS(p) =>
      replace(p) match {
        case None => t
        case Some(r) => r
      }
    case t => Traverser(this,t)
  }
}

object OMSReplacer {
  /**
    * returns an [[OMSReplacer]] given by the specified replacer function
    */
  def apply(r: GlobalName => Option[Term]): OMSReplacer = (p: GlobalName) => r(p)
}

/** a translator that renames local names of a module */
class Renamer(rename: GlobalName => Option[GlobalName]) extends OMSReplacer {
  def replace(p: GlobalName) = rename(p) map {pR => OMS(pR)}
}

object Renamer {
  /** convenience for creating [[Renamer]]s */
  def apply(r: GlobalName => Option[GlobalName]) = new Renamer(r)
  /** a [[Renamer]] of finitely many symbols */
  def apply(rs: (GlobalName,GlobalName)*): Renamer = Renamer {n =>
    rs.toList.find(_._1 == n) map {r => r._2}
  }
  /** a [[Renamer]] that prefixes all names */
  def prefix(m: MPath, prefix: GlobalName) = Renamer {p =>
    if (p.module == m) Some(prefix / p.name) else None
  }
}
