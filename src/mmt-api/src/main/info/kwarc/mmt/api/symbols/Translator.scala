package info.kwarc.mmt.api.symbols

import info.kwarc.mmt.api._
import objects._
import uom._

/**
 * a general purpose term translator
 *
 * There are a number of desirable properties that Translator can have.
 * In particular: preservation of typing, equality; commute with substitution.
 */
abstract class Translator {self =>
   /** map terms that occur on the left side of MMT's typing judgment */
   def applyDef(context: Context, tm: Term): Term
   /**
    * map terms that occur on the right side of MMT's typing judgment (i.e., types)
    * note that the same term may occur on both sides and thus be translated differently depending on where it occurs
    */
   def applyType(context: Context, tm: Term): Term

   def applyVarDecl(context: Context, vd: VarDecl) = vd.copy(tp = vd.tp map {t => applyType(context,t)}, df = vd.df map {t => applyDef(context,t)})

   def applyContext(context: Context, con: Context): Context = con.mapVarDecls {case (c, vd) =>
     val nc = context ++ c
     applyVarDecl(nc, vd)
   }

   def applyModule(context: Context, tm: Term): Term = applyDef(context, tm)

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
     def applyDef(con: Context, tm: Term) = that.applyDef(self.applyContext(Context.empty, con), self.applyDef(con, tm))
     def applyType(con: Context, tm: Term) = that.applyType(self.applyContext(Context.empty, con), self.applyType(con, tm))
   }
}

/** a translator that maps all terms in the same way (i.e., applyDef and applyType are the same) */
abstract class UniformTranslator extends Translator {
   def apply(context: Context, tm: Term): Term

   def applyType(context: Context, tm: Term) = apply(context, tm)
   def applyDef(context: Context, tm: Term) = apply(context, tm)
}

/** a translator obtained from a traverser */
abstract class TraversingTranslator extends UniformTranslator {
  val trav: StatelessTraverser
  def apply(context: Context, tm: Term) = trav(tm, context)
}

object TraversingTranslator {
  def apply(t: StatelessTraverser) = new TraversingTranslator {val trav = t}
}

/** a translator that applies a morphism (lazily) */
case class ApplyMorphism(morph: Term) extends UniformTranslator {
   def apply(context: Context, tm: Term) = tm * morph
}

/** a translator that performs substitution */
case class ApplySubs(subs: Substitution) extends UniformTranslator {
  def apply(context: Context, tm: Term) = tm ^? subs
}

/** replaces all naked OML's; for convenience a substitution is used even though we are replacing OML's not OMV's */
class OMLReplacer(replacements: Substitution) extends StatelessTraverser {
  def traverse(t: Term)(implicit con : Context, state : State) = t match {
    case OML(n, None, None, None, None) =>
      replacements(n) match {
        case None => t
        case Some(r) => r
      }
    case t => Traverser(this,t)
  }
}

/** fully applies a morphism that is given as a Scala function */
abstract class OMSReplacer extends StatelessTraverser {
  def replace(p: GlobalName): Option[Term]
  def traverse(t: Term)(implicit con : Context, state : State) = t match {
    case OMS(p) =>
      replace(p) match {
        case None => t
        case Some(r) => r
      }
    case t => Traverser(this,t)
  }
}

object OMSReplacer {
  def apply(r: GlobalName => Option[Term]) = new OMSReplacer {
    def replace(p: GlobalName) = r(p)
  }
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
