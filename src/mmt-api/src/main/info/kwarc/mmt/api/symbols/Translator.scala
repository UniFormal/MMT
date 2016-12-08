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
abstract class Translator {
   /** map terms that occur on the left side of MMT's typing judgment */
   def applyDef(context: Context, tm: Term): Term
   /**
    * map terms that occur on the right side of MMT's typing judgment (i.e., types)
    * note that the same term may occur on both sides and thus be translated differently depending on where it occurs
    */
   def applyType(context: Context, tm: Term): Term
   
   def applyContext(con: Context): Context = con.mapVarDecls {case (c, VarDecl(n,tp,df,nt)) =>
     VarDecl(n, tp map {t => applyType(c,t)}, df map {t => applyDef(c,t)}, nt)
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
     def applyDef(con: Context, tm: Term) = that.applyDef(this.applyContext(con), this.applyDef(con, tm))
     def applyType(con: Context, tm: Term)  = that.applyType(this.applyContext(con), this.applyType(con, tm))
   }
}

/** a translator that maps all terms in the same way (i.e., applyDef and applyType are the same) */
abstract class UniformTranslator extends Translator {
   def apply(context: Context, tm: Term): Term

   def applyType(context: Context, tm: Term) = apply(context, tm)
   def applyDef(context: Context, tm: Term) = apply(context, tm)
}

/** a translator that applies a morphism (lazily) */
case class ApplyMorphism(morph: Term) extends UniformTranslator {
   def apply(context: Context, tm: Term) = tm * morph
}

/** a translator that renames local names of a module */
abstract class Renamer extends UniformTranslator {
  def rename(old: GlobalName): Option[GlobalName]
  private object trav extends StatelessTraverser {
    def traverse(t: Term)(implicit con : Context, state : State) = t match {
      case OMS(p) => rename(p) match {
        case None => t
        case Some(pR) => OMS(pR)
      }
      case _ => Traverser(this, t)
    }
  }
  def apply(c:Context, t: Term) = trav(t, c)
}

object Renamer {
  /** convenience for creating [[Renamer]]s */
  def apply(r: GlobalName => Option[GlobalName]) = new Renamer {
    def rename(n: GlobalName) = r(n)
  }
  /** a [[Renamer]] of finitely many symbols */
  def apply(rs: (GlobalName,GlobalName)*) = new Renamer {
    def rename(n: GlobalName) = rs.toList.find(_._1 == n) map {r => r._2}
  }
  /** a [[Renamer]] that prefixes all names */
  def prefix(m: MPath, prefix: GlobalName) = Renamer {p =>
    if (p.module == m) Some(prefix / p.name) else None
  }
}

case class ApplySubs(subs: Substitution) extends UniformTranslator {
  def apply(context: Context, tm: Term) = tm ^? (subs ++ context.id)
}