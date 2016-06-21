package info.kwarc.mmt.api.symbols

import info.kwarc.mmt.api._
import objects._

/**
 * a general purpose term translator
 * 
 * There are a number of desirable properties that Translator can have.
 * In particular: preservation of typing, equality; commute with substitution.  
 */
abstract class Translator {
   def apply(context: Context, tm: Term): Term
   
   def applyContext(con: Context): Context = con.mapTerms {case (c, t) => apply(c,t)}
   
   /** diagrammatic composition (first this, then that) */
   def compose(that: Translator) = new Translator {
     def apply(con: Context, tm: Term) = that.apply(this.applyContext(con), this.apply(con, tm))
   }
}

/** a translator that applies a morphism (lazily) */
class ApplyMorphism(morph: Term) extends Translator {
   def apply(context: Context, tm: Term) = tm * morph
}