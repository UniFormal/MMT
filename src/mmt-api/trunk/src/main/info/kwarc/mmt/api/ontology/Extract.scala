package info.kwarc.mmt.api.ontology
import info.kwarc.mmt.api._
import modules._
import symbols._
import patterns._
import objects._

/** The Extractor produces the declaration-level relational representation of a SructuralElement
 */
object Extractor {
   /** apply a continuation function to every relational element of a StructuralElement */
   def apply(e: StructuralElement)(implicit f: RelationalElement => Unit) {
      val path = e.path
      e match {
         case t: Theory =>
            f(IsTheory(path))
            t match {case t: DeclaredTheory => t.meta map {p => f(HasMeta(path, p))}}
         case v: View =>
            f(HasDomain(path, v.from.toMPath))
            f(HasCodomain(path, v.to.toMPath))
            f(IsView(path))
         case _ => 
      }
      e match {
         case t: DeclaredModule[_] =>
            t.valueList foreach {d => {
               val dec = Declares(path,d.path)
               d match {
                  case c: Constant =>
                     f(dec)
                     f(IsConstant(c.rl).apply(c.path))
                  case s: Structure =>
                     if (s.name.isAnonymous) {
                        f(Includes(t.path, TheoryExp.simplify(s.from).toMPath))
                     } else {
                        f(dec)
                        f(HasDomain(s.path, TheoryExp.simplify(s.from).toMPath))
                        f(HasCodomain(s.path, TheoryExp.simplify(s.to).toMPath))
                        f(IsStructure(s.path))
                     }
                  case p: Pattern =>
                     f(dec)
                     f(IsPattern(p.path))
                  case i: Instance => 
                     f(dec)
                     f(IsInstance(i.path))
                     f(IsInstanceOf(i.path, i.pattern))
                  case a: Alias =>
                     f(IsAliasFor(a.path, a.forpath))
                     f(dec)
                  case _: Assignment =>
                     f(dec)
               }
            }}
         case _ =>
      }
   }
}