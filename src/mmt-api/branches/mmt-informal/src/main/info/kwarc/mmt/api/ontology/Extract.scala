package info.kwarc.mmt.api.ontology
import info.kwarc.mmt.api._
import documents._
import modules._
import symbols._
import patterns._
import objects._
import flexiformal._

/** The Extractor produces the declaration-level relational representation of a SructuralElement
 */
object Extractor {
   /** apply a continuation function to every relational element of a StructuralElement */
   def apply(e: StructuralElement)(implicit f: RelationalElement => Unit) {
      val path = e.path
      e match {
         case d: Document =>
            f(IsDocument(d.path))
            //TODO should getLocalItems but then it wouldn't work for documents created from folders
            d.getItems.foreach {x =>
               f(Declares(d.path, x.target))
            }
         case t: Theory =>
            f(IsTheory(path))
            t match {
               case t: DeclaredTheory =>
                  t.meta foreach {p => f(HasMeta(path, p))}
               case _ => 
            }
         case v: View =>
            f(HasDomain(path, v.from.toMPath))
            f(HasCodomain(path, v.to.toMPath))
            f(IsView(path))
         case _ => 
      }
      e match {
         case t: DeclaredModule =>
            t.getDeclarations foreach {d => {
               val dec = Declares(path,d.path)
               d match {
                  case c: Constant =>
                     f(dec)
                     f(IsConstant(c.path))
                     c.alias foreach {a =>
                       f(IsAliasFor(c.home % a, c.path))
                     }
                  case s: Structure =>
                     if (s.isAnonymous) {
                        f(Includes(t.path, TheoryExp.simplify(s.from).toMPath))
                     } else {
                        f(dec)
                        f(HasDomain(s.path, TheoryExp.simplify(s.from).toMPath))
                        f(HasCodomain(s.path, TheoryExp.simplify(s.to).toMPath))
                        f(IsStructure(s.path))
                     }
                  case nm: NestedModule => apply(nm.module)
                  case p: Pattern =>
                     f(dec)
                     f(IsPattern(p.path))
                  case i: Instance => 
                     f(dec)
                     f(IsInstance(i.path))
                     f(IsInstanceOf(i.path, i.pattern))
                  case fd : FlexiformalDeclaration => 
                    fd match {
                      case  p : PlainNarration => //nothing to do
                      case  d : Definition => 
                        d.targets foreach {target => 
                          f(isDefinedBy(target, d.fragPath))
                        }
                    
                    }
               }
            }}
         case _ =>
      }
   }
}