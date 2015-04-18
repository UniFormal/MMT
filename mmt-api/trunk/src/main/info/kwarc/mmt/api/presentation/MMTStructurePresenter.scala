package info.kwarc.mmt.api.presentation

import info.kwarc.mmt.api._
import symbols._
import patterns._
import objects._
import documents._
import modules._
import symbols._
import objects.Conversions._
import notations._
import parser.SourceRef

/**
 * standard structure text syntax for MMT, not necessarily parsable
 */
class MMTStructurePresenter(objectPresenter: ObjectPresenter) extends Presenter(objectPresenter) {
   val key = "present-text-notations"
   override def outExt = "mmt"
  
   def apply(e : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler) {apply(e, 0)(rh)}
   
   private def apply(e : StructuralElement, indent: Int)(implicit rh: RenderingHandler) {
      def doIndent {
         Range(0,indent).foreach {_ => rh("   ")}
      }
      doIndent
      e match {
         //TODO delimiters
         case d: Document =>
            rh("document " + d.path.toPath + "\n")
            d.getItems foreach {i => apply(i, indent+1)}
         case r: DRef =>
            rh("document " + r.target.toPath)
         case r: MRef =>
            rh("module " + r.target.toPath)
         case c: Constant =>
            rh("constant " + c.name)
            c.alias foreach {a =>
               rh(" @ ")
               rh(a.toPath)
            }
            c.tp foreach {t =>
               rh("\n")
               doIndent
               rh("  : ")
               apply(t, Some(c.path $ TypeComponent))
            }
            c.df foreach {t =>
               rh("\n")
               doIndent
               rh("  = ")
               apply(t, Some(c.path $ DefComponent))
            }
            c.notC.parsing foreach {n =>
               rh("\n")
               doIndent
               rh("  # ")
               rh(n.toText)
            }
            c.notC.presentation foreach {n =>
               rh("\n")
               doIndent
               rh("  ## ")
               rh(n.toText)
            }
         case t: DeclaredTheory =>
            rh("theory " + t.name + " =\n")
            t.getPrimitiveDeclarations.foreach {d => apply(d, indent+1)}
         case v: DeclaredView =>
            rh("view " + v.name + " : ")
            apply(v.from, Some(v.path $ DomComponent))
            rh(" -> ")
            apply(v.to, Some(v.path $ CodComponent))
            rh(" =\n")
            v.getPrimitiveDeclarations.foreach {d => apply(d, indent+1)}
         case nm: NestedModule =>
            apply(nm.module, indent+1)
         case s: DeclaredStructure =>
            rh("structure " + s.name + " : ")
            apply(s.from, Some(s.path $ TypeComponent))
            rh(" =\n")
            s.getPrimitiveDeclarations.foreach {d => apply(d, indent+1)}
         case t: DefinedTheory =>
            rh("theory " + t.name + " abbrev ")
            apply(t.df, Some(t.path $ DefComponent))
         case v: DefinedView =>
            rh("view " + v.name + " abbrev ")
            apply(v.df, Some(v.path $ DefComponent))
         case s: DefinedStructure =>
            rh("structure " + s.name + " : ")
            apply(s.from, Some(s.path $ TypeComponent))
            rh(" abbrev")
            apply(s.df, Some(s.path $ DefComponent))
      }
      rh("\n")
   }
}
