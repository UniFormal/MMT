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
  
   def beginDecl(e: StructuralElement)(implicit rh: RenderingHandler) {}
   def endDecl(e: StructuralElement)(implicit rh: RenderingHandler) {}
   
   def apply(e : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler) {apply(e, 0)(rh)}

   protected def doConstant(c: Constant,indent:Int)(implicit rh: RenderingHandler) = {
      rh("constant " + c.name)
      c.alias foreach {a =>
         rh(" @ ")
         rh(a.toPath)
      }
      c.tp foreach {t =>
         rh("\n")
         doIndent(indent)
         rh("  : ")
         apply(t, Some(c.path $ TypeComponent))
      }
      c.df foreach {t =>
         rh("\n")
         doIndent(indent)
         rh("  = ")
         apply(t, Some(c.path $ DefComponent))
      }
      c.notC.parsing foreach {n =>
         rh("\n")
         doIndent(indent)
         rh("  # ")
         rh(n.toText)
      }
      c.notC.presentation foreach {n =>
         rh("\n")
         doIndent(indent)
         rh("  ## ")
         rh(n.toText)
      }
   }

   protected def doDeclaredStructure(s:DeclaredStructure,indent:Int)(implicit rh: RenderingHandler) = {
      rh("structure " + s.name + " : ")
      apply(s.from, Some(s.path $ TypeComponent))
      rh(" =\n")
      s.getPrimitiveDeclarations.foreach {d => apply(d, indent+1)}
   }

   protected def doIndent(indent:Int)(implicit rh: RenderingHandler) {
      Range(0,indent).foreach {_ => rh("   ")}
   }

   protected def doDeclaredView(v:DeclaredView,indent:Int)(implicit rh: RenderingHandler) = {
      rh("view " + v.name + " : ")
      apply(v.from, Some(v.path $ DomComponent))
      rh(" -> ")
      apply(v.to, Some(v.path $ CodComponent))
      rh(" =\n")
      v.getPrimitiveDeclarations.foreach {d => apply(d, indent+1)}
   }

   //this used to be private; but had to change that for MMTSyntaxPresenter override of doDeclaredStructure
   protected def apply(e : StructuralElement, indent: Int)(implicit rh: RenderingHandler) {
      doIndent(indent)
      beginDecl(e)
      e match {
         //TODO delimiters, metadata
         case d: Document =>
            rh("document " + d.path.toPath + "\n")
            d.getDeclarations foreach {i => apply(i, indent+1)}
         case r: DRef =>
            rh("document " + r.target.toPath)
         case r: MRef =>
            rh("module " + r.target.toPath)
         case c: Constant => doConstant(c,indent)
         case t: DeclaredTheory =>
            rh("theory " + t.name)
            t.meta.foreach(p => rh(" : "+p.toString))
            rh(" =\n")
            t.getDeclarations.foreach {d => apply(d, indent+1)}
         case v: DeclaredView => doDeclaredView(v,indent)
         case nm: NestedModule =>
            apply(nm.module, indent+1)
         case s: DeclaredStructure => doDeclaredStructure(s,indent)
         case t: DefinedTheory =>
            rh("theory " + t.name + " abbrev ")
            apply(t.df, Some(t.path $ DefComponent))
         case v: DefinedView =>
            rh("view " + v.name + " abbrev ")
            apply(v.df, Some(v.path $ DefComponent))
         case s: DefinedStructure =>
            rh("structure " + s.name + " : ")
            apply(s.from, Some(s.path $ TypeComponent))
            rh(" abbrev ")
            apply(s.df, Some(s.path $ DefComponent))
      }
      endDecl(e)
      rh("\n")
   }
}
