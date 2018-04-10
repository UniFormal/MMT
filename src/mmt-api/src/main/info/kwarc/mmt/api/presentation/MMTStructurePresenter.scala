package info.kwarc.mmt.api.presentation

import info.kwarc.mmt.api._
import symbols._
import patterns._
import objects._
import documents._
import info.kwarc.mmt.api.opaque.{OpaqueElement, OpaqueTextPresenter}
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

   def apply(e : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler) {
     controller.simplifier(e) //TODO simplifying here is bad for elements that are not part of the diagram yet
     apply(e, 0)(rh)
   }

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
         case oe: OpaqueElement =>
            controller.extman.get(classOf[OpaqueTextPresenter], oe.format)
         case c: Constant => doConstant(c,indent)
         case t: DeclaredTheory =>
            //TODO this ignores all narrative structure inside a theory
            rh("theory " + t.name)
            t.meta.foreach(p => rh(" : "+p.toString))
            rh(" =\n")
            t.getDeclarations.foreach {d => apply(d, indent+1)}
         case v: DeclaredView => doDeclaredView(v,indent)
         case dd: DerivedDeclaration =>
            rh << dd.feature + " "
            controller.extman.get(classOf[StructuralFeature], dd.feature) match {
              case None => rh << dd.name + " (implementation is not known)"
              case Some(sf) =>
                val header = sf.makeHeader(dd)
                apply(header, Some(dd.path $ TypeComponent))
            }
            rh << "\n"
            dd.module.getDeclarations.foreach {d => apply(d, indent+1)}
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
         case r: RuleConstant =>
            if (r.df.isEmpty) rh("unknown ")
            rh("rule ")
            r.tp foreach {t => apply(t, Some(r.path $ TypeComponent))}
      }
      endDecl(e)
      rh("\n")
   }
}
