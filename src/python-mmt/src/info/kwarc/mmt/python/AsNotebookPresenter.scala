package info.kwarc.mmt.python

import info.kwarc.mmt.api._
import presentation._
import modules._
import documents._
import symbols._
import opaque._
import objects._

class AsNotebookPresenter extends AsNotebookStructurePresenter(new NotationBasedPresenter) {
  override def outExt = "ipynb"
}

/** renders MMT as Jupyter notebooks */
class AsNotebookStructurePresenter(oP: ObjectPresenter) extends Presenter(oP) {   
   val key = "omdoc-jupyter"
   def apply(e : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler) = {
     val cells = doElement(e, 0)
     val firstCell = Notebook.MarkdownCell("# This notebook was automatically generated by MMT from the document " + e.path)
     val nb = MMTNotebook(firstCell::cells)
     rh(nb.toJSON.toFormattedString("  "))
   }
   
   protected def doElement(e: StructuralElement, level: Int) : List[Cell] = {
     e match {
       case d: Document => doDocument(d, level)
       case t: DeclaredTheory => doTheory(t, level)
       case d: Declaration => doDeclaration(d, level)
     }
   }
   protected def doDocument(doc: Document, level: Int): List[Cell] = {
     doNarrativeElementInDoc(doc, level)
   }
   
   protected def doTheory(t: DeclaredTheory, level: Int): List[Cell] = {
     doNarrativeElementInMod(t, t.asDocument, level)
   }
   
   protected def doDeclaration(d: Declaration, level: Int): List[Cell] = {
     val text = d match {
       case c: Constant =>
         var s = c.name.toString
         c.tp.foreach {t => s += " : " + oP.asString(t)}
         c.df.foreach {t => s += " = " + oP.asString(t)}
         // TODO add notations etc.
         s
       case Include(_, p, args) =>
         var s = "include "
         val dom = if (args.isEmpty) p.toString
         else oP.asString(OMA(OMMOD(p), args))
         s + dom
       case d =>
         "// ignored declaration " + d
     }
     makeTitle(d, level).toList ::: List(MMTCell(text))
   }

  /** captures common parts of narrative and content element rendering */
  protected def doNarrativeElement(ne: NarrativeElement, recurse: (NarrativeElement,Int) => List[Cell], level: Int): List[Cell] = {
    var result: List[Cell] = Nil
    ne match {
      case doc: Document =>
        val innerCells = doc.getDeclarations flatMap {d =>
          recurse(d, level+1)
        }
        makeTitle(doc, level).toList ::: innerCells
      case oe: OpaqueElement =>
         val oi: OpaqueTextPresenter = controller.extman.get(classOf[OpaqueTextPresenter], oe.format)
                  .getOrElse(new DefaultOpaqueElementInterpreter)
         val rh = new StringBuilder
         oi.toString(oP, oe)(rh)
         val cell = Notebook.MarkdownCell(rh.get)
         List(cell)
      case r: NRef =>
        val rT = controller.get(r.target)
        doElement(rT, level)
    }
  }

  protected def doNarrativeElementInDoc(ne: NarrativeElement, level:Int): List[Cell] = {
     doNarrativeElement(ne, doNarrativeElementInDoc _, level)
  }
  
  protected def doNarrativeElementInMod(body: Body, ne: NarrativeElement, level: Int): List[Cell] = ne match {
    case r:SRef =>
      val d = body.get(r.target.name)
      doDeclaration(d, level)
    case r: NRef =>
      throw ImplementationError("nref in module") // impossible
    case ne =>
      doNarrativeElement(ne, {case (e,i) => doNarrativeElementInMod(body, e, i)}, level)
  }

  protected def makeTitle(se: StructuralElement, level: Int): Option[Cell] = {
    NarrativeMetadata.title.get(se) match {
      case Some(t) =>
        Some(Notebook.MarkdownCell(utils.repeatString("#", level) + " " + t))
      case None =>
        None
    }
  }
}
