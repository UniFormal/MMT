package info.kwarc.mmt.api.presentation

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents.{DRef, Document, MRef}
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.parser.Reader
import info.kwarc.mmt.api.symbols._

/**
 * Basically a copy of MMTStructurePresenter(NotationBasedPresenter), that should yield parsable MMT Code
 */

class MMTSyntaxPresenter(objectPresenter: ObjectPresenter = new NotationBasedPresenter) extends MMTStructurePresenter(objectPresenter) {
  override def endDecl(e: StructuralElement)(implicit rh: RenderingHandler) = e match {
    case x: Document            => rh(Reader.GS.toChar.toString) // check?
    case x: DRef                => rh(Reader.GS.toChar.toString) // check?
    case x: MRef                => rh(Reader.GS.toChar.toString) // check?
    case x: Constant            => rh(Reader.RS.toChar.toString)
    case x: RuleConstant        => rh(Reader.RS.toChar.toString)
    case x: Theory              => rh(Reader.GS.toChar.toString)
    case x: View                => rh(Reader.GS.toChar.toString)
    case x: NestedModule        => rh(Reader.GS.toChar.toString)                           // check?
    case x: Structure           => rh(Reader.GS.toChar.toString)
  }
  override def doConstant(c:Constant,indent:Int)(implicit rh: RenderingHandler) = {
    rh("" + c.name.last)
    c.alias foreach {a =>
      rh(" @ ")
      rh(a.toPath)
      rh(Reader.US.toChar.toString)
    }
    c.tp foreach {t =>
      rh("\n")
      this.doIndent(indent)(rh)
      rh("  : ")
      this.apply(t, Some(c.path $ TypeComponent))
      rh(Reader.US.toChar.toString)
    }
    c.df foreach {t =>
      rh("\n")
      this.doIndent(indent)(rh)
      rh("  = ")
      this.apply(t, Some(c.path $ DefComponent))
      rh(Reader.US.toChar.toString)
    }
    c.notC.parsing foreach {n =>
      rh("\n")
      this.doIndent(indent)(rh)
      rh("  # ")
      rh(n.toText)
      rh(Reader.US.toChar.toString)
    }
    //probably not necessary?
    /*
    c.notC.presentation foreach {n =>
      rh("\n")
      doIndent(indent)
      rh("  ## ")
      rh(n.toText)
    }
    */
  }
  
  override def doView(v: View,indent:Int)(implicit rh: RenderingHandler) = {
    rh("view " + v.name + " : ")
    this.apply(v.from, Some(v.path $ DomComponent))
    rh(" -> ")
    this.apply(v.to, Some(v.path $ CodComponent))
    doDefComponent(v)
    v.getPrimitiveDeclarations.foreach {d => d match {
      case c:Constant => {
        this.doIndent(indent)
        rh("" + c.name.last)
        c.df foreach {t =>
          rh("  = ")
          this.apply(t, Some(c.path $ DefComponent))
          rh(Reader.RS.toChar.toString+"\n")
        }
      }
      case _ => apply(d, indent+1)
    }}
  }

  override def doStructure(s: Structure,indent:Int)(implicit rh: RenderingHandler) = {
    val decs = s.getPrimitiveDeclarations
    if (decs.isEmpty) {
      rh("include ")
      this.apply(s.from, Some(s.path $ TypeComponent))
      rh(Reader.RS.toChar.toString)
    } else {
      rh("structure " + s.name + " : "+s.from.toMPath.^^.last+"?"+s.from.toMPath.last)
      //this.apply(s.from, Some(s.path $ TypeComponent))
      rh(Reader.US.toChar.toString)
      doDefComponent(s)
      decs.foreach {d => this.apply(d, indent+1)}
      rh(Reader.GS.toChar.toString)
    }
  }
  
  override def doDefComponent(m: ModuleOrLink)(implicit rh: RenderingHandler) = {
    val b = super.doDefComponent(m)
    if (b) rh(Reader.US.toChar.toString)
    b
  }
}
