package info.kwarc.mmt.api.presentation

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents.{MRef, DRef, Document}
import info.kwarc.mmt.api.modules.{DefinedView, DefinedTheory, DeclaredView, DeclaredTheory}
import info.kwarc.mmt.api.parser.Reader
import info.kwarc.mmt.api.symbols.{DefinedStructure, DeclaredStructure, NestedModule, Constant}

/**
 * Basically a copy of MMTStructurePresenter(NotationBasedPresenter), that should yield parsable MMT Code
 */

class MMTSyntaxPresenter(objectPresenter: ObjectPresenter = new NotationBasedPresenter) extends MMTStructurePresenter(objectPresenter) {
  override def endDecl(e: StructuralElement)(implicit rh: RenderingHandler) = e match {
    case x: Document            => rh(Reader.GS.toChar.toString) // check?
    case x: DRef                => rh(Reader.GS.toChar.toString) // check?
    case x: MRef                => rh(Reader.GS.toChar.toString) // check?
    case x: Constant            => rh(Reader.RS.toChar.toString)
    case x: DeclaredTheory      => rh(Reader.GS.toChar.toString)
    case x: DeclaredView        => rh(Reader.GS.toChar.toString)
    case x: NestedModule        => { }                           // check?
    case x: DeclaredStructure   => { }
    case x: DefinedTheory       => rh(Reader.RS.toChar.toString) // check?
    case x: DefinedView         => rh(Reader.RS.toChar.toString) // check?
    case x: DefinedStructure    => rh(Reader.RS.toChar.toString) // check?
  }
  override def doConstant(c:Constant,indent:Int)(implicit rh: RenderingHandler) = {
    rh("" + c.name.last)
    c.alias foreach {a =>
      rh(" @ ")
      rh(a.toPath)
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
  override def doDeclaredView(v:DeclaredView,indent:Int)(implicit rh: RenderingHandler) = {
    rh("view " + v.name + " : ")
    this.apply(v.from, Some(v.path $ DomComponent))
    rh(" -> ")
    this.apply(v.to, Some(v.path $ CodComponent))
    rh(" =\n")
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

  override def doDeclaredStructure(s:DeclaredStructure,indent:Int)(implicit rh: RenderingHandler) = {
    val decs = s.getPrimitiveDeclarations
    if (decs.isEmpty) {
      rh("include ")
      this.apply(s.from, Some(s.path $ TypeComponent))
      rh(Reader.RS.toChar.toString)
    } else {
      rh("structure " + s.name + " : "+s.from.toMPath.^^.last+"?"+s.from.toMPath.last)
      //this.apply(s.from, Some(s.path $ TypeComponent))
      rh(Reader.US.toChar.toString+ " =\n")
      decs.foreach {d => this.apply(d, indent+1)}
      rh(Reader.GS.toChar.toString)
    }
  }

}
