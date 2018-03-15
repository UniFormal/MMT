package info.kwarc.mmt.reflection

import info.kwarc.mmt.api._
import uom._
import objects._
import objects.Conversions._

object Terms extends TheoryScala {
  val _base = DPath(utils.URI("http", "cds.omdoc.org") / "urtheories" / "reflection")
  val _name = LocalName("Terms")

  object formation extends ConstantScala {
    val parent = _path
    val name = "formation"
    def apply(p: MPath, t: Term) = OMBINDC(OMID(this.path), Context(p), List(t))
    def unapply(t: Term): Option[(MPath, Term)] = t match {
      case OMBINDC(OMID(this.path), Context(IncludeVarDecl(_, OMMOD(p),_)), List(t)) => Some((p, t))
      case _ => None
    }
  }

  object refl extends ConstantScala {
    val parent = _path
    val name = "refl"
    def apply(p: MPath, t: Term) = OMBINDC(OMID(this.path), Context(p), List(t))
    def unapply(t: Term): Option[(MPath, Term)] = t match {
      case OMBINDC(OMID(this.path), Context(IncludeVarDecl(_, OMMOD(p),_)), List(t)) => Some((p, t))
      case _ => None
    }
  }

  object elim extends ConstantScala {
    val parent = _path
    val name = "elim"
    def apply(t: Term, mor: Term) = OMA(OMID(this.path), List(t, mor))
    def unapply(t: Term): Option[(Term, Term)] = t match {
      case OMA(OMID(this.path), List(t, mor)) => Some((t, mor))
      case _ => None
    }
  }

  object eval {
     def apply(t: Term) = elim(t, OMCOMP())
  }
}
