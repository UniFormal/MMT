/**
  * Various paths and helper objects for MMT theories in the MMT/examples archive
  * revolving around SFOL.
  */

package info.kwarc.mmt.moduleexpressions

import info.kwarc.mmt.api.uom.TheoryScala
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.{DPath, GlobalName, LocalName}
import info.kwarc.mmt.lf.{BinaryLFConstantScala, TernaryLFConstantScala, TypedBinderScala, UnaryLFConstantScala}

object TypedTerms extends TheoryScala {
  val _base = DPath(URI("http://cds.omdoc.org/examples"))
  val _name = LocalName("TypedTerms")

  // The LF type of sorts in our theory
  val typeOfSorts: GlobalName = _path ? "tp"
}

/** MMT declarations
  *
  *  namespace http://cds.omdoc.org/examples
  *  fixmeta ?LF
  *
  *  theory PL =
  *    prop : type
  *    ded : prop -> type
  *
  *  theory SFOL =
  *    sort : type
  *    term : sort -> type
  */

object PL extends TheoryScala {
  val _base = DPath(URI("http://cds.omdoc.org/examples"))
  val _name = LocalName("PL")
  val prop: GlobalName = _path ? "prop"
  object ded extends UnaryLFConstantScala(_path, "ded")
  object implies extends BinaryLFConstantScala(_path, "impl")
}

object SFOL extends TheoryScala {
  val _base = DPath(URI("http://cds.omdoc.org/examples"))
  val _name = LocalName("SFOL")
  val sort: GlobalName = _path ? "sort"
  /*
  object forall{
      val _path = SFOL._path ? "forall"
      def apply(context: Context,body : Term): Term ={
        OMBIND(OMS(_path),context,body)
        Apply(OMS(path), Lambda(..)
      }
      def unapply(t : Term): Option[(Term,Context,Term)] = t match {
        case OMBIND(binder,context,body) => Some((binder,context,body))
        case _ => None
      }
  }
  */
  object term extends UnaryLFConstantScala(_path, "term")
  object equal extends TernaryLFConstantScala(_path, "equal")
  object forall extends TypedBinderScala(_path, "forall", term)
  object exists extends TypedBinderScala(_path, "exists", term)
}