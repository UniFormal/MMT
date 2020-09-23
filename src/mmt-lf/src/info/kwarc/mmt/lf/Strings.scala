package info.kwarc.mmt.lf

import info.kwarc.mmt.api.{DPath, LocalName}
import info.kwarc.mmt.api.objects.{OMLIT, Term}
import info.kwarc.mmt.api.uom.{RealizedType, StandardString, TheoryScala}
import info.kwarc.mmt.api.utils.URI

object Strings extends TheoryScala {
  override val _base: DPath = DPath(URI("http://cds.omdoc.org/urtheories"))
  override val _name: LocalName = LocalName("Strings")

  object string extends NullaryLFConstantScala(this._path, "string")

  def apply(str: String): Term = OMLIT(str, RealizedType(string.term, StandardString))

  /**
    * Unapply method for MMT/urtheories string literals as [[Term]]s.
    *
    * @todo Ask Florian where this should go. Ideally either in mmt-lf or into the archive MMT/urtheories
    */
  def unapply(term: Term): Option[String] = term match {
    case OMLIT(str: String, RealizedType(string.term, StandardString)) => Some(str)
    case _ => None
  }
}