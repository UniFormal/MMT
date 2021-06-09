package info.kwarc.mmt.lf

import info.kwarc.mmt.api.{DPath, LocalName}
import info.kwarc.mmt.api.objects.{OMLIT, Term, UnknownOMLIT}
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
  def unapply(t: Term): Option[String] = t match {
    case OMLIT(str: String, RealizedType(string.term, StandardString)) => Some(str)
    // Literals in surface syntax are parsed as UnknownOMLITs (see docs of that class)
    // and persisted as those into OMDoc.
    // Thus, when reading directly from OMDoc (in contrast to reading from memory after
    // typechecking or mmt-omdoc building), we will face this case.
    case UnknownOMLIT(str, string.term) => Some(str)
    case _ => None
  }
}