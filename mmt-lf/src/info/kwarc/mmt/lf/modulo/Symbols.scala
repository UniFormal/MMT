package info.kwarc.mmt.lf.modulo
import info.kwarc.mmt.api._
import objects._

object LFModulo {
  val _base = DPath(utils.URI("http", "cds.omdoc.org") / "urtheories")
  val _path = _base ? "LFModulo"

  object Rewrite {
    val parent = _path
    val name = "Rewrite"
    val path = parent ? name
    def apply(cont: Context, left: Term, right: Term) = OMBINDC(OMID(this.path), cont, List(left, right))
    def unapply(t: Term): Option[(Context, Term, Term)] = t match {
      case OMBINDC(OMID(this.path), cont, List(left, right)) => Some((cont, left, right))
      case _ => None
    }
  }
}