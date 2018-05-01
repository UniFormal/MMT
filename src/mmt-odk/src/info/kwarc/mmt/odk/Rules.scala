package info.kwarc.mmt.odk

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.checking.{History, Solver, SubtypingRule}
import objects._
import uom._
import utils._
import info.kwarc.mmt.lf._
import info.kwarc.mmt.odk.Singular.SingularImporter
import info.kwarc.mmt.mitm.MitM._
import SemanticOperator._
import info.kwarc.mmt.mitm.MitM

object IntegerLiterals extends RepresentedRealizedType(z,Z)
object NatLiterals extends RepresentedRealizedType(n,N)
object PosLiterals extends RepresentedRealizedType(p,P)

object NatSucc extends RealizedOperator(succ, n =>: n, Arithmetic.Succ, N =>: N)

object NatSuccInverse extends InverseOperator(MitM.succ) {
  def unapply(l: OMLIT): Option[List[OMLIT]] = l match {
    case NatLiterals(u : BigInt) if u>0 => Some(List(NatLiterals.of(u-1)))
    case _ => None
  }
}

object StringLiterals extends RepresentedRealizedType(OMS(MitM.string),StandardString)

object IntegerSubtype extends SubtypingRule {
  val head = MitM.int
  def applicable(tp1: Term, tp2: Term): Boolean = (tp1,tp2) match {
    case (NatLiterals.synType,IntegerLiterals.synType) => true
    case _ => false
  }

  def apply(solver: Solver)(tp1: Term, tp2: Term)(implicit stack: Stack, history: History) : Option[Boolean] = (tp1,tp2) match {
    case (NatLiterals.synType,IntegerLiterals.synType) => Some(true)
    case _ => None
  }
}

object LFX {
  class LFRecSymbol(name:String) {
    val path = (DPath(URI.http colon "gl.mathhub.info") / "MMT" / "LFX" / "Records") ? "Symbols" ? name
    val term = OMS(path)
  }

  case class RecordBody(self: Option[LocalName], fields: List[OML]) {
    /** names of all fields */
    def names = fields.map(_.name)
    /** checks for duplicate names */
    def hasDuplicates = utils.hasDuplicates(names)
    /** retrieve a field for a given name */
    def get(l: LocalName) = fields.find(_.name == l)
  }

  /** unifies record terms and types; the empty record is OMA(this.term,Nil), not this.term */
  class RecordLike(n: String) extends LFRecSymbol(n) {
    // there may not be an apply method that takes a context instead of a List[OML]
    def apply(v:OML*): Term = apply(None, v.toList)
    def apply(self: Option[LocalName], fields: List[OML]): Term = {
      self match {
        case None => OMA(this.term, fields)
        case Some(l) => OMBINDC(this.term, Context(VarDecl(l)), fields)
      }
    }
    def unapply(t : Term) : Option[RecordBody] = t match {
      case OMA(this.term, OMLList(fs)) => Some(RecordBody(None, fs))
      case OMBINDC(this.term, Context(VarDecl(n, None, None, None, _), rest@_*), OMLList(fs)) if rest.isEmpty => Some(RecordBody(Some(n), fs))
      case _ => None
    }
  }

  object RecExp extends RecordLike("Recexp")
}
