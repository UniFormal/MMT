package info.kwarc.mmt.LFX.Records

import info.kwarc.mmt.api.DPath
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.LFX.LFX

/** General comments (attached here for lack of a better place)
 * 
 * The word 'record' refers to a record type or term.
 * Both are lists of OML elements, called 'fields'.
 *
 * A field is called abstract if it has no definiens, otherwise concrete.
 * In a record term, all fields must be concrete.
 * In a record type, fields may have type or definiens or both.
 * For consistency reasons, the rule [[RecordTypeTerm]] enforces a low universe for all abstract fields in a record type.
 * This could be relaxed to allow for using records in type theories with higher universes.
 * 
 * The typing relation is structural: a record term R has record type T if R has every field of T with the required type.
 * T-fields with definiens can be omitted in R; if present, they must be equal to the definiens in T.
 * Thus, T is a subtype of T' if it agrees with except for having more fields and definitions than T'.
 * The smallest type of R is R itself, seen as a type; the largest type of R is the empty record type. 
 * The inferred type of a record term is always the fully abstract type though.
 * 
 * Equality of two records depends on the type at which equality is taken: the records must agree in all fields required by the type.
 * In particular, two unequal records may become equal if equality is taken at a larger type.
 * For two fixed records, equality is a monotone function from types (ordered by size) to booleans (ordered by implication);
 * in particular, record equality is not inconsistent with congruence;
 * but downcasting can introduce logical inconsistency even if the casts succeed computationally. 
 * 
 * No name may be used twice in a record.
 * Fields may depend on other fields: fields may use OML(n) (with all other components None) to refer to other fields.
 * The dependency order must be a strict order (irreflexive and transitive, in particular acyclic). 
 * The order of fields in a record does not matter. But for simplicity we require that the order always respects the dependency order of the fields.
 *
 * Because multiple unrelated records may use the same field name, the OML(n) references can be ambiguous.
 * For simplicity, we use the following convention: OML(n) always refers to the field n in the innermost enclosing record;
 * if that record does not have a field n, but an outermore enclosing record does, the reference is still ill-formed.
 * To allow for referencing fields in all places, a record R may start by declaring a variable (usually called 'self') which is bound in all its fields;
 * then the projection OMV(self).OML(n) can be used to unambiguously refer to a field of R from anywhere inside R;
 * OMV(self) is ill-formed when used in any other way inside R.
 * 
 * The self variable can also be used to declare the type resp. a supertype of a record term resp. type.
 * This has the effect of copying over all fields of the self-type into the current record.
 * If the self-type has undefined fields, the record may still declare the same field with a definiens.
 * 
 * Not all features are implemented yet:
 * - subtyping does not work
 * - self may not have a type yet
 */
object Records {
  val baseURI = LFX.ns / "Records"
  val thname = "Symbols"
  val path = baseURI ? thname
  def lfrecsymbol(name : String) = path ? name
}

class LFRecSymbol(name:String) {
  val path = Records.path ? name
  val term = OMS(path)
}

/** unifies the case of bodies of record expressions and types */
case class RecordBody(self: Option[LocalName], fields: List[OML]) {
  /** names of all fields */
  def names = fields.map(_.name)
  /** checks for duplicate names */
  def hasDuplicates = utils.hasDuplicates(names)
  /** retrieve a field for a given name */
  def get(l: LocalName) = fields.find(_.name == l)
}

/** matches for a list of OMLs */
object OMLList {
  def unapply(ts: List[Term]): Option[List[OML]] = {
    val omls = ts map {
      case o:OML => o
      case _ => return None
    }
    Some(omls)
  }
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

object RecType extends RecordLike("Rectype")
object RecExp extends RecordLike("Recexp")

object Getfield extends LFRecSymbol("Getfield") {
  def apply(t:Term, f: LocalName) = OMA(this.term, List(t, OML(f)))
  def unapply(t: Term) : Option[(Term,LocalName)] = t match {
    case OMA(this.term, List(tm, OML(f,_,_,_,_))) => Some((tm, f))
    case _ => None
  }
}