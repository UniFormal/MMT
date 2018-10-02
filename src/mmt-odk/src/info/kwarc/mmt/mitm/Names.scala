package info.kwarc.mmt.mitm

import info.kwarc.mmt.api.{DPath, MPath, uom}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.refactoring.{Preprocessor, SimpleParameterPreprocessor}
import info.kwarc.mmt.api.uom.{RepresentedRealizedType, StandardInt, StandardNat, StandardPositive}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lf.{ApplySpine, LFClassicHOLPreprocessor}
import info.kwarc.mmt.odk.LFX.LFRecSymbol

object MitM {
  val basepath = DPath(URI("http","mathhub.info") / "MitM")
  val path = basepath / "Foundation"
  val mathpath = path ? "Math"

  val logic = path ? "Logic"
  // val natliterals = ODK.path ? "Nat"
  // val intliterals = ODK.path ? "Int"
  val literals = path ? "Literals"
  val strings = path ? "Strings"
  val lists = path ? "Lists"
  val vectors = path ? "Vectors"
  val matrices = path ? "Matrices"

  val polypath = (basepath / "smglom" / "elliptic_curves") ? "Base"
  val polynomials = polypath ? "polynomial"
  val polycons = polypath ? "poly_con"

  // val tms = typesystem ? "tm"
  val bool = logic ? "bool"
  val BoolLit = new RepresentedRealizedType(OMS(bool),uom.StandardBool)
  val tt = BoolLit(true)
  val ff = BoolLit(false)
  val int = literals ? "int_lit"
  val nat = literals ? "nat_lit"
  val pos = literals ? "pos_lit"
  val succ = literals ? "nat_lit_succ"
  val string = strings ? "string"
  val list = lists ? "list"
  val nil = lists ? "nil"
  val cons = lists ? "cons"
  val vector = vectors ? "vector"
  val zerovec = vectors ? "zerovec"
  val vectorprepend = vectors ? "vector_prepend"
  val matrix = matrices ? "matrix"
  val matrixconst = matrices ? "matrix_const"

  val n = OMS(nat)
  val z = OMS(int)
  val p = OMS(pos)
  val N = StandardNat
  val Z = StandardInt
  val P = StandardPositive

  val ded = logic ? "ded"
  val not = logic ? "not"
  val and = logic ? "and"
  val or = logic ? "or"
  val implies = logic ? "implies"
  val equiv = logic ? "iff"
  val forall = logic ? "forall"
  val exists = logic ? "exists"
  val eq = logic ? "eq"

  val implicitProof = logic ? "ImplicitProof"

  private object EliminateImplicits extends Preprocessor {
    val trav = new StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
        case ApplySpine(OMS(`implicitProof`),_) => OMS(implicitProof)
        case _ => Traverser(this,t)
      }
    }
    override protected def doTerm(tm: Term): Term = super.doTerm(tm)
  }

  val preproc = (SimpleParameterPreprocessor + info.kwarc.mmt.api.refactoring.DefinitionExpander + EliminateImplicits +
    new LFClassicHOLPreprocessor(
    ded = MitM.ded,
    and = MitM.and,
    not = MitM.not,
    or = Some(MitM.or),
    implies = Some(MitM.implies),
    equiv = Some(MitM.equiv),
    forall = Some(MitM.forall),
    exists = Some(MitM.exists),
    equal = Some(eq)
  )).withKey("MitM").withKey(logic)
}

object ModelsOf extends LFRecSymbol("ModelsOf") {
  // val path2 = Records.path ? "ModelsOfUnary"
  // val term2 = OMS(path2)
  def apply(mp : MPath, args : Term*) = OMA(this.term,List(OMPMOD(mp,args.toList)))
  def apply(t : Term) = OMA(this.term,List(t))
  def unapply(t : Term) : Option[Term] = t match {
    case OMA(this.term, List(tm)) => Some(tm)
    // case OMA(this.term2,List(OMMOD(mp))) => Some(OMMOD(mp))
    case OMA(this.term, OMMOD(mp) :: args) => Some(OMPMOD(mp,args))
    case _ => None
  }
}