package info.kwarc.mmt.mitm

import info.kwarc.mmt.api.{DPath, uom}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.refactoring.{Preprocessor, SimpleParameterPreprocessor}
import info.kwarc.mmt.api.uom.{RepresentedRealizedType, StandardInt, StandardNat, StandardPositive}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lf.{ApplySpine, LFClassicHOLPreprocessor}

object MitM {
  val path = DPath(URI("http","mathhub.info") / "MitM" / "Foundation")
  val mathpath = path ? "Math"

  val logic = path ? "Logic"
  // val natliterals = ODK.path ? "Nat"
  // val intliterals = ODK.path ? "Int"
  val literals = path ? "Literals"
  val strings = path ? "Strings"
  val lists = path ? "Lists"
  val vectors = path ? "Vectors"
  val matrices = path ? "Matrices"

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
