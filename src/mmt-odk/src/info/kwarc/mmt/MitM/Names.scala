package info.kwarc.mmt.MitM

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.refactoring.{Preprocessor, SimpleParameterPreprocessor}
import info.kwarc.mmt.api.uom.{RepresentedRealizedType, StandardInt, StandardNat, StandardPositive}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lf._
import info.kwarc.mmt.odk._
import info.kwarc.mmt.odk.LFX.{LFList, LFRecSymbol}

object MitM {
  val basepath: DPath = DPath(URI("http","mathhub.info") / "MitM")
  private val path = basepath / "Foundation"

  val mathpath: MPath = path ? "Math"

  // logic
  val logic: MPath = path ? "Logic"
  val bool: GlobalName = logic ? "bool"
  val BoolLit = new RepresentedRealizedType(OMS(bool),uom.StandardBool)
  val tt = BoolLit(true)
  val ff = BoolLit(false)

  // elliptic curves
  val polypath: MPath = (basepath / "smglom" / "algebra") ? "Polynomials"
  val polynomials: GlobalName = polypath ? "polynomial"
  val multipoly : GlobalName = polypath ? "multi_polynomial"
  val polycons: GlobalName = polypath.parent ? "RationalPolynomials" ? "poly_con"

  val rationalRing = (basepath / "smglom" / "algebra") ? "RationalField" ? "rationalField"

  object Monomial {
    def apply(vars : List[(String,BigInt)],coeff : BigInt, ring : Term = OMS(MitM.rationalRing)) =
      OMA(OMS(MitM.monomial_con),ring :: LFX.Tuple(LFList(vars.map(p => LFX.Tuple(StringLiterals(p._1),IntegerLiterals(p._2)))),
        IntegerLiterals(coeff)):: Nil)
    def unapply(tm : Term) = tm match {
      case OMA(OMS(MitM.monomial_con),List(ring,LFX.Tuple(LFList(ls),IntegerLiterals(coeff)))) =>
        val ils = ls.map {
          case LFX.Tuple(StringLiterals(x),IntegerLiterals(i)) => (x,i)
          case _ => ???
        }
        Some((ils,coeff,ring))
      case _ => None
    }
  }

  val monomials : GlobalName = polypath ? "monomial"
  val monomial_con : GlobalName = polypath.parent ? "RationalPolynomials" ? "monomial_con"
  val multi_polycon : GlobalName = polypath.parent ? "RationalPolynomials" ? "multi_poly_con"


  // strings
  val strings: MPath = path ? "Strings"
  val string: GlobalName = strings ? "string"

  // lists
  val lists: MPath = path ? "Lists" // unused?

  // Vectors
  val vectors: MPath = path ? "Vectors"
  val vector: GlobalName = vectors ? "vector"
  val zerovec: GlobalName = vectors ? "zerovec"
  val vectorprepend: GlobalName = vectors ? "vector_prepend"


  // matrices
  val matrices: MPath = path ? "Matrices"
  val matrix: GlobalName = matrices ? "matrix"
  val matrixconst: GlobalName = matrices ? "matrix_const"

  // literals
  private val intliterals = path ? "IntLiterals"
  val int: GlobalName = intliterals ? "int_lit"

  private val natliterals = path ? "NatLiterals"
  val nat: GlobalName = natliterals ? "nat_lit"
  val pos: GlobalName = natliterals ? "pos_lit"
  val succ: GlobalName = natliterals ? "nat_lit_succ"


  val n = OMS(nat)
  val z = OMS(int)
  val p = OMS(pos)
  def synType(t: Term) = uom.SynOpType(List(Apply.path), Nil, t)
  
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

/** Symbols used for all the different Systems */
object MitMSystems {
  private val _basepath = DPath(URI("http","opendreamkit.org"))
  private val vretheory = _basepath ? "Systems"

  val evalSymbol: GlobalName = vretheory ? "Eval"

  val gapsym: GlobalName = vretheory ? "GAPEval"
  val sagesym: GlobalName = vretheory ? "SageEval"
  val singularsym: GlobalName = vretheory ? "SingularEval"
  val lmfdbsym: GlobalName = vretheory ? "LMFDBEval"
  val querysym: GlobalName = vretheory ? "ODKQuery"
  
  val evaluateSym = _basepath ? "scscp_transient_1" ? "MitM_Evaluate"
}

