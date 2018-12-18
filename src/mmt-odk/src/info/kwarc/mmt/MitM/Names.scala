package info.kwarc.mmt.MitM

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.refactoring.{Preprocessor, SimpleParameterPreprocessor,AcrossLibraryTranslation, AcrossLibraryTranslator, TranslationTarget}
import info.kwarc.mmt.api.uom.{RepresentedRealizedType, StandardInt, StandardNat, StandardPositive}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lf._
import info.kwarc.mmt.odk._
import info.kwarc.mmt.odk.LFX.{LFList, LFRecSymbol}

import scala.math.BigInt

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

  val rationalRing = (basepath / "smglom" / "algebra") ? "RationalField" ? "rationalField2"
  val numberfield = (MitM.basepath / "smglom" / "algebra") ? "NumberSpaces" ? "numberField"

  object Monomial {
    object IInt {
      def unapply(tm : Term): Option[BigInt] = tm match {
        case IntegerLiterals(i : BigInt) => Some(i)
        case info.kwarc.mmt.api.objects.UnknownOMLIT(s, _) => unapply(IntegerLiterals.parse(s))
        case _ => None
      }
    }
    object SString {
      def unapply(tm : Term): Option[String] = tm match {
        case StringLiterals(s) => Some(s)
        case info.kwarc.mmt.api.objects.UnknownOMLIT(s, _) => unapply(StringLiterals.parse(s))
        case _ => None
      }
    }
    def apply(vars : List[(String,BigInt)],coeff : BigInt, ring : Term = OMS(MitM.rationalRing)) =
      ApplySpine(OMS(MitM.monomial_con),ring :: LFX.Tuple(LFList(vars.map(p => LFX.Tuple(StringLiterals(p._1),IntegerLiterals(p._2)))),
        IntegerLiterals(coeff)):: Nil :_*)
    def unapply(tm : Term) = tm match {
      case OMA(OMS(MitM.monomial_con),List(ring,LFX.Tuple(LFList(ls),IInt(coeff)))) =>
        val ils = ls.map {
          case LFX.Tuple(SString(x),IInt(i)) => (x,i)
          case _ => ???
        }
        Some((ils,coeff,ring))
      case ApplySpine(OMS(MitM.monomial_con),List(ring,LFX.Tuple(LFList(ls),IInt(coeff)))) =>
        val ils = ls.map {
          case LFX.Tuple(SString(x),IInt(i)) => (x,i)
          case _ => ???
        }
        Some((ils,coeff,ring))
      case _ => None
    }
  }

  /** convenience class for representing monomials */
  case class MultiMonomialStructure(coefficient: BigInt, exponents: List[BigInt])
  /** convenience class for representing polynomials */
  case class MultiPolynomialStructure(baseRing: Term, varnames: List[String], monomials: List[MultiMonomialStructure]) {
    val varTerms = varnames map StringLiterals.apply
    def toTerm = {
      val monomTerms = monomials.map {m =>
         val powers = varnames zip m.exponents
         Monomial(powers, m.coefficient, baseRing)  
      }
      multi_polycon(baseRing :: monomTerms :_*)
    }
  }
  object MultiPolynomialStructure {
    def fromTerm(tm: Term): Option[MultiPolynomialStructure] = tm match {
      case MitM.MultiPolynomial(r,ls) =>
        var length = -1
        val names : List[String] = ls.flatMap { case (vars,_,_) =>
          vars.map(_._1)
        }.distinct.sorted
        val monoms = ls map { case (vars,coeff,_) =>
          // names :::= vars.map(_._1)
          val args = names.map(s => vars.find(_._1 == s).map(_._2).getOrElse(BigInt(0)))//vars.sortBy(_._1).map(_._2)
          if (length == -1) length = args.length
          assert(length == args.length)
          MultiMonomialStructure(coeff, args)
        }
        val mps = MultiPolynomialStructure(r,names,monoms)
        Some(mps)
      case _ => None
    }
  }
  /** aligns a specific system's polynomials with MitM polynomials via [[MultiPolynomialStructure]] */
  abstract class MultiPolynomialMatcher {self =>
    def unapply(tm: Term): Option[MitM.MultiPolynomialStructure]
    def apply(mps: MultiPolynomialStructure): Term
    
    def getAlignmentTo = new AcrossLibraryTranslation {
      def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = MultiPolynomialStructure.fromTerm(tm).isDefined
      def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = {
        val mps = MultiPolynomialStructure.fromTerm(tm).get
        self.apply(mps)
      }
    }
    def getAlignmentFrom = new AcrossLibraryTranslation {
       def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = self.unapply(tm).isDefined
       def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = self.unapply(tm).get.toTerm
    }
  }  
  
  object MultiPolynomial {
    def unapply(tm : Term) = tm match {
      case ApplySpine(OMS(`multi_polycon`), r :: LFList(ls):: Nil) if ls.nonEmpty && ls.forall(MitM.Monomial.unapply(_).isDefined) =>
        Some((r,ls.map(MitM.Monomial.unapply(_).get)))
      case OMA(OMS(`multi_polycon`), LFList(ls) :: Nil) if ls.nonEmpty && ls.forall(MitM.Monomial.unapply(_).isDefined) =>
        val r = MitM.Monomial.unapply(ls.head).get._3
        Some((r,ls.map(MitM.Monomial.unapply(_).get)))
      case OMA(OMS(`multi_polycon`), r :: LFList(ls):: Nil) if ls.nonEmpty && ls.forall(MitM.Monomial.unapply(_).isDefined) =>
        Some((r,ls.map(MitM.Monomial.unapply(_).get)))
      case _ => None
    }
    def apply(r : Term,ls : List[Term]) = ApplySpine(OMS(multi_polycon),r,LFList(ls))
  }
  def present(tm : Term, pres : Term => String) : String = tm match {
    case LFList(ls) =>
      "[ " + ls.map(present(_,pres)).mkString(", ") + " ]"
    case MultiPolynomial(ring,monoms) =>
      "(in " + pres(ring) + ":) " + monoms.map { case (vars,coeff,_) =>
        coeff.toString + "⋅" + vars.flatMap {
          case (s,i) if i>1 => Some(s + "^" + i)
          case (s,i) if i==1 => Some(s)
          case (s,i) if i==0 => None
        }.mkString("⋅")
      }.mkString(" + ")
    case _ => pres(tm)
  }

  val monomials : GlobalName = polypath ? "monomial"
  val monomial_con : GlobalName = polypath.parent ? "RationalPolynomials" ? "monomial_con"
  val multi_polycon : GlobalName = polypath.parent ? "RationalPolynomials" ? "multi_poly_con"

  val polyOrbit : GlobalName = (basepath / "smglom" / "algebra" / "permutationgroup") ? "GroupAction" ? "polynomialOrbit"
  val dihedralGroup : GlobalName = (basepath / "smglom" / "algebra" / "permutationgroup") ? "PermutationGroup" ? "dihedralGroup"
  val groebner : GlobalName = polypath.parent ? "RationalPolynomials" ? "groebner"


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
  val succ: GlobalName = natliterals ? "succ_nat_lit"


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
  private val _basepath = DPath(URI("http","www.opendreamkit.org"))
  private val vretheory = _basepath ? "Systems"

  /** marks a term for evaluation in a specific system */
  val evalSymbol: GlobalName = vretheory ? "Eval"

  val gapsym: GlobalName = vretheory ? "GAPEval"
  val sagesym: GlobalName = vretheory ? "SageEval"
  val singularsym: GlobalName = vretheory ? "SingularEval"
  val lmfdbsym: GlobalName = vretheory ? "LMFDBEval"
  val querysym: GlobalName = vretheory ? "ODKQuery"
  
  /** kicks off a computation, and serves as default head of SCSCP servers */
  val evaluateSym = _basepath ? "scscp_transient_1" ? "MitM_Evaluate"
}

