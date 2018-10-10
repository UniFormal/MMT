package info.kwarc.mmt.MiTM

import info.kwarc.mmt.api.{DPath, GlobalName, MPath, uom}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.refactoring.{Preprocessor, SimpleParameterPreprocessor}
import info.kwarc.mmt.api.uom.{RepresentedRealizedType, StandardInt, StandardNat, StandardPositive}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lf.{ApplySpine, LFClassicHOLPreprocessor}
import info.kwarc.mmt.odk.LFX
import info.kwarc.mmt.odk.LFX.LFRecSymbol

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
  val polypath: MPath = (basepath / "smglom" / "elliptic_curves") ? "Base"
  val polynomials: GlobalName = polypath ? "polynomial"
  val polycons: GlobalName = polypath ? "poly_con"


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
  val N= StandardNat
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

object Lists {
  val baseURI = LFX.ns / "Datatypes"
  val th = baseURI ? "ListSymbols"
}

object ListNil {
  val path = Lists.th ? "nil"
  val term = OMS(path)
}

object Append {
  val path2 = Lists.th ? "ls"
  val term2 = OMS(path2)
  val path = Lists.th ? "append"
  val term = OMS(path)
  def apply(a: Term, ls : Term) : Term = OMA(this.term,List(a,ls))
  def unapply(tm : Term) : Option[(Term,Term)] = tm match {
    case OMA(this.term,List(a,ls)) => Some((a,ls))
    case OMA(this.term2,args) if args.nonEmpty =>
      if (args.length==1) Some((args.head,ListNil.term))
      else Some((args.head,OMA(this.term2,args.tail)))
    case _ => None
  }
}

object LFList {
  val path = Lists.th ? "ls"
  val term = OMS(path)
  def apply(tms : List[Term]) : Term = OMA(this.term,tms)
  def unapply(ls : Term) : Option[List[Term]] = ls match {
    case OMA(this.term,args) => Some(args)
    case Append(a,lsi) => unapply(lsi).map(a :: _)
    case _ => None
  }
}

/** Symbols used for all the different Systems */
object MiTMSystems {
  private val _basepath = DPath(URI("http","opendreamkit.org"))
  private val vretheory = _basepath ? "Systems"

  val evalSymbol: GlobalName = vretheory ? "Eval"

  val gapsym: GlobalName = vretheory ? "GAPEval"
  val sagesym: GlobalName = vretheory ? "SageEval"
  val singularsym: GlobalName = vretheory ? "SingularEval"
  val lmfdbsym: GlobalName = vretheory ? "LMFDBEval"
  val querysym: GlobalName = vretheory ? "ODKQuery"
}
