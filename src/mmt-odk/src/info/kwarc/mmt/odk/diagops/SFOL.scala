package info.kwarc.mmt.odk.diagops

import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.{GlobalName, MPath, Path}
import info.kwarc.mmt.lf._

// to be replaced by auto-generated classes/objects (via lf-scala build target)
private[diagops] object SFOL {

  val sfoleqnd : MPath = Path.parseM("latin:/?SFOLEQND")

  val Strengthened : MPath = Path.parseM("latin:/algebraic/diagop-test?StrengthenedSFOL")

  object tp extends NullaryLFConstantScala(Path.parseM("latin:/?Types"), "tp")

  object prop extends TernaryLFConstantScala(Path.parseM("latin:/?Propositions"), "prop")

  object predicateSubTp extends BinaryLFConstantScala(Path.parseM("latin:/?PredicateSubtypes"), "predsub")

  object predicateSubTpIn extends FouraryLFConstantScala(Path.parseM("latin:/?PredicateSubtypes"), "in")

  object predicateSubTpIsSubtype extends BinaryLFConstantScala(Path.parseM("latin:/?PredicateSubtypes"), "predsub_sub")

  object subtypeInject extends FouraryLFConstantScala(Path.parseM("latin:/?SubtypeInjections"), "inject")

  def injectSubtypeElementIntoParent(parentTp: Term, selectionFun: Term, subElem: Term): Term = {
    subtypeInject(
      predicateSubTp(parentTp, selectionFun),
      parentTp,
      predicateSubTpIsSubtype(parentTp, selectionFun),
      subElem
    )
  }

  def downcastParentElementToSubtype(parentTp: Term, selectionFun: Term, parentElem: Term, containmentProof: Term): Term =
    predicateSubTpIn(parentTp, selectionFun, parentElem, containmentProof)

  object QuotientTypes {

    object quotientTp extends BinaryLFConstantScala(Path.parseM("latin:/?QuotientTypesBase"), "quot")

    object quot_project extends TernaryLFConstantScala(Path.parseM("latin:/?QuotientTypesBase"), "quot")

    object quot_inj extends TernaryLFConstantScala(Path.parseM("latin:/?QuotientIndefiniteAccess"), "quot_inj")

  }

  object tm extends UnaryLFConstantScala(Path.parseM("latin:/?TypedTerms"), "tm")

  object true_ extends NullaryLFConstantScala(Path.parseM("latin:/?Truth"), "true")
  object forall extends BinaryLFConstantScala(Path.parseM("latin:/?TypedUniversalQuantification"), "forall")
  object forallI extends TernaryLFConstantScala(Path.parseM("latin:/?TypedUniversalQuantificationND"), "forallI")

  object exists extends BinaryLFConstantScala(Path.parseM("latin:/?TypedExistentialQuantification"), "exists")

  object forallMany {
    def apply(ctx: Context, body: Term): Term = {
      var term = body
      for (vd <- ctx.reverse) {
        term = forall(vd.tp.get, Lambda(vd.name, vd.tp.get, term))
      }
      term
    }
  }

  object impl extends BinaryLFConstantScala(Path.parseM("latin:/?Implication"), "impl")

  /**
    * Constructs implication with possibly empty antecedence.
    *
    * If antecedence is empty, consequence is returned.
    */
  def implOption(antecedence: Option[Term], consequence: Term): Term = {
    antecedence.map(impl(_, consequence)).getOrElse(consequence)
  }

  object equiv extends BinaryLFConstantScala(Path.parseM("latin:/?Equivalence"), "equiv")

  // alias for [[equiv]]
  object biimpl extends BinaryLFConstantScala(Path.parseM("latin:/?Equivalence"), "equiv")

  /**
    * Bulids a chain of biimplication-chained propositions.
    *
    * @example ''biimplChain(a, b) = a ⇔ b''
    * @example ''biimplChain(a, b, c) = (a ⇔ b) ∧ (b ⇔ c)''
    * @param propositions Sequence of propositions of size >= 2
    */
  def biimplChain(propositions: Term*): Term = {
    require(propositions.size >= 2)
    propositions.zip(propositions.tail).map {
      case (biimplLeftArgument, biimplRightArgument) =>
        SFOL.biimpl(biimplLeftArgument, biimplRightArgument)
    }.reduceLeft(SFOL.and(_, _))
  }

  object or extends BinaryLFConstantScala(Path.parseM("latin:/?Disjunction"), "or")

  object and extends BinaryLFConstantScala(Path.parseM("latin:/?Conjunction"), "and")

  // invocations of this method to be later replaced by real sketch
  def sketchLazy(str: String): Term = sketch(OMV("<todo:type>"), str)

  object sketch extends BinaryLFConstantScala(Path.parseM("latin:/?SKETCH_DOESNT_EXIST_YET"), "sketch") {
    def apply(tp: Term, str: String): Term = apply(tp, Strings(str))
  }

  object ded extends UnaryLFConstantScala(Path.parseM("latin:/?Proofs"), "ded")

  object eq extends TernaryLFConstantScala(Path.parseM("latin:/?TypedEquality"), "equal")

  object TypeSymbolType {
    def unapply(t: Term): Boolean = t match {
      case OMS(p) if p == tp.path => true
      case _ => false
    }
  }

  object FunctionSymbolType {
    def unapply(t: Term): Option[(List[GlobalName], GlobalName)] = t match {
      case FunctionLikeSymbolType(args, tm(OMS(retPath))) => Some((args, retPath))
      case _ => None
    }
  }

  object PredicateSymbolType {
    def unapply(t: Term): Option[List[GlobalName]] = t match {
      case FunctionLikeSymbolType(args, OMS(p)) if p == prop.path => Some(args)
      case _ => None
    }
  }

  // Some operators do very similar things for function and predicate symbol types
  object FunctionOrPredicateSymbolType {
    def unapply(t: Term): Option[List[GlobalName]] = t match {
      case FunctionSymbolType(args, _) => Some(args)
      case PredicateSymbolType(args) => Some(args)
      case _ => None
    }
  }

  object AxiomSymbolType {
    def unapply(t: Term): Boolean = t match {
      case ded(_) => true
      case _ => false
    }
  }

  private object FunctionLikeSymbolType {
    // this only matches with hard-coded tm
    def unapply(t: Term): Option[(List[GlobalName], Term)] = t match {
      case FunType(args, retType) =>
        val argTypes = args.map {
          case (None, tm(OMS(p))) => p
          case _ => return None
        }
        Some((argTypes, retType))

      case _ => None
    }
  }

  def isMonotone(t: Term, context: Context, allowedReferences: Set[GlobalName]): Boolean = {
    // allowed operations apart from all the symbols from operatorState.processedDeclarations.
    val allowedOps: List[GlobalName] = List(
      Lambda.path,
      Apply.path,
      SFOL.and.path, SFOL.or.path, SFOL.eq.path, SFOL.exists.path,
      SFOL.tm.path
    )
    sealed class MonotonicityStatus(var isMonotone: Boolean)

    val monotonicityTraverser = new Traverser[MonotonicityStatus] {
      override def traverse(t: Term)(implicit con: Context, state: MonotonicityStatus): Term = {
        if (!state.isMonotone) {
          // no need to recurse further
          t
        } else t match {
          case OMS(p) if allowedReferences.contains(p) => t
          case OMS(p) if allowedOps.contains(p) => t
          case OMS(_) =>
            // todo: log the path to the non-monotone op that occurred here
            state.isMonotone = false
            t

          case _ => Traverser(this, t)
        }
      }
    }

    val monotonicity = new MonotonicityStatus(true)
    monotonicityTraverser(t, monotonicity, context)

    monotonicity.isMonotone
  }
}
