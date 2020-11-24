package info.kwarc.mmt.moduleexpressions.newoperators

import info.kwarc.mmt.api.{GlobalName, Path}
import info.kwarc.mmt.api.objects.{Context, OMA, OMS, Term, Traverser}
import info.kwarc.mmt.lf.{BinaryLFConstantScala, FouraryLFConstantScala, FunType, Lambda, NullaryLFConstantScala, Strings, TernaryLFConstantScala, UnaryLFConstantScala}

// to be replaced by auto-generated classes/objects (via lf-scala build target)
private[newoperators] object SFOL {
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

  object tm extends UnaryLFConstantScala(Path.parseM("latin:/?TypedTerms"), "tm")
  object forall extends BinaryLFConstantScala(Path.parseM("latin:/?TypedUniversalQuantification"), "forall")
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
  object or extends BinaryLFConstantScala(Path.parseM("latin:/?Disjunction"), "or")
  object and extends BinaryLFConstantScala(Path.parseM("latin:/?Conjunction"), "and")

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
      SFOL.and.path, SFOL.or.path, SFOL.eq.path, SFOL.exists.path
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