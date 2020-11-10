package info.kwarc.mmt.moduleexpressions.newoperators

import info.kwarc.mmt.api.{GeneralError, GlobalName, LocalName, MPath, Path}
import info.kwarc.mmt.api.checking.CheckingCallback
import info.kwarc.mmt.api.modules.{DefaultStateOperator, Module, SimpleLinearOperator, SystematicRenamingUtils}
import info.kwarc.mmt.api.objects.Context.context2list
import info.kwarc.mmt.api.objects.{Context, OMS, OMV, Term, VarDecl}
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.lf.{ApplySpine, BinaryLFConstantScala, FunType, Lambda, NullaryLFConstantScala, TernaryLFConstantScala, UnaryLFConstantScala}

object HomOperator extends SimpleLinearOperator with DefaultStateOperator with SystematicRenamingUtils {
  override val head: GlobalName = Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?hom_operator")

  override protected val operatorDomain: MPath = Path.parseM("latin:/?SFOLEQND")
  override protected val operatorCodomain: MPath = Path.parseM("latin:/?SFOLEQND")

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_hom")

  override protected def applyConstantSimple(module: Module, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit solver: CheckingCallback, state: HomOperator.LinearState): List[List[(LocalName, Term, Option[Term])]] = {

    val dom = getRenamerFor("d", module.toTerm)
    val cod = getRenamerFor("c", module.toTerm)
    val hom = getRenamerFor("h", module.toTerm)

    val domCodCopies = List(
      (dom(name), dom(tp), df.map(dom(_))),
      (cod(name), cod(tp), df.map(cod(_)))
    )

    def quantify(argTypes: List[GlobalName]): (Context, Term, Term) = {
      val vars: List[OMV] = Range(1, argTypes.size + 1).map(idx => OMV(s"x_${idx}")).toList

      val domExpr = GeneralApplySpine( // use GeneralApplySpine: the function symbol might be nullary
        OMS(dom(c.path)),
        vars : _*
      )

      val codExpr = GeneralApplySpine( // use GeneralApplySpine: the function symbol might be nullary
        OMS(cod(c.path)),
        argTypes.zip(vars).map {
          case (arg, boundVar) => ApplySpine(OMS(hom(arg)), boundVar)
        } : _*
      )

      val forallContext = Context(vars.zip(argTypes).map {
        case (boundVar, arg) => VarDecl(boundVar.name, OMS(dom(arg)))
      } : _*)

      (forallContext, domExpr, codExpr)
    }

    tp match {
      case SFOL.TypeSymbolType() =>
        // input:  t: tp
        // output: t^d: tp, t^c: tp, t^h: tm t^d -> t^c
        val thType = FunType(
          List((None, SFOL.tm(OMS(applyModulePath(module.path) ? dom(name))))),
          SFOL.tm(OMS(applyModulePath(module.path) ? cod(name)))
        )

        List(List(
          (dom(name), dom(tp), df.map(dom(_))),
          (cod(name), cod(tp), df.map(cod(_))),
          (hom(name), thType, None)
        ))

      case SFOL.FunctionSymbolType(argTypes, retType) =>
        val (forallContext, domExpr, codExpr) = quantify(argTypes)

        val homomorphismCondition = SFOL.ded(SFOL.forallMany(
          forallContext,
          SFOL.eq(
            SFOL.tm(OMS(cod(retType))),
            ApplySpine(OMS(hom(retType)), domExpr),
            codExpr
          )
        ))

        List(
          domCodCopies :+ (hom(name), homomorphismCondition, None)
        )

      case SFOL.PredicateSymbolType(argTypes) =>
        val (forallContext, domExpr, codExpr) = quantify(argTypes)

        val homomorphismCondition = SFOL.ded(SFOL.forallMany(
          forallContext,
          SFOL.impl(domExpr, codExpr)
        ))

        List(
          domCodCopies :+ (hom(name), homomorphismCondition, None)
        )

      case SFOL.AxiomSymbolType() => List(domCodCopies)

      case _ =>
        throw GeneralError(s"Hom operator cannot process SFOL constant ${c.path} of unknown form (neither type, function, " +
          "predicate, nor axiom symbol.")
    }
  }

  /**
    * Like [[ApplySpine]] but doesn't generate an [[OMA]] upon application with 0 arguments.
    * Instead it just returns 'f' in that case.
    */
  private def GeneralApplySpine(f: Term, a: Term*): Term = if (a.isEmpty) f else ApplySpine(f, a : _*)

  private object SFOL {
    object tp extends NullaryLFConstantScala(Path.parseM("latin:/?Types"), "tp")
    object prop extends TernaryLFConstantScala(Path.parseM("latin:/?Propositions"), "prop")

    object tm extends UnaryLFConstantScala(Path.parseM("latin:/?TypedTerms"), "tm")
    object forall extends BinaryLFConstantScala(Path.parseM("latin:/?TypedUniversalQuantification"), "forall")
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
  }
}
