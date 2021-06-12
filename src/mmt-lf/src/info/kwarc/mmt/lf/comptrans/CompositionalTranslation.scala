package info.kwarc.mmt.lf.comptrans

import info.kwarc.mmt.api.libraries.Library
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils.UnicodeStrings
import info.kwarc.mmt.api.{GlobalName, ImplementationError, LocalName}
import info.kwarc.mmt.lf._

/**
  * TODO(NR or FR): better name for map?
  * @param baseTranslations
  * @param map
  * @param variableSuffix
  */
class CompositionalTranslation(
                                val baseTranslations: List[CompositionalTranslation],
                                map: GlobalName => Option[Term],
                                variableSuffix: LocalName = LocalName("ᕁ")
                              ) {

  override def toString: String = "[[" + baseTranslations.mkString(",") + "]] = <map not printed in toString>"

  private def listOfOptToOptOfList[T](list: List[Option[T]]): Option[List[T]] = {
    Some(list.map {
      case Some(x) => x
      case _ => return None
    })
  }

  /**
    * For a term `t: A`, computes the expected type of `R(t)`.
    */
  def getExpected(ctx: Context, t: Term, A: Term): Option[Term] = {
    val target = listOfOptToOptOfList(baseTranslations.map(_(ctx, None, t)))
    apply(ctx, target, A)
  }

  /**
    * For a typing judgement `t: A` computes the typing judgement `t': A'` under the compositional translation
    * if defined.
    *
    * This method can be used to compute parts of the "Basic Lemma" (a theorem).
    * The Basic Lemma says if `Γ ⊦ t: A` then `Γ' ⊦' t': A'` where the primed symbols all signify the counterparts
    * of the non-primed symbol under the compositional translation.
    *
    * @see [[getExpected()]]
    * @return The typing judgement `t': A'` (i.e., under the compositional translation) is represented as
    *         `Some((t', A'))` if defined.
    */
  def applyPair(ctx: Context, t: Term, A: Term): Option[(Term, Term)] =
    apply(ctx, None, t) zip getExpected(ctx, t, A)

  def star(name: LocalName): LocalName = name / variableSuffix

  /**
    * document this
    * @param ctx
    * @param target Note that for morphisms (i.e., baseTranslations == Nil) there is no difference between
    * target == None and target == Some(Nil).
    * @param t
    * @return
    */
  def apply(ctx: Context, target: Option[List[Term]], t: Term): Option[Term] = {
    require(target.forall(_.size == baseTranslations.size))

    t match {
      case OMV(x) =>
        val starXGenerated = applyVarDecl(ctx.before(x), ctx.get(x)).exists(_.name == star(x))
        if (starXGenerated) {
          Some(OMV(star(x)))
        } else {
          None
        }

      case Univ(1) =>
        if (target.isDefined) {
          Some(Arrow(target.get, Univ(1)))
        } else {
          Some(Univ(1))
        }

      // unified case for Pi and lambda, for lambda target can be ignored
      // case OMBIND(OMS(Pi.path), boundCtx, retType) =>
      case OMBIND(binder, boundCtx, body) =>
        val newBaseCtx = baseTranslations.flatMap(_.apply(ctx, boundCtx))
        val newBoundCtx = apply(ctx, boundCtx)

        // For the new target, transform `Some(List(t₁, …, tₙ))` to `Some(List(t₁ @ newBaseCtx, …, tₙ @ newBaseCtx))`
        // where `t @ newBaseCtx` applies the term `t` to all variables in `newBaseCtx`.
        val newTarget = target.map(_.map(ApplySpine.orSymbol(_, newBaseCtx.map(_.toTerm) : _*)))

        val newBody = apply(ctx ++ boundCtx, newTarget, body)
        newBody.map(OMBIND(binder, newBoundCtx, _))

      // reduce function types to case for Pi types above
      case t @ FunType(args, _) if args.nonEmpty =>
        apply(ctx, target, CompositionalTranslation.funToPiType(t))

      case ApplySpine(f, args) =>
        // In principle, we follow the definition on paper for comptrans applied on function applications,
        // however, it gets more involved here due to handling more than one argument at once.
        // To understand the code, it is recommended to apply the definition on paper on nested applications
        // like `(f s) t` and `((f r) s) t` to see the general pattern that is codified here.
        val newTarget = args.headOption.flatMap(firstArg => {
          listOfOptToOptOfList(baseTranslations.map(_(ctx, None, firstArg)))
        })

        val newArgs: List[Term] =args.headOption.flatMap(apply(ctx, None, _)).toList ::: args.tail.flatMap(arg =>
          baseTranslations.flatMap(_(ctx, None, arg)) ::: apply(ctx, None, arg).toList
        ) ::: target.toList.flatten

        apply(ctx, newTarget, f).map(ApplySpine.orSymbol(_, newArgs : _*))

      // this case is last as it definitely needs to come after Univ(1)
      case OMS(p) => map(p).map(ApplySpine.orSymbol(_, target.toList.flatten : _*))

      case _ => ???
    }
  }

  /**
    * Maps a [[Context]] `g` (in context of its context `ctx`).
    *
    * @return The context effectively emerging from `g` by applying [[applyVarDecl]] iteratively
    *         to every [[VarDecl]].
    */
  def apply(ctx: Context, g: Context): Context = {
    g.mapVarDecls((partialCtx, vd) => applyVarDecl(ctx ++ partialCtx, vd)).flatten
  }

  def applyVarDecl(ctx: Context, vd: VarDecl): List[VarDecl] = {
    val tp = vd.tp.getOrElse(throw new UnsupportedOperationException("compositional translation on untyped variable"))

    val baseCtx = baseTranslations.flatMap(_.applyVarDecl(ctx, vd))
    val newTp = if (baseTranslations.forall(_.apply(ctx, None, tp).isDefined)) {
      apply(ctx, Some(baseCtx.map(_.toTerm)), tp)
    } else {
      apply(ctx, None, tp)
    }
    val newVd = newTp.map(VarDecl(star(vd.name), _))

    baseCtx ::: newVd.toList
  }
}

class CompositionalMorphism(mor: Term)(implicit val lookup: Library) extends CompositionalTranslation(Nil, p => {
  Some(lookup.ApplyMorphs(OMS(p), mor)) // only for total morphisms, otherwise we get exceptions
}) {
  override def toString: String = s"mor $mor"
}

object CompositionalTranslation {

  /**
    * Transforms a [[Term]] matching a [[FunType]] to a term matching a [[Pi]].
    *
    * TODO: possibly leads to name clashes, investigate this
    *
    * Used to reduce the case of simple function types to the more general case of
    * dependent function types below.
    */
  private def funToPiType(t: Term): Term = t match {
    case FunType(args, retType) =>
      val namedArgsCtx = args.zipWithIndex.map {
        case ((Some(name), tp), _) => VarDecl(name, tp)
        case ((None, tp), index) => VarDecl(LocalName("x" + UnicodeStrings.superscriptInteger(index)), tp)
      }

      Pi(namedArgsCtx, retType)

    case _ => throw ImplementationError("called funToPiType on term not matching a FunType")
  }
}
