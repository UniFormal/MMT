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

  val isAtomic: Boolean = baseTranslations.isEmpty
  override def toString: String =
    (if (isAtomic) "mor" else "[[" + baseTranslations.mkString(",") + "]]") + " = <map not printed in toString>"

  /**
    * For a term `t: A`, computes the expected type of `R(t)`.
    */
  def getExpected(ctx: Context, t: Term, A: Term): Option[Term] = {
    apply(ctx, baseTranslations.flatMap(_(ctx, Nil, t)), A)
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
    apply(ctx, Nil, t) zip getExpected(ctx, t, A)

  def star(name: LocalName): LocalName = name / variableSuffix

  def apply(ctx: Context, target: List[Term], t: Term): Option[Term] = {
    t match {
      case OMV(x) =>
        val starXGenerated = applyVarDecl(ctx.before(x), ctx.get(x)).exists(_.name == star(x))
        if (starXGenerated) Some(OMV(star(x))) else None

      case Univ(1) => Some(Arrow(target, Univ(1)))
      // needs to come after case for Univ(1) (to prevent matching on LF's `type` symbol)
      case OMS(p) => map(p).map(ApplyGeneral(_, target))

      case OMBIND(binder, boundCtx, body) =>
        val newBaseCtx = baseTranslations.flatMap(_.apply(ctx, boundCtx))
        val newBoundCtx = apply(ctx, boundCtx)

        // For the new target, transform `List(o₁, …, oₙ)` to `List(o₁ @ newBaseCtx, …, oₙ @ newBaseCtx))`
        // where `Some(t) @ newBaseCtx` applies the term `t` to all variables in `newBaseCtx` and
        // `None @ newBaseCtx` is `None`.
        val newTarget = target.map(ApplyGeneral(_, newBaseCtx.map(_.toTerm)))

        val newBody = apply(ctx ++ boundCtx, newTarget, body)
        newBody.map(OMBIND(binder, newBoundCtx, _))

      // Special case arrow types for atomic comptrans ("morphisms") to prevent making up unnecessary names as
      // the case underneath does. This is important for some applications, e.g., the cleanup operator for
      // softening as described in the softening paper 2021 by Florian Rabe, Navid Roux.
      case Arrow(arg, ret) if baseTranslations.isEmpty =>
        apply(ctx, Nil, ret).map(Arrow(apply(ctx, Nil, arg).toList, _))

      case t@FunType(args, _) if args.nonEmpty =>
        apply(ctx, target, CompositionalTranslation.funToPiType(t)) // make up names for unnamed arguments

      case ApplySpine(f, args) =>
        val newTarget = args.flatMap(arg => {
          baseTranslations.flatMap(_(ctx, Nil, arg)) ::: apply(ctx, Nil, arg).toList
        }) ::: target

        apply(ctx, newTarget, f)

      case _ => None
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
    val newTp = apply(ctx, baseCtx.map(_.toTerm), tp)
    val newVd = newTp.map(VarDecl(star(vd.name), _))

    baseCtx ::: newVd.toList
  }
}

/**
  * The special case of a total atomic comptrans whose mapping is specified by an MMT morphism term.
  * @param mor A morphism term, e.g. an [[OMMOD]] to a view or a complex term.
  * @param lookup A lookup used to apply the morphism to terms.
  */
class CompositionalTotalMorphism(mor: Term)(implicit val lookup: Library) extends CompositionalTranslation(Nil, p => {
  Some(lookup.ApplyMorphs(OMS(p), mor))
}) {
  override def toString: String = s"mor ${mor.toStr(true)}"

  // with the new definition, this is superfluous and the require() even wrong
  /*override def apply(ctx: Context, target: List[Term], t: Term): Option[Term] = {
    require(target.isEmpty)
    super.apply(ctx, target, t)
  }*/
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
