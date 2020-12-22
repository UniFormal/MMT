package info.kwarc.mmt

import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils.UnicodeStrings
import info.kwarc.mmt.api.{GlobalName, LocalName}
import info.kwarc.mmt.lf._

// coding style:
// n: number of morphisms
// i: a concrete index of a single morphism (in 0 .. n - 1)

/**
  *
  *     * Explanation for the need:
  *
  * Consider logical relations over the theory `T = {prop: type, ⊦: prop ⟶ type}`
  * Then
  * {{{
  * apply(Context.empty, Πx: prop. Πy: ⊦ x)
  *   = Πx₁: m₁(prop) … Πxₙ: mₙ(prop) Πx: lr(prop) x₁ … xₙ. Πy₁: m₁(⊦ x₁) … Πyₙ: mₙ(⊦ xₙ). Πy: lr(???
  * }}}
  */
class LogicalRelation(mors: List[Term], lr: GlobalName => Term, lookup: Lookup) {

  private val n = mors.size

  /**
    * Maps `x` to `xᵢ`.
    */
  private def suffix(name: LocalName, i: Int): LocalName = name.suffixLastSimple(UnicodeStrings.subscriptInteger(i))

  /**
    * Maps `x` to `List(x₁, …, xₙ)`.
    */
  private def suffixAll(name: LocalName): List[LocalName] = mors.indices.map(suffix(name, _)).toList

  /**
    * Maps `t` to `mᵢ'(t)` where `mᵢ'` is `mᵢ` with subsequent substitution replacing
    * variables `x` in ctx by `xᵢ`.
    */
  private def applyMor(ctx: Context, t: Term, i: Int): Term = {
    val sub = ctx.map(vd => Sub(vd.name, OMV(suffix(vd.name, i))))
    lookup.ApplyMorphs(t, mors(i)) ^ sub
  }

  /**
    * Maps `t` to `List(m₁(t), … ,mₙ(t))`.
    */
  private def applyMors(ctx: Context, t: Term): List[Term] = mors.indices.map(applyMor(ctx, t, _)).toList

  /**
    * Maps [[VarDecl]] `v: tp [= df]` to `vᵢ: mᵢ'(tp) [= mᵢ'(df)]`.
    */
  private def applyMor(ctx: Context, vd: VarDecl, i: Int): VarDecl = {
    // should refactor VarDecl apply to avoid orNull antipattern?
    VarDecl(suffix(vd.name, i), vd.tp.map(applyMor(ctx, _, i)).orNull, vd.df.map(applyMor(ctx, _, i)).orNull)
  }

  /**
    * Maps [[VarDecl]] `v: tp [= df]` to `List(v₁: m₁'(tp) [= m₁'(df)], …, vₙ: mₙ'(tp) [= mₙ'(df)])`.
    *
    * The resulting [[VarDecl]] should be treated in [[applyMo]]
    */
  private def applyMors(ctx: Context, vd: VarDecl) = mors.indices.map(applyMor(ctx, vd, _)).toList

  def apply(ctx: Context, t: Term): Term = t match {
    case OMV(x) => OMV(x)

    // @DM: try to match Univ(i) in this style
    case Univ(0) =>
      /* basic lemma: e:A in input theory implies lr(e):^r(A) m_1(e) ... m_n(e)
           given c: type in input theory, we obtain expected type of lr(c):E where E = lr(type) m_1(c) ... m_n(c)
           lr(type) = [a_1:type,...,a_n:type] a_1 --> ... --> a_n --> type
           so E = m_1(c) --> ... --> m_n(c) --> type
        */
      val vars = suffixAll(LocalName("x"))

      Lambda(
        vars.map(VarDecl(_, Univ(0))),
        Arrow(vars.map(OMV(_)), Univ(0))
      )

    case ApplySpine(f, args) =>
      ApplySpine(
        apply(ctx, f),
        args.flatMap(applyTerm(ctx, _)) : _*
      )
    case OMBIND(Lambda.path, boundCtx, t) => Lambda(apply(ctx, boundCtx), apply(ctx ++ boundCtx, t))
    case OMBIND(Pi.path, boundCtx, t)     => Lambda(apply(ctx, boundCtx), apply(ctx ++ boundCtx, t))
    // would work for Sigma too

    case OMS(p) => lr(p) // this case needs to come last lest to confuse with OMS to Univ(0)
  }

  /**
    * Maps a [[Context]].
    *
    * d_1, ..., d_r ---> lr(d_1) ... lr(d_n)
    * todo: improve docs here
    */
  def apply(ctx: Context, g: Context): Context = {
    g.mapVarDecls((partialCtx, vd) => applyVarDecl(ctx ++ partialCtx, vd)).flatten
  }  // g flatMapInContext { case (h, vd) => applyMors(c ++ h, vd) }

  /**
    * Maps `t` to `List(m₁(t), …, mₙ(t), lr(t))`.
    */
  private def applyTerm(c: Context, t: Term): List[Term] = applyMors(c, t) :+ apply(c, t)

  /**
    * Maps [[VarDecl]] `v: tp [=df]` to `List(v₁: m₁'(tp) [= m₁'(df)], …, vₙ: mₙ'(tp) [= mₙ'(df)], v: lr(tp) x₁ … xₙ)`.
    */
  private def applyVarDecl(ctx: Context, vd: VarDecl): List[VarDecl] = {
    val vars = suffixAll(vd.name).map(OMV(_))
    val tp = vd.tp.map(t => ApplySpine(apply(ctx, t), vars : _*))

    applyMors(ctx, vd) :+ VarDecl(vd.name, tp.orNull)
  }

  /**
    * For a term `t: tp`, computes the expected type of `lr(t)`.
    *
    * Namely, the expected type is `lr(tp) m₁(t) … mₙ(t)`.
    *
    * This only works for LF, right?
    */
  def getExpected(ctx: Context, t: Term, A: Term): Term = ApplySpine(apply(ctx, A), applyMors(ctx, t) : _*)

  /**
    * For a term `t: tp`, computes the expected judgement `lr(t) : getExpected(t)`…
    *
    * Namely, `lr(t) : lr(tp) m₁(t) … mₙ(t)`.
    *
    * This only works for LF, right?
    *
    * @see [[getExpected()]]
    */
  def applyPair(c: Context, t: Term, A: Term): (Term, Term) = (apply(c, t), getExpected(c, t, A))
}