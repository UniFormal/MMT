package info.kwarc.mmt

import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils.UnicodeStrings
import info.kwarc.mmt.api.{GlobalName, LocalName}
import info.kwarc.mmt.lf._

/**
  * Logical Relations
  *
  * TODO: comment more
  *
  * {{{basic lemma: e:A in input theory implies lr(e):^r(A) m_1(e) ... m_n(e)
           given c: type in input theory, we obtain expected type of lr(c):E where E = lr(type) m_1(c) ... m_n(c)
           lr(type) = [a_1:type,...,a_n:type] a_1 --> ... --> a_n --> type
           so E = m_1(c) --> ... --> m_n(c) --> type
       }}}
  *
  *
  * Consider logical relations over the theory `T = {prop: type, ⊦: prop ⟶ type}`
  * Then
  * {{{
  * apply(Context.empty, Πx: prop. Πy: ⊦ x)
  *   = Πx₁: m₁(prop) … Πxₙ: mₙ(prop) Πx: lr(prop) x₁ … xₙ. Πy₁: m₁(⊦ x₁) … Πyₙ: mₙ(⊦ xₙ). Πy: lr(???
  * }}}
  *
  * Notes on style for coding and comments:
  *
  * - n stands for mors.size
  * - meta variable i stands for some concrete index of a single morphism (in 0 .. n - 1)
  * - indices in comments are 1-based since argument indices are usually presented as 1-based to humans
  * - the variable names actually produced by this code may differ slightly from those mentioned in
  *   comments.
  *   The overall goal is to make comments human-readable and useful to give an intuition on what the
  *   code does to a human reader.
  */
class LogicalRelation(mors: List[Term], lr: GlobalName => Term, lookup: Lookup) {

  /**
    * For a term `t: A`, computes the expected type of `lr(t)`.
    *
    * Namely, the expected type is `lr(A) m₁(t) … mₙ(t)`.
    *
    * TODO: This only works for LF, right?
    */
  def getExpected(ctx: Context, t: Term, A: Term): Term = {
    val tmpForDebugging = ApplySpine(apply(ctx, A), applyMors(ctx, t) : _*)
    tmpForDebugging
  }

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

  def apply(ctx: Context, t: Term): Term = t match {
    case OMV(x) => OMV(star(x))

    // Cases for inhabitable t
    // =========================================================================
    // The return value in all these cases must have the form:
    //
    //   λa₁: m₁(t). … λaₙ: mₙ(t). <some MMT term here depending on case>
    //
    // This form corresponds to `getExpected`, see its docs.
    // Use `bindTerm(ctx, some name you may choose, t)` to generate the context bound by the
    // lambdas shown above.
    //
    case Univ(1) => // @DM: try to match Univ(i) in this style
      // create context `{x₁: m₁'(t), …, xₙ: mₙ'(t)}`
      val targetBinder = bindTerm(ctx, LocalName("x"), t)

      // return `λx₁: m₁'(t). … λxₙ: mₙ'(t). x₁ ⟶ … ⟶ xₙ ⟶ type
      Lambda(
        targetBinder,
        Arrow(targetBinder.map(_.toTerm), Univ(1))
      )

    case OMBIND(OMS(Pi.path), boundCtx, retType) =>
      // For reading along in comments, suppose `boundCtx = {a: tp_a, …, z: tp_z}`.
      // create context `{f₁: m₁'(t), …, fₙ: mₙ'(t)}`
      val targetBinder = bindTerm(ctx, LocalName("f"), t)

      // create `List(f₁ a₁ … zₙ, …, fₙ aₙ … zₙ )`
      val targetApplications = targetBinder.zipWithIndex.map {
        case (vd, i) =>
          ApplySpine(
            vd.toTerm,
            boundCtx.map(vd => OMV(suffix(vd.name, i))): _*
          )
      }

      val ret = Lambda(
        targetBinder,
        Pi(
          apply(ctx, boundCtx),
          ApplySpine(
            apply(ctx ++ boundCtx, retType),
            targetApplications : _*
          )
        )
      )
      ret

    // case for LFX' Sigma similar to Pi's case?

    case FunType(args, retType) if args.nonEmpty =>
      // reduce to case of Pi by making up names for unnamed arguments

      val namedArgsCtx = args.zipWithIndex.map {
        case ((Some(name), tp), _) => VarDecl(name, tp)
        case ((None, tp), index) => VarDecl(LocalName("x" + UnicodeStrings.superscriptInteger(index)), tp)
      }

      apply(ctx, Pi(namedArgsCtx, retType))

    // end: cases for inhabitable t.
    // =========================================================================

    case ApplySpine(f, args) =>
      ApplySpine(
        apply(ctx, f),
        args.flatMap(applyTerm(ctx, _)) : _*
      )
    case OMBIND(Lambda.path, boundCtx, t) => Lambda(apply(ctx, boundCtx), apply(ctx ++ boundCtx, t))

    case OMS(p) => // this case is last as it definitely needs to come after Univ(1)
      val ret = lr(p)
      ret
  }

  /**
    * Maps a [[Context]].
    *
    * d_1, ..., d_r ---> lr(d_1) ... lr(d_n)
    * todo: improve docs here
    */
  def apply(ctx: Context, g: Context): Context = {
    g.mapVarDecls((partialCtx, vd) => applyVarDecl(ctx ++ partialCtx, vd)).flatten
  }

  /**
    * Maps a variable name (from the logrel's domain) to the name of the variable standing
    * for the proof of relatedness of `suffix(name, 0) … suffix(name, n - 1)`.
    *
    * The case of just a single morphism is special-cased to overall produce more human-readable
    * variable names.
    *
    * Only change in conjunction with [[suffix()]]!
    */
  private def star(name: LocalName): LocalName = {
    if (mors.size == 1) name.suffixLastSimple("ᕁ") else name
  }

  /**
    * Maps a variable name (from the logrel's domain) to the name of the i-th variable translated
    * through mᵢ.
    *
    * The case of just a single morphism is special-cased to overall produce more human-readable
    * variable names.
    *
    * Only change in conjunction with [[star()]]!
    */
  private def suffix(name: LocalName, i: Int): LocalName = {
    // i + 1 to since human-readable argument indices are usually 1-based
    if (mors.size == 1) name else name.suffixLastSimple(UnicodeStrings.subscriptInteger(i + 1))
  }

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
  private def applyMors(ctx: Context, vd: VarDecl): List[VarDecl] =
    mors.indices.map(applyMor(ctx, vd, _)).toList

  /**
    * Maps `t` to `List(m₁(t), …, mₙ(t), lr(t))`.
    */
  private def applyTerm(c: Context, t: Term): List[Term] = applyMors(c, t) :+ apply(c, t)

  /**
    * Creates the [[Context]] `name₁: m₁'(tp), …, nameₙ: mₙ'(tp)`.
    *
    * Should be used dually to [[applyTerm]].
    */
  private def bindTerm(ctx: Context, name: LocalName, tp: Term): Context = {
    applyMors(ctx, tp).zipWithIndex.map {
      case (mTp, i) => VarDecl(suffix(name, i), mTp)
    }
  }

  /**
    * Maps [[VarDecl]] `v: tp [=df]` to `List(v₁: m₁'(tp) [= m₁'(df)], …, vₙ: mₙ'(tp) [= mₙ'(df)], v: lr(tp) x₁ … xₙ)`.
    */
  private def applyVarDecl(ctx: Context, vd: VarDecl): List[VarDecl] = {
    val vars = suffixAll(vd.name).map(OMV(_))
    val tp = vd.tp.map(t => ApplySpine(apply(ctx, t), vars : _*))

    val ret = applyMors(ctx, vd) :+ VarDecl(star(vd.name), tp.orNull)
    ret
  }
}