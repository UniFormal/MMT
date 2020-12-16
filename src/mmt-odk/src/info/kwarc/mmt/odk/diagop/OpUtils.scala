package info.kwarc.mmt.odk.diagop

import info.kwarc.mmt.api.objects.{Context, OMV, Term, VarDecl}
import info.kwarc.mmt.lf.{ApplySpine, Lambda}

private[diagop] object StringUtils {
  private val digitSubscripts   = List("₀", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉")
  private val digitSuperscripts = List("⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹")

  /**
    * Stringifies an integer with the provided digits.
    * @param digits A 10-element list of strings representing digits 0 - 9.
    */
  private def translateInteger(digits: List[String])(i: Int): String = {
    if (i < 0) {
      "₋" + translateInteger(digits)(-1 * i)
    } else {
      val prefix = if (i >= 10) translateInteger(digits)(i / 10) else ""
      prefix + digits(i % 10)
    }
  }

  def subscriptInteger(i: Int): String = translateInteger(digitSubscripts)(i)
  def superscriptInteger(i: Int): String = translateInteger(digitSuperscripts)(i)
}

private[diagop] object OpUtils {
  /**
    * Binds variables of given types freshly in `ctx`.
    *
    * Tries to use sensible names like x_0, x_1, ...
    *
    * @param ctx A context in which the newly bound variables should be fresh.
    * @param types The types of the to-be-bound variables, possibly empty list.
    * @return A context of the to-be-bound variables with as many entries as there were in types (thus possible empty).
    *         It can, for instance, be passed to [[GeneralLambda]].
    */
  def bindFresh(ctx: Context, types: List[Term], hint: Option[Int => String] = None): Context = {
    val chooseName = hint.getOrElse((idx: Int) => "x" + StringUtils.subscriptInteger(idx))
    val vars: List[OMV] = types.indices.map(idx => OMV(chooseName(idx))).toList

    Context(vars.zip(types).map {
      case (boundVar, arg) => VarDecl(boundVar.name, arg)
    } : _*)
  }

  /**
    * Like [[ApplySpine]] but doesn't generate an [[OMA]] upon application with 0 arguments.
    * Instead it just returns 'f' in that case.
    */
  def GeneralApplySpine(f: Term, a: Term*): Term = if (a.isEmpty) f else ApplySpine(f, a : _*)

  /**
    * Like [[Lambda.apply()]] but doesn't generate an empty [[OMBINDC]] upon application with
    * an empty context. Instead it just returns 'body' in that case.
    */
  def GeneralLambda(ctx: Context, body: Term): Term = if (ctx.isEmpty) body else Lambda(ctx, body)
}
