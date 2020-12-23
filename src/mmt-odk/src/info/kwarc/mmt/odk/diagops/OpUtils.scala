package info.kwarc.mmt.odk.diagops

import info.kwarc.mmt.api.objects.{Context, OMV, Term, VarDecl}
import info.kwarc.mmt.api.utils.UnicodeStrings
import info.kwarc.mmt.lf.{ApplySpine, Lambda}



private[diagops] object OpUtils {
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
    val chooseName = hint.getOrElse((idx: Int) => "x" + UnicodeStrings.subscriptInteger(idx))
    val vars: List[OMV] = types.indices.map(idx => OMV(chooseName(idx))).toList

    Context(vars.zip(types).map {
      case (boundVar, arg) => VarDecl(boundVar.name, arg)
    } : _*)
  }

  /**
    * @deprecated Use [[ApplySpine.applyOrSymbol()]].
    */
  def GeneralApplySpine(f: Term, a: Term*): Term = if (a.isEmpty) f else ApplySpine(f, a : _*)

  /**
    * Like [[Lambda.apply()]] but doesn't generate an empty [[OMBINDC]] upon application with
    * an empty context. Instead it just returns 'body' in that case.
    */
  def GeneralLambda(ctx: Context, body: Term): Term = if (ctx.isEmpty) body else Lambda(ctx, body)
}
