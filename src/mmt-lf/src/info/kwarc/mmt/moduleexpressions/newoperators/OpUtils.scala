package info.kwarc.mmt.moduleexpressions.newoperators

import info.kwarc.mmt.api.objects.{Context, OMV, Term, VarDecl}
import info.kwarc.mmt.lf.{ApplySpine, Lambda}

private[newoperators] object OpUtils {
  def bindFresh(t: Term, argTypes: List[Term]): Context = {
    val vars: List[OMV] = Range(1, argTypes.size + 1).map(idx => OMV(s"x_${idx}")).toList

    Context(vars.zip(argTypes).map {
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
