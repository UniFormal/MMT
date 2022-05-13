package info.kwarc.mmt.stex

import info.kwarc.mmt.api.objects.{Context, OMA, OMBINDC, OMS, OMV, Obj, Term}

sealed abstract class SOMBArg {
  def obj:Obj
}
case class STerm(tm : Term) extends SOMBArg {
  val obj = tm
}
case class SCtx(ctx : Context) extends SOMBArg {
  val obj = ctx
}

object SOMBArg {
  implicit def toSOMB(ctx: Context) = SCtx(ctx)
  implicit def toSOMB(tm : Term) = STerm(tm)
}

import SOMBArg._

object SOMB {
  def apply(f : Term,scopes:SOMBArg*) : OMBINDC = {
    val ctx = Context(scopes.flatMap{
      case SCtx(ctx) => ctx.variables
      case _ => Nil
    }:_*)
    OMBINDC(f,ctx,scopes.toList.map {
      case STerm(tm) => tm
      case SCtx(ctx) => OMA(OMS(STeX.meta_quantification),ctx.map(vd => OMV(vd.name)))
    })
  }
  def unapply(tm : Term) = tm match {
    case OMBINDC(f,ctx,scopes) =>
      var nctx = Context.empty
      val ret = (f,scopes.map {
        case OMA(OMS(STeX.meta_quantification),args : List[OMV]) =>
          SCtx(args.map(v => {val vd = ctx(v.name); nctx = nctx ++ vd; vd} ))
        case t => STerm(t)
      })
      if (ctx.forall(nctx.contains)) Some(ret) else None
    case _ => None
  }
}