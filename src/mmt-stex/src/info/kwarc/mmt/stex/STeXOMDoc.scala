package info.kwarc.mmt.stex

import info.kwarc.mmt.api.objects.{Context, OMA, OMBIND, OMBINDC, OMS, OMV, Obj, Term}

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

object SOMB2 {
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

object SOMB {
  def apply(f : Term,scopes:SOMBArg*) : OMBINDC = {
    val ctx = Context(scopes.flatMap{
      case SCtx(ctx) => ctx.variables
      case _ => Nil
    }:_*)
    OMBIND(OMS(STeX.apply),ctx,OMA(f,scopes.toList.map{
      case STerm(tm) => tm
      case SCtx(ctx) => OMA(OMS(STeX.meta_quantification),ctx.map(vd => OMV(vd.name)))
    }))
  }
  def unapply(tm : Term) = tm match {
    case OMBIND(OMS(STeX.apply),ctx,OMA(f,scopes)) =>
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

object SOMA {
  def apply(f : Term,args : Term*) : Term = args.toList match {
    case Nil => f
    case ls => OMA(OMS(STeX.apply),f :: ls)
    //case List(a) => OMA(OMS(STeX.apply),List(f,a))
    //case h :: tail => apply(OMA(OMS(STeX.apply),List(f,h)),tail:_*)
  }
  def unapply(tm : Term) : Option[(Term,List[Term])] = tm match {
    /*case OMA(OMS(STeX.apply),(it@OMA(OMS(STeX.apply),_ :: _)) :: ls) =>
      unapply(it).map { case (h,as) =>
        (h,as ::: ls)
      }*/
    case OMA(OMS(STeX.apply),f :: ls) => Some((f,ls))
    case _ => None
  }
}