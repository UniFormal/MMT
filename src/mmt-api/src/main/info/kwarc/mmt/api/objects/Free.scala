package info.kwarc.mmt.api.objects

import info.kwarc.mmt.api._

object Free {
  val free = utils.mmt.mmtcd ? "free"
  def apply(context: Context, tm: Term): Term = OMBIND(OMS(free), context, tm)
  def apply(context: List[LocalName], tm: Term): Term = apply(context.map(VarDecl(_)), tm)
  def unapply(t: Term): Option[(Context,Term)] = t match {
    case OMBIND(OMS(this.free), con, tm) =>
      Some((con, tm))
    case _ =>
      None
  }
}

/** always matches, possibly with empty context */
object FreeOrAny {
  def apply(context: List[LocalName], tm: Term): Term = apply(context.map(VarDecl(_)), tm)
  def apply(context: Context, t: Term) = if (context.isEmpty) t else Free(context, t)
  def unapply(t: Term): Option[(Context,Term)] = Free.unapply(t) orElse Some((Context.empty,t))
}
