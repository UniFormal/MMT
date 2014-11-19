package info.kwarc.mmt.sequences

import info.kwarc.mmt.api.objects._ 

object Length {
   def apply(context: Context, tm: Term): Term = tm match {
     case LFS.ellipsis(m,n,_,_) => LFS.succ(LFS.minus(n,m))
     case OMV(x) =>
        context(x).tp.map(t => apply(context.before(x), t)).getOrElse(LFS.natlit(1))
     case _ => LFS.natlit(1)
   }
}