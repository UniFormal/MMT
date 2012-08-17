package info.kwarc.mmt.lf
import info.kwarc.mmt.api._

object LFHOAS extends pragmatics.HOAS {
   val theory = LF.lftheory
   val apply = Apply.path
   val lambda = Lambda.path
}