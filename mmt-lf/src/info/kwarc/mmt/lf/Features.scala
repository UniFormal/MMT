package info.kwarc.mmt.lf
import info.kwarc.mmt.api._

object LFHOAS extends pragmatics.HOAS {
   val theory = LF._path
   val apply = Apply.path
   val lambda = Lambda.path
}

object LFTyping extends pragmatics.Typing {
   val theory = LF._path
   val hastype = OfType.path
}


