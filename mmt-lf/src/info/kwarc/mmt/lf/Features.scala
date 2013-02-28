package info.kwarc.mmt.lf
import info.kwarc.mmt.api._

object LFHOAS extends pragmatics.HOAS {
   val theory = LF.lftheory
   val apply = Apply.path
   val lambda = Lambda.path
}

object LFTyping extends pragmatics.Typing {
   val theory = LF.lftheory
   val hastype = OfType.path
}

object OldLFHOAS extends pragmatics.HOAS {
   val lfbase = DPath(utils.URI("http", "cds.omdoc.org") / "foundations" / "lf" / "lf.omdoc")
   val theory = lfbase ? "lf"
   val apply = theory ? "@"
   val lambda = theory ? "lambda"
}

