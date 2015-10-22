package info.kwarc.mmt.tps

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.lf.Arrow
import utils._

object TPSTheory {
   val rootdpath = DPath((URI.http colon "gl.mathhub.info") / "tps")
   val thname = "tpslogic"
   val thpath = rootdpath ? thname
   abstract class sym(s: String) {
      val path = thpath ? s
      val term = OMS(path)
   }

   object proof extends sym("ded")

}