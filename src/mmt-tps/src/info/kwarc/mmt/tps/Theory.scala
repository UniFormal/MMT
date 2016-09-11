package info.kwarc.mmt.tps

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.lf.Arrow
import utils._

object TPSTheory {
   val rootdpath = DPath((URI.http colon "gl.mathhub.info") / "tps")
   val thname = "tpslogic"
   val thpath = (rootdpath / thname) ? "simpletypes"
   abstract class sym(th:String,s: String) {
      val path = (rootdpath / thname) ? LocalName(th) ? s
      val term = OMS(path)
   }

   object proof extends sym("pl0","ded")
   object tp extends sym("simpletypes","all_types")
   object tm extends sym("simpletypes","tm")
   object ded extends sym("pl0","ded")
}