/*

package info.kwarc.mmt.oeis

import info.kwarc.mmt.api._

import symbols._
import info.kwarc.mmt.api.parser._

class OEISPresenter extends PlanetaryPresenter {
  override val key = "oeis-pres"
  override val objectLevel = new OEISObjectPresenter

  import htmlRh._

  override def doConstant(c : Constant) {
     var attributes : List[(String, String)]= Nil
     SourceRef.get(c) foreach {sref =>
       attributes ::= ("line" -> sref.region.start.line.toString)
     }
     div(cls = "constant", attributes = attributes) {
       c.df foreach {df =>
         doMath(df)
       }
     }
   }
}
*/