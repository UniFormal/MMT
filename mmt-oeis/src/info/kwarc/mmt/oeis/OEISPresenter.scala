package info.kwarc.mmt.oeis

import info.kwarc.mmt.api._

import symbols._
import info.kwarc.mmt.planetary._

class OEISPresenter extends PlanetaryPresenter {
  override val key = "oeis-pres"
  override val objectLevel = new OEISObjectPresenter
  
  import htmlRh._
  
  override def doConstant(c : Constant) {
     div(cls = "constant") {
       c.df foreach {df =>
         doMath(df)
       }
     }
   }
}
