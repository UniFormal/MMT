package info.kwarc.mmt.lf.hollight

import info.kwarc.mmt._
import api._
import lf._

/** added by rule in HOL Light theory */
object HOLLightHOAS extends notations.ChurchNestedHOASNotation(HOLLight.hoas, LF.hoas)
