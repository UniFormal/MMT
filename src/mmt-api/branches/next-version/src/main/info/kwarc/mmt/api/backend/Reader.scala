package info.kwarc.mmt.api.backend

import info.kwarc.mmt.api._
import metadata._
import documents._
import modules._
import objects._
import symbols._
import utils._

/**  */
abstract class Reader(controller : frontend.Controller) {
   val report = controller.report
}