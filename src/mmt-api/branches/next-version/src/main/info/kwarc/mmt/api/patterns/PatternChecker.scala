package info.kwarc.mmt.api.patterns

import info.kwarc.mmt.api._
import libraries._
import modules._
import frontend._
import symbols._
import objects._
import objects.Conversions._
import utils._
import scala.io.Source

/** elaborates Instance declarations
 * this is also called the pragmatic-to-strict translation
 */
class PatternChecker(controller: Controller) extends Elaborator {
   def apply(e: ContentElement) : Unit = e match {
     case c: Constant =>
     case _ =>
   }
}