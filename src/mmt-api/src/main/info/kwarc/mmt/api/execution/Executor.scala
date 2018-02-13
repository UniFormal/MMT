package info.kwarc.mmt.api.execution

import info.kwarc.mmt.api._
import objects._
import frontend._

abstract class Executor extends Extension {
   def apply(context: Context, prog: Term): Term
}
