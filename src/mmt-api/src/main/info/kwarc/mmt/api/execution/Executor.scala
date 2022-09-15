package info.kwarc.mmt.api.execution

import info.kwarc.mmt.api._
import objects._
import frontend._
import info.kwarc.mmt.api.modules.Theory


abstract class Executor extends Extension {
   def apply(theory: Theory,context: Context, prog: Term): Term
}

