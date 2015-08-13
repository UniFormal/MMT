package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import utils._

/**
 * .mbt files schould contain a single object that extends MMTScript
 */
abstract class MMTScript extends Extension {
   def main()
}

class MMTScriptEngine(controller: Controller) {
   def apply(f: File) {
     val code = File.read(f)
     val textscript = s"object UserScript extends info.kwarc.mmt.api.frontend.MMTScript {\ndef main() {\n$code\n}\n}\nUserScript"
     import scala.reflect.runtime._
     val cm = universe.runtimeMirror(getClass.getClassLoader)
     import scala.tools.reflect.ToolBox
     val tb = cm.mkToolBox()
     //println(textscript)
     val scalascript = tb.eval(tb.parse(textscript))
     scalascript match {
        case s: MMTScript =>
           s.init(controller)
           s.main()
        case _ => 
     }
   }
}