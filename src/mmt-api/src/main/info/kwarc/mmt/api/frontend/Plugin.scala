package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils.MMT_TODO

/** extensions that provide semantics for MMT theories
  */
@MMT_TODO("contents of plugins should become rules, registered build targets (which are loaded on demand) or content-generating operators that are declared in theories")
abstract class Plugin extends Extension {
   val theory: MPath
   /** lists the id's of depended-on plugins
    * they will be loaded an initialized before this one if they have not been yet
    */
   val dependencies : List[String]
}
