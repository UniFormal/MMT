package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._

/** extensions that provide semantics for MMT theories
  */
abstract class Plugin extends Extension {
   val theory: MPath
   /** lists the id's of depended-on plugins
    * they will be loaded an initialized before this one if they have not been yet
    */
   val dependencies : List[String]
}
