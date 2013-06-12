package info.kwarc.mmt.api.frontend

/** the base class of all MMT plugins
 * plugins are identified by a string giving the Java-style URI of the respective instance of this class
 * e.g., info.kwarc.mmt.lf.Plugin
 */
abstract class Plugin extends Extension {
   /** lists the id's of depended-on plugins
    * they will be loaded an initialized before this one if they have not been yet
    * plugins are added to a Controller by executing the AddPlugin action
    */
   val dependencies : List[String]
}