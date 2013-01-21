package info.kwarc.mmt.api.frontend

/** the base class of all MMT plugins
 * plugins are identified by a string giving the Java-style URI of the respective instance of this class
 * e.g., info.kwarc.mmt.lf.Plugin
 */
abstract class Plugin {
   /** lists the id's of depended-on plugins
    * they will be loaded an initialized before this one if they have not been yet
    * plugins are added to a Controller by executing the AddPlugin action
    */
   val dependencies : List[String]
   /**
    * initializes the plugin
    * plugins may do with the Controller whatever they want
    * but if they have to cleanup after themselves in the cleanup method
    * @param c the controller that received the command to add the plugin
    * @param args any arguments that were passed in that command 
    */
   def init(c: Controller, args: List[String])
   /** called when the plugin is unloaded
    *  this method must release all resources that are not anyway released by garbage collection
    *  in particular, additional threads created by the Plugin must be terminated
    *  does nothing by default
    */
   def cleanup(c: Controller) {}
}