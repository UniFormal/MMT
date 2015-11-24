package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import modules._
import objects._
import libraries._
import notations._

/**
 * Declaration unifies MMT symbols and MMT assignments.
 * 
 * These are the named statements living in [[info.kwarc.mmt.api.modules.Module]]s
 */
abstract class Declaration extends ContentElement {
   /** the containing module */
   lazy val parent = home.toMPath
   /** the containing module
    * 
    * this is almost always OMMOD(p:MPath),
    * the main exception are generated anonymous modules
    */
   val home : Term
   /** the local name in the containing module
    * 
    *  for symbols: the name of the symbols
    *  
    *  for assignments: the name of the symbols to which a value is assigned 
    */
   val name : LocalName
   /** an alternative name
    * 
    *  None by default; overridden in particular by Constant
    */
   def alternativeName: Option[LocalName] = None
   /** the full MMT URI, parent ? name */
   def path = GlobalName(parent, name)
   /** the component used to identify anonymous declarations, e.g., the from of an import, None by default but may be overridden */ 
   def implicitKey : Option[MPath] = None
}

/** declarations that have a notation */
trait HasNotation {
   def notC: NotationContainer
   def not = notC.parsing
}

class NestedModule(val module: Module) extends Declaration {
   val home = OMMOD(module.parent ? module.name.init)
   val name = LocalName(module.name.last)
   
   def toNode = module.toNode
   override def toString = module.toString
   def getComponents = module.getComponents 
   def getDeclarations = module.getDeclarations

   // bend over metadata pointer
   this.metadata = module.metadata
}