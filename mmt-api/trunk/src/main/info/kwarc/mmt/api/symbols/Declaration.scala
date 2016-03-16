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

   /** the containing document if there is a document between this declaration and the containing module */
   /* This is meant to be provided when constructing the declaration.
      But it is realized as a variable because it would be too tedious to add it as a constructor argument everywhere.
    */
   private var relDocHome_ = LocalName.empty
   def setDocumentHome(ln: LocalName) {
      relDocHome_ = ln
   }
   def relativeDocumentHome = relDocHome_
   def documentHome = parent.toDPath / relDocHome_
   override def merge(that: StructuralElement) {
      that match {
         case that: Declaration =>
           relDocHome_ = that.relDocHome_
         case _ => throw ImplementationError("not a declaration")
      }
   }
   
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
   def alternativeNames: List[LocalName] = Nil
   /** the full MMT URI, parent ? name */
   def path = GlobalName(parent, name)
   /** the OMS referencing this declaration */
   def toTerm = OMS(path)
   /** the component used to identify anonymous declarations, e.g., the from of an import, None by default but may be overridden */ 
   def implicitKey : Option[MPath] = None
}

/** declarations that have a notation */
trait HasNotation {
   def notC: NotationContainer
   def not = notC.parsing
}

class NestedModule(val module: Module) extends Declaration with ModuleWrapper {
  val home = OMMOD(module.parent ? module.name.init)
  val name = LocalName(module.name.last)
  
}