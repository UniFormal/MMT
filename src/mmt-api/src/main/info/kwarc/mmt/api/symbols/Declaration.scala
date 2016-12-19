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
   /** to allow for sharper types of fields, every subclass of Declaration defines this to be itself */
   type ThisType >: this.type <: Declaration
  
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
    * 9
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

   /** a recursively translated copy of this declaration */
   def translate(newHome: Term, prefix: LocalName, translator: Translator): ThisType
   /** a new declaration with the same path obtained by replacing fields in 'this' with corresponding fields of 'that'
    *  Unfortunately, this must take any declaration and throw an error if 'not (that : ThisType)' 
    */
   def merge(that: Declaration): ThisType

   /** called to throw an error from within 'merge' */
   protected def mergeError(that: Declaration): Nothing = throw GeneralError("cannot merge " + that.path + " into " + this.path)
}

/** declarations that have a notation */
trait HasNotation {
   def notC: NotationContainer
   def not = notC.parsing
}

/** a [[Module]] as a [[Declaration]], i.e., inside some other module
 *  @param home the containing module
 *  @param name the name in that module
 *  @param mod the contained module
 *  invariant for non-induced NestedModule's: module.path == parent / name
 */
class NestedModule(val home: Term, val name: LocalName, mod: Module) extends Declaration with ModuleWrapper {
   type ThisType = NestedModule
   def module = mod
   //val home = OMMOD(module.parent ? module.name.init)
   //val name = LocalName(module.name.last)
   def translate(newHome: Term, prefix: LocalName, translator: Translator): NestedModule = {
     val modT = mod.translate(utils.mmt.mmtbase, LocalName(newHome.toMPath/prefix)/name, translator)
     new NestedModule(newHome, prefix/name, modT)
   }
   //TODO this is where patterns (seen as defined theories) can be mapped to larger theories
   def merge(that: Declaration): NestedModule = {
     mergeError(that)
   }

}