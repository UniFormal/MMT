package info.kwarc.mmt.api.modules
import info.kwarc.mmt.api._
import symbols._
import objects._

/**
 * A Module represents an MMT module.
 *
 * @param parent the [[Path]] of the parent document
 * @param name the name of the module
 */
abstract class Module(val parent : DPath, val name : LocalName) extends ContentElement {
   def path: MPath = parent ? name
   def toTerm = OMMOD(path)
   def superModule: Option[MPath] = if (name.length > 1) Some(parent ? name.tail) else None
   // sharper type
   def getDeclarations: List[Declaration]
   //def parameters : Context
   def translate(newNS: DPath, prefix: LocalName, translator: Translator, context : Context): Module
}

/**
 * Module given by a set of statements
 */
trait DeclaredModule extends Module with Body {
   /** the meta-theory, domain, and codomain are not part of the term components because it is just a Path */
   def getInnerContext: Context
   def asDocument: documents.Document
   def translate(newNS: DPath, prefix: LocalName, translator: Translator, context : Context): DeclaredModule
}

/**
 * Module given by existing modules/morphisms
 */
trait DefinedModule extends Module with ModuleDefiniens {
   def translate(newNS: DPath, prefix: LocalName, translator: Translator, context : Context): DefinedModule
}

/**
 * A [[ContentElement]] that that is defined via a module
 */
trait ModuleWrapper extends ContentElement {
  def module : Module
  def getComponents = module.getComponents 
  def getDeclarations = module.getDeclarations
  // bend over metadata pointer
  this.metadata = module.metadata
  //default
  def toNode = module.toNode
  override def toString = module.toString
}

/**
 * A Module or Link given by existing modules/morphisms
 */
trait ModuleDefiniens extends StructuralElement {
  /** the TermContainer holding the definiens */
   val dfC : TermContainer
   /** the definiens as a Term */
   def df : Term
   protected def innerString = " = " + df.toString
   protected def innerNodes = getMetaDataNode ++ <definition>{df.toNode}</definition>
   def getDeclarations = Nil
}

