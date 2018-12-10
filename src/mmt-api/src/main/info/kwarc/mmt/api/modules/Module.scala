package info.kwarc.mmt.api.modules
import info.kwarc.mmt.api._
import symbols._
import objects._

/**
 * MMT modules, unifies theories and views
 *
 * @param parent the [[Path]] of the parent document
 * @param name the name of the module
 */
abstract class Module(val parent : DPath, val name : LocalName) extends ModuleOrLink {
   def path: MPath = parent ? name
   def toTerm = OMMOD(path)
   def superModule: Option[MPath] = if (name.length > 1) Some(parent ? name.init) else None
   //def parameters : Context
   def translate(newNS: DPath, newName: LocalName, translator: Translator, context : Context): Module

   //protected def innerString = dfC.get.map(df => " = " + df.toString).getOrElse("")
   //protected def innerNodes = getMetaDataNode ++ dfC.get.map(df => <definition>{df.toNode}</definition>).getOrElse(Nil)
}

/**
 * theories and views are [[Module]]; structures and views are [[Link]]s
 * this class carries the common structure, in particular the body and the optional definiens
 */
trait ModuleOrLink extends ContentElement with Body with Definable {
   // sharper type
   def getDeclarations: List[Declaration]
   /** the narrative inner structure */
   def asDocument: documents.Document
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
