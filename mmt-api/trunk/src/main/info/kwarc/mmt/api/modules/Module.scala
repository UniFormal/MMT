package info.kwarc.mmt.api.modules
import info.kwarc.mmt.api._
import symbols._
import objects._

/**
 * A Module represents an MMT module.<p>
 *
 * @param parent the [[Path]] of the parent document
 * @param name the name of the module
 */
abstract class Module(val parent : DPath, val name : LocalName) extends ContentElement {
   def path: MPath = parent ? name
   def toTerm = OMMOD(path)
   def superModule: Term = if (name.length > 1) OMMOD(parent ? name.tail) else TheoryExp.empty
   //def parameters : Context
}

/**
 * Module given by a set of statements
 */
trait DeclaredModule extends Module with Body {
   /** the meta-theory, domain, and codomain are not part of the term components because it is just a Path */
   def getInnerContext: Context
   def asDocument: documents.Document
}

/**
 * Module given by existing modules/morphisms
 */
trait DefinedModule extends Module with ModuleDefiniens {
}

/**
 * A Module or Link given by existing modules/morphisms
 */
trait ModuleDefiniens extends StructuralElement {
  /** the TermContainer holding the definiens */
   val dfC : TermContainer
   /** the definiens as a Term */
   def df = dfC.get.get // TODO for now, we assume the definiens is always present
   protected def innerString = " = " + df.toString
   protected def innerNodes = getMetaDataNode ++ <definition>{df.toNode}</definition>
   def getDeclarations = Nil
}
