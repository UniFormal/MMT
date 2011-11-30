package info.kwarc.mmt.api.modules
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._

/**
 * A Module represents an MMT module.<p>
 * 
 * @param doc the {@link info.kwarc.mmt.api.names.Path} of the parent document
 * @param name the name of the module
 */
abstract class Module(val parent : DPath, val name : LocalPath) extends ContentElement {
   def path = parent ? name
}

/**
 * Module given by a set of statements
 */
trait DeclaredModule[S <: Declaration] extends Body[S] {
}

/**
 * Module given by existing modules/morphisms
 */
trait DefinedModule[M <: ModuleObj] extends ModuleDefiniens[M]

/**
 * A Module or Link given by existing modules/morphisms
 */
trait ModuleDefiniens[M <: ModuleObj] {
  /** the ModuleObj that constitutes the definiens */
   val df : M
   protected def innerString = " = " + df.toString
   protected def innerNodes = <definition>{df.toNode}</definition>
   protected def innerComponents = List(df)
}