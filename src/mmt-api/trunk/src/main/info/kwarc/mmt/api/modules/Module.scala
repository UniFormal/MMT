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

trait DeclaredModule[S <: Declaration, I <: Import] extends Body[S,I]

trait DefinedModule[M <: ModuleObj] extends ModuleDefiniens[M]

trait ModuleDefiniens[M <: ModuleObj] {
   val df : M
   protected def dfToNode = <definition>{df.toOBJNode}</definition>
   protected def dfToString = df.toString
   def dfNode = <definition>{df.toNode}</definition>
}