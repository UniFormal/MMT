package info.kwarc.mmt.api.modules
import info.kwarc.mmt.api._
import symbols._
import objects._
import libraries._
import utils._
import presentation.{StringLiteral,Omitted}

/**
 * A Module represents an MMT module.<p>
 * 
 * @param doc the {@link info.kwarc.mmt.api.names.Path} of the parent document
 * @param name the name of the module
 */
sealed abstract class Module(val parent : DPath, val name : LocalPath) extends ContentElement {
   def path: MPath = parent ? name
   def toTerm = OMMOD(path)
}

/**
 * Module given by a set of statements
 */
trait DeclaredModule[S <: Declaration] extends Body[S] {
}

/**
 * Module given by existing modules/morphisms
 */
trait DefinedModule extends ModuleDefiniens

/**
 * A Module or Link given by existing modules/morphisms
 */
trait ModuleDefiniens {
  /** the Term that constitutes the definiens */
   val df : Term
   protected def innerString = " = " + df.toString
   protected def innerNodes = <definition>{df.toNode}</definition>
   protected def innerComponents = List(df)
}

class SFModule(parent : DPath, val tokens : List[SFModuleElem]) extends Module(parent, LocalPath.Anon) {
  def components = presentation.StringLiteral(parent.toPath) :: tokens.flatMap(_.components)
  def role = Role_SFModule
  def toNode = <SFModule parent={parent.toPath}>{tokens.map(_.toNode)}</SFModule>
}


abstract class Theory(doc : DPath, name : LocalPath) extends Module(doc, name)

/**
 * A View represents an MMT view.<p>
 * 
 * Views may be declared (given by a list of assignments) or defined (given by an existing morphism).
 * These cases are distinguished by which subtrait of Link is mixed in.
 * 
 * @param doc the {@link info.kwarc.mmt.api.names.Path} of the parent document
 * @param name the name of the view
 * @param from the domain theory
 * @param to the codomain theory
 */
abstract class View(doc : DPath, name : LocalPath)
         extends Module(doc, name) with Link {
   protected def outerComponents = List(StringLiteral(name.flat), from, to)
   protected def outerString = path + " : " + from.toString + " -> " + to.toString
   def toNode = (from, to) match {
	   case (OMMOD(p), OMMOD(q)) =>
         <view name={name.flat} base={doc.toPath} from={p.toPath} to={q.toPath} implicit={if (isImplicit) "true" else null}>
           {getMetaDataNode}
           {innerNodes}
         </view>
	   case _ => 
         <view name={name.flat} base={doc.toPath}>
           {getMetaDataNode}
           <from>{from.toOBJNode}</from><to>{to.toOBJNode}</to>
           {innerNodes}
         </view>
   }
}

