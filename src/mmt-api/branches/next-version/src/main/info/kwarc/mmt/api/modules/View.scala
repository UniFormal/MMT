package info.kwarc.mmt.api.modules
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.presentation.{StringLiteral,Omitted}

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
abstract class View(doc : DPath, name : LocalPath, val from : Term, val to : Term)
         extends Module(doc, name) with Link {
   def toMorph = OMMOD(path)
   protected def outerComponents = List(StringLiteral(name.flat), from, to)
   protected def outerString = path + " : " + from.toString + " -> " + to.toString
   def toNode = (from, to) match {
	   case (OMMOD(p), OMMOD(q)) =>
         <view name={name.flat} base={doc.toPath} from={p.toPath} to={q.toPath}>
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

 /**
  * A DeclaredView represents an MMT view given by a list of assignments.<p>
  * 
  * @param doc the {@link info.kwarc.mmt.api.names.Path} of the parent document
  * @param name the name of the view
  * @param from the domain theory
  * @param to the codomain theory
  * @param meta the optional meta-morphism
  */
class DeclaredView(doc : DPath, name : LocalPath, from : Term, to : Term)
      extends View(doc, name, from, to) with DeclaredModule[Assignment] with DeclaredLink {
   def role = info.kwarc.mmt.api.Role_View
}

  /**
   * A DefinedView represents an MMT view given by an existing morphism.<p>
   * 
   * @param doc the {@link info.kwarc.mmt.api.names.Path} of the parent document
   * @param name the name of the view
   * @param from the domain theory
   * @param to the codomain theory
   * @param df the definiens
   */
class DefinedView(doc : DPath, name : LocalPath, from : Term, to : Term, val df : Term)
      extends View(doc, name, from, to) with DefinedModule with DefinedLink {
   def role = info.kwarc.mmt.api.Role_DefinedView
}
