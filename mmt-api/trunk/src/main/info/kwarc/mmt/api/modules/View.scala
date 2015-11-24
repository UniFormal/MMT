package info.kwarc.mmt.api.modules
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._

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
abstract class View(doc : DPath, name : LocalName)
         extends Module(doc, name) with Link {
   protected def outerString = path + " : " + from.toString + " -> " + to.toString
   def toNode = (from, to) match {
	   case (OMMOD(p), OMMOD(q)) =>
         <view name={name.last.toPath} base={doc.toPath} from={p.toPath} to={q.toPath} implicit={if (isImplicit) "true" else null}>
           {getMetaDataNode}
           {innerNodes}
         </view>
	   case _ => 
         <view name={name.last.toPath} base={doc.toPath}>
           {getMetaDataNode}
           <from>{from.toOBJNode}</from><to>{to.toOBJNode}</to>
           {innerNodes}
         </view>
   }
}

 /**
  * A DeclaredView represents an MMT view given by a list of assignments.
  * 
  * Assignments are [[Constant]]s, whose name is the qualified name (always starts with [[ComplexStep]])
  * of a domain Constant
  * and whose definiens is codomain [[Term]].
  * 
  * @param doc the namespace/parent document
  * @param name the name of the view
  * @param from the domain theory
  * @param to the codomain theory
  * @param isImplicit true iff the link is implicit
  */
class DeclaredView(doc : DPath, name : LocalName, val from : Term, val to : Term, val isImplicit : Boolean)
      extends View(doc, name) with DeclaredModule with DeclaredLink {
   def getIncludes: List[MPath] = getDeclarations.flatMap {
      case PlainViewInclude(_,_,v) => List(v)
      case _ => Nil
   }
   def getInnerContext = codomainAsContext
   def getComponents = List(DomComponent(new FinalTermContainer(from)),CodComponent(new FinalTermContainer(to)))
}

  /**
   * A DefinedView represents an MMT view given by an existing morphism.
   * 
   * @param doc the URI of the parent document
   * @param name the name of the view
   * @param from the domain theory
   * @param to the codomain theory
   * @param dfC the definiens
   * @param isImplicit true iff the link is implicit
   */
class DefinedView(doc : DPath, name : LocalName, val from : Term, val to : Term, val dfC : TermContainer, val isImplicit : Boolean)
      extends View(doc, name) with DefinedModule with DefinedLink {
   def getComponents = List(DeclarationComponent(DefComponent, dfC))
}

object DefinedView {
   def apply(doc : DPath, name : LocalName, from : Term, to : Term, df : Term, isImplicit: Boolean) =
      new DefinedView(doc, name, from, to, TermContainer(df), isImplicit)
}
