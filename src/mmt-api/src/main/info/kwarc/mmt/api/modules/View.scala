package info.kwarc.mmt.api.modules
import info.kwarc.mmt.api._
import objects._
import symbols._
import utils.xml.addAttrOrChild

/**
 * A View represents an MMT view.
 * 
 * Views may be declared (given by a list of assignments) or defined (given by an existing morphism).
 * These cases are distinguished by which subtrait of Link is mixed in.
 * 
 * @param doc the URI of the parent document
 * @param name the name of the view
 */
abstract class View(doc : DPath, name : LocalName) extends Module(doc, name) with Link {
   def namePrefix = LocalName(path)
   protected def outerString = path + " : " + from.toString + " -> " + to.toString
   def toNode = {
      val node = <view name={name.last.toPath} base={doc.toPath} implicit={if (isImplicit) "true" else null}>
           {innerNodes}
         </view>
      val fromN = Obj.toStringOrNode(from)
      val toN = Obj.toStringOrNode(to)
      addAttrOrChild(addAttrOrChild(node, "to", toN), "from", fromN)
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

   def translate(newNS: DPath, newName: LocalName, translator: Translator): DeclaredView = {
     def tl(m: Term)= translator.applyModule(Context.empty, m)
     val res = new DeclaredView(newNS, newName, tl(from), tl(to), isImplicit)
     getDeclarations foreach {d =>
       res.add(d.translate(res.toTerm, LocalName.empty, translator))
     }
     res
   }
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
   def translate(newNS: DPath, prefix: LocalName, translator: Translator): DefinedModule = {
     def tl(m: Term)= translator.applyModule(Context.empty, m)
     new DefinedView(newNS, prefix/name, tl(from), tl(to), dfC map tl, isImplicit)
   }
}

object DefinedView {
   def apply(doc : DPath, name : LocalName, from : Term, to : Term, df : Term, isImplicit: Boolean) =
      new DefinedView(doc, name, from, to, TermContainer(df), isImplicit)
}
