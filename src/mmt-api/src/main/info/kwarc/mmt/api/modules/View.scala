package info.kwarc.mmt.api.modules
import info.kwarc.mmt.api._
import objects._
import symbols._
import utils.xml.addAttrOrChild

 /**
  * an MMT view, i.e., a named morphism
  *
  * @param doc the namespace/parent document
  * @param name the name of the view
  * @param fromC the domain theory
  * @param toC the codomain theory
  * @param isImplicit true iff the link is implicit
  */
class View(doc : DPath, name : LocalName, val fromC : TermContainer, val toC : TermContainer, val dfC: TermContainer, val isImplicit : Boolean)
      extends Module(doc, name) with Link {
   val feature = "view"
   def namePrefix = LocalName(path)

   def getComponents = List(DomComponent(fromC), CodComponent(toC), DefComponent(dfC))
   
   def getInnerContext = codomainAsContext

   def translate(newNS: DPath, newName: LocalName, translator: Translator,context:Context): View = {
     def tl(m: Term) = translator.applyModule(context, m)
     val res = new View(newNS, newName, fromC map tl, toC map tl, dfC map tl, isImplicit)
     getDeclarations foreach {d =>
       res.add(d.translate(res.toTerm, LocalName.empty, translator,context))
     }
     res
   }

   protected def outerString = implicitString + feature + " " + path + " : " + from.toString + " -> " + to.toString
   def toNode = {
      val node = <view name={name.last.toPath} base={doc.toPath} implicit={if (isImplicit) "true" else null}>
           {headerNodes}{innerNodes}
         </view>
      val fromN = Obj.toStringOrNode(from)
      val toN = Obj.toStringOrNode(to)
      addAttrOrChild(addAttrOrChild(node, "to", toN), "from", fromN)
   }
}

object View {
   def apply(doc : DPath, name : LocalName, from : Term, to : Term, isImplicit: Boolean) =
      new View(doc, name, TermContainer(from), TermContainer(to), TermContainer.empty(), isImplicit)
   def apply(doc : DPath, name : LocalName, from : Term, to : Term, df: TermContainer, isImplicit: Boolean) =
      new View(doc, name, TermContainer(from), TermContainer(to), df, isImplicit)
}
