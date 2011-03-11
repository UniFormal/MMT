package info.kwarc.mmt.api.modules
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
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
abstract class View(doc : DPath, name : LocalPath, val from : MPath, val to : MPath) extends Module(doc, name) with Link {
   protected def main_components = List(StringLiteral(name.flat), objects.OMT(from), objects.OMT(to))
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
class DeclaredView(doc : DPath, name : LocalPath, from : MPath, to : MPath) extends View(doc, name, from, to) with DeclaredLink {
   def role = info.kwarc.mmt.api.Role_View
   def toNode =
      <view name={name.flat} cdbase={doc.toPath} from={from.toPath} to={to.toPath}>
        {statementsToNode}
      </view>
   override def toString = path + " : " + from.toPath + " -> " + to.toPath + " = " + statementsToString 
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
class DefinedView(doc : DPath, name : LocalPath, from : MPath, to : MPath, val df : Morph) extends View(doc, name, from, to) with DefinedLink {
   def role = info.kwarc.mmt.api.Role_DefinedView
   def toNode =
      <view name={name.flat} cdbase={doc.toPath} from={from.toPath} to={to.toPath}>
        {dfToNode}
      </view>
   override def toString = path + " : " + from.toPath + " -> " + to.toPath + " = " + dfToString
}
