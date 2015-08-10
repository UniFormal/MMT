package info.kwarc.mmt.api.modules
import info.kwarc.mmt.api._
import symbols._
import libraries._
import objects._
import objects.Conversions._
import utils._

abstract class Theory(doc : DPath, name : LocalName) extends Module(doc, name) {
   def parameters: Context
}
/**
 * A Theory represents an MMT theory.<p>
 * 
 * Theories are constructed empty. Body is derived to hold a set of named symbols.
 * 
 * @param doc the URI of the parent document
 * @param name the name of the theory
 * @param meta the path of the optional meta-theory
 * @param parameters the interface/parameters/arguments of the theory
 */
// TODO find a way that does not require meta and parameters to be vars
class DeclaredTheory(doc : DPath, name : LocalName, var meta : Option[MPath], var parameters: Context = Context())
      extends Theory(doc, name) with DeclaredModule {
   def getComponents = meta.toList.map(p => (TypeComponent, new FinalTermContainer(OMMOD(p))))
   /** the context governing the body: meta-theory, parameters, and this theory */
   def getInnerContext = {
      val self = IncludeVarDecl(path, parameters.id.map(_.target))
      meta.map(p => Context(p)).getOrElse(Context()) ++ parameters ++ self
   }
   /** convenience method to obtain all constants */
   def getConstants:List[Constant] = getDeclarations.flatMap {
      case c: Constant => List(c)
      case _ => Nil
   }
   /** convenience method to obtain all included theories (including a possible meta-theory) */
   def getIncludes:List[MPath] = meta.toList ::: getIncludesWithoutMeta 
   /** convenience method to obtain all included theories (without a possible meta-theory) */
   def getIncludesWithoutMeta:List[MPath] = {
    getDeclarations.flatMap {
        case PlainInclude(from, _) => List(from)
        case _ => Nil
     }
   }   
   /** convenience method to obtain all named structures */
   def getNamedStructures:List[Structure] = getDeclarations.flatMap {
      case s: Structure if ! s.isInclude => List(s)
      case _ => Nil
   }
   /** convenience method to obtain all patterns */
   def getPatterns:List[patterns.Pattern] = getDeclarations.flatMap {
      case p: patterns.Pattern => List(p)
      case _ => Nil
   }
   /** convenience method to obtain all pattern instances */
   def getInstances:List[patterns.Instance] = getDeclarations.flatMap {
      case p: patterns.Instance => List(p)
      case _ => Nil
   }
   override def toString = "theory " + path + meta.map(" : " + _.toPath).getOrElse("") +
     (if(parameters.nonEmpty) " > " + parameters else "") + innerString
   def toNode =
      <theory name={name.last.toPath} base={doc.toPath} meta={if (meta.isDefined) meta.get.toPath else null}>
        {getMetaDataNode}
        {if (parameters.isEmpty) Nil else <parameters>{parameters.toNode}</parameters>}
        {innerNodes}
      </theory>
   def toNodeElab = 
    <theory name={name.last.toPath} base={doc.toPath}>
        {getMetaDataNode}
        {innerNodesElab}
    </theory>
   override def toNode(rh: presentation.RenderingHandler) {
      val metaS = if (meta.isDefined) s""" meta="${meta.get.toPath}"""" else ""
      rh(s"""<theory name="${name.last.toPath}" base="${doc.toPath}"$metaS>""")
      rh(getMetaDataNode)
      if (parameters.nonEmpty) rh(<parameters>{parameters.toNode}</parameters>)
      getPrimitiveDeclarations.foreach {i =>
         i.toNode(rh)
      }
      rh("</theory>")
   }
}

class DefinedTheory(doc : DPath, name : LocalName, val dfC : TermContainer) extends Theory(doc, name) with DefinedModule {
   val parameters = Context()
   def getComponents = List((DefComponent, dfC))
   override def toString = path + innerString
   def toNode = 
    <theory name={name.last.toPath} base={doc.toPath}>
        {getMetaDataNode}
        {innerNodes}
    </theory>
}

object DefinedTheory {
   def apply(doc : DPath, name : LocalName, df : Term) =
      new DefinedTheory(doc, name, TermContainer(df))
}