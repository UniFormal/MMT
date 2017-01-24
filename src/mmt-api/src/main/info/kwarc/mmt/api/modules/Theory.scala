package info.kwarc.mmt.api.modules
import info.kwarc.mmt.api._
import symbols._
import libraries._
import objects._
import objects.Conversions._
import utils._

abstract class Theory(doc : DPath, name : LocalName) extends Module(doc, name) {
   val feature = "theory"
   def parameters: Context
}
/**
 * A Theory represents an MMT theory.<p>
 * 
 * Theories are constructed empty. Body is derived to hold a set of named symbols.
 * 
 * @param doc the URI of the parent document
 * @param name the name of the theory
 * @param mt the optional meta-theory
 * @param parameters the interface/parameters/arguments of the theory
 */
class DeclaredTheory(doc : DPath, name : LocalName, mt : Option[MPath], val paramC: ContextContainer = new ContextContainer)
      extends Theory(doc, name) with DeclaredModule {
   /** the container of the meta-theory */
   val metaC = TermContainer(mt.map(OMMOD(_)))
   /** the meta-theory */
   def meta = metaC.get map {case OMMOD(mt) => mt}
   /** @return the parameters */
   def parameters = paramC.get getOrElse Context.empty

   def getComponents = {
     val mtComp = if (metaC.isDefined) List(DeclarationComponent(TypeComponent, metaC)) else Nil
     val prComp = if (paramC.isDefined) List(DeclarationComponent(ParamsComponent, paramC)) else Nil
     mtComp ::: prComp
   }
   
   /** the context governing the body: meta-theory, parameters, and this theory */
   def getInnerContext = {
      val self = IncludeVarDecl(path, parameters.id.map(_.target))
      meta.map(p => Context(p)).getOrElse(Context.empty) ++ parameters ++ self
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
   /** convenience method to obtain all derived declarations for a given feature */
   def getDerivedDeclarations(f: String) = getDeclarations.collect {
     case dd: DerivedDeclaration if dd.feature == f => dd
   }

   override def toString = "theory " + path + meta.map(" : " + _.toPath).getOrElse("") +
     (if(parameters.nonEmpty) " > " + parameters else "") + "\n" + innerString
   def toNode =
      <theory name={name.last.toPath} base={doc.toPath} meta={if (meta.isDefined) meta.get.toPath else null}>
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
      rh << s"""<theory name="${name.last.toPath}" base="${doc.toPath}"$metaS>"""
      if (parameters.nonEmpty)
         rh(<parameters>{parameters.toNode}</parameters>)
      streamInnerNodes(rh)
      rh << "</theory>"
   }
   def translate(newNS: DPath, newName: LocalName, translator: Translator, context : Context): DeclaredTheory = {
     val res = new DeclaredTheory(newNS, newName, mt)
     val ncont = context ++ parameters
     getDeclarations foreach {d =>
       res.add(d.translate(res.toTerm, LocalName.empty, translator,ncont))
     }
     res
   }
}

class DefinedTheory(doc : DPath, name : LocalName, val dfC : TermContainer) extends Theory(doc, name) with DefinedModule {
   val parameters = Context()
   def getComponents = List(DefComponent(dfC))
   def df = dfC.get.getOrElse(TheoryExp.empty)
   override def toString = path + innerString
   def toNode = 
    <theory name={name.last.toPath} base={doc.toPath}>
        {innerNodes}
    </theory>
   def translate(newNS: DPath, newName: LocalName, translator: Translator, context : Context): DefinedTheory = {
     new DefinedTheory(newNS, newName, dfC.map(translator.applyModule(context, _)))
   }
}

object DefinedTheory {
   def apply(doc : DPath, name : LocalName, df : Term) =
      new DefinedTheory(doc, name, TermContainer(df))
}