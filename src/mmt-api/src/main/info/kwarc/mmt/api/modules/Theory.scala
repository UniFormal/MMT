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

/** convenience functions for explicitly omitting constructor arguments (default arguments tend to mask implementation errors) */
object Theory {
   def noMeta: Option[MPath] = None
   def noBase = new TermContainer
   def noParams = new ContextContainer
   
   def empty(doc: DPath, n: LocalName, mt: Option[MPath]) = new DeclaredTheory(doc, n, mt, noParams, noBase)
}

/**
 * A Theory represents an MMT theory.
 * 
 * Theories are constructed empty. Body is derived to hold a set of named symbols.
 * 
 * @param doc the URI of the parent document
 * @param name the name of the theory
 * @param mt the optional meta-theory
 * @param paramC the interface/parameters/arguments of this theory
 * @param dfC the definiens/base theory of this theory
 */
class DeclaredTheory(doc : DPath, name : LocalName, private var mt : Option[MPath], val paramC: ContextContainer, val dfC: TermContainer)
      extends Theory(doc, name) with DeclaredModule {
   /** the container of the meta-theory */
   def metaC = TermContainer(mt.map(OMMOD(_)))
   /** the meta-theory */
   def meta = metaC.get map {case OMMOD(mt) => mt}
   /** the parameters */
   def parameters = paramC.get getOrElse Context.empty
   /** the base theory */
   def df = dfC.get

   @deprecated("awkward hack", "")
   def addMeta(mp : MPath) = mt match {
    case Some(mti) if mti != mp =>
      throw GeneralError("Theory " + path + " already has meta theory " + mti)
    case _ => mt = Some(mp)
  }

   def getComponents = {
     val mtComp = if (metaC.isDefined) List(DeclarationComponent(TypeComponent, metaC)) else Nil
     val dfComp = if (dfC.isDefined) List(DeclarationComponent(DefComponent, dfC)) else Nil
     val prComp = if (paramC.isDefined) List(DeclarationComponent(ParamsComponent, paramC)) else Nil
     mtComp ::: dfComp ::: prComp
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
        {if (df.isEmpty) Nil else <definition>{df.get.toNode}</definition>}
        {innerNodes}
      </theory>
   def toNodeElab = 
    <theory name={name.last.toPath} base={doc.toPath}>
        {getMetaDataNode}
        {if (parameters.isEmpty) Nil else <parameters>{parameters.toNode}</parameters>}
        {innerNodesElab}
    </theory>
   override def toNode(rh: presentation.RenderingHandler) {
      val metaS = if (meta.isDefined) s""" meta="${meta.get.toPath}"""" else ""
      rh << s"""<theory name="${name.last.toPath}" base="${doc.toPath}"$metaS>"""
      if (parameters.nonEmpty)
         rh(<parameters>{parameters.toNode}</parameters>)
      df.foreach {d => rh(<definition>{d.toNode}</definition>)}
      streamInnerNodes(rh)
      rh << "</theory>"
   }
   def translate(newNS: DPath, newName: LocalName, translator: Translator, context : Context): DeclaredTheory = {
     val npar = paramC map {c => translator.applyContext(context, c)}
     val icont = getInnerContext
     val ndf = dfC map {df => translator.applyModule(icont, df)}
     val res = new DeclaredTheory(newNS, newName, mt, npar, ndf)
     getDeclarations foreach {d =>
       res.add(d.translate(res.toTerm, LocalName.empty, translator,icont))
     }
     res
   }
}

@deprecated("use dfC in DeclaredTheory", "")
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