package info.kwarc.mmt.api.modules
import info.kwarc.mmt.api._
import symbols._
import libraries._
import objects._
import objects.Conversions._
import utils._

import scala.collection.mutable

/** convenience functions for explicitly omitting constructor arguments (default arguments tend to mask implementation errors) */
object Theory {
  def noMeta: Option[MPath] = None

  def noBase = new TermContainer

  def noParams = new ContextContainer

  def apply(doc: DPath,n: LocalName,mt: Option[MPath],params: ContextContainer = noParams,df: TermContainer = noBase) =
    new Theory(doc,n,mt,params,df)

  def empty(doc: DPath,n: LocalName,mt: Option[MPath]) = apply(doc,n,mt)

  /** if this theory is flattened already, an iterator over all constants is a set of theories
    *
    * @param paths        the theories and the morphisms via which they are visible (if not direct)
    * @param lup          lookup to retrieve included theories
    * @param withIncludes if false, only local declarations; if true, also included declarations
    * @param seen         theories to exclude (used by recursive implementation)
    */

  import scala.collection.mutable.HashSet

  private def flatDeclarations(lup: Lookup,from: MPath,via: Option[Term],withMeta: Boolean,withIncludes: Boolean,seen: HashSet[MPath]): List[(Declaration,Option[Term])] = {
    if (seen.contains(from)) return Nil
    seen += from
    val thy = lup.getAs(classOf[Theory],from)
    val mtDs = if (withMeta) {
      thy.meta.toList.flatMap {m =>
        flatDeclarations(lup,m,None,true,true,seen)
      }
    } else Nil
    mtDs ::: thy.getDeclarations.flatMap {
      case Include(id) =>
        if (seen.contains(id.from)) Nil
        else {
          flatDeclarations(lup,id.from,id.df,false,true,seen)
        }
      case rc: RuleConstant => List((rc,via))
      case c: Constant => List((c,via))
      case d: DerivedDeclaration => Nil
      case s: Structure => Nil
    }
  }

  /** if this theory is flattened already, an iterator over all constants is a theory
    *
    * @param path         the theory
    * @param lup          lookup to retrieve included theories
    * @param withMeta     if true, also declarations of meta-theory
    * @param withIncludes if true, also included declarations
    * @return all pairs of included declaration and (for defined includes) the mediating morphism
    */
  def flatDeclarations(lup: Lookup, path: MPath, withMeta: Boolean = false, withIncludes: Boolean = false): List[(Declaration,Option[Term])] = {
    val seen = new HashSet[MPath]
    flatDeclarations(lup, path, None, withMeta, withIncludes, seen)
  }

  /** list of all undefined constants and their types (with definitions expanded)
    * contains: constants from included theories
    * does not contain: constants from meta-theory, defined constants
    */
  def primitiveConstants(path: MPath, lup: Lookup): List[(GlobalName,Option[Term])] = {
    val defined: mutable.Set[ContentPath] = mutable.Set()
    flatDeclarations(lup, path, false, true) flatMap {
      case (c: Constant, None) =>
        c.df match {
          case None =>
            val tpE = c.tp.map(lup.ExpandDefinitions(_, defined))
            Iterator((c.path, tpE))
          case Some(_) =>
            defined += c.path
            Iterator.empty
        }
      case _ => Iterator.empty
    }
  }
}

/** abstract interface of theories and related classes, analog to [[Link]] */
trait AbstractTheory extends ModuleOrLink {
   /** the meta-theory */
   def meta: Option[MPath]
   /** the parameters */
   def parameters: Context

   /** the context governing the body: meta-theory, parameters, and this theory */
   def getInnerContext = {
      val self = IncludeVarDecl(modulePath, parameters.id.map(_.target))
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
   /** like getIncludes but also with includes of parametric theories and defined includes */
   def getAllIncludes: List[IncludeData] = meta.map(m => IncludeData(toTerm,m,Nil,None,false)).toList ::: getAllIncludesWithoutMeta
   def selfInclude = IncludeData(toTerm, modulePath, Nil, None, false)
   /** like getIncludesWithoutMeta but also with includes of parametric theories and their instantiations */
   def getAllIncludesWithoutMeta: List[IncludeData] = {
     getDeclarations.flatMap {
       case Include(id) => List(id)
       case _ => Nil
     }     
   }
   /** return all includes that are realizations */ 
   def getRealizees: List[IncludeData] = getAllIncludesWithoutMeta.filter(_.isRealization)
   /** convenience method to obtain all named structures */
   def getNamedStructures:List[Structure] = getDeclarations.flatMap {
      case s: Structure if ! s.isInclude => List(s)
      case _ => Nil
   }
   /** convenience method to obtain all derived declarations for a given feature */
   def getDerivedDeclarations(f: String) = getDeclarations.collect {
     case dd: DerivedDeclaration if dd.feature == f => dd
   }
}


/**
 * A Theory represents an MMT theory.
 *
 * Theories are constructed empty. Body is derived to hold a set of named symbols.
 *
 * @param doc as for [[Module]]
 * @param name as for [[Module]]
 * @param mt the optional meta-theory
 * @param paramC the interface/parameters/arguments of this theory
 * @param dfC the definiens/base theory of this theory
 */
class Theory(doc: DPath, name: LocalName, private var mt: Option[MPath], val paramC: ContextContainer, val dfC: TermContainer)
   extends Module(doc, name) with AbstractTheory {
   val feature = "theory"
   
   /** the container of the meta-theory */
   val metaC = TermContainer(mt.map(OMMOD(_)))

   def meta = metaC.get map {case OMMOD(mt) => mt}
   def parameters = paramC.get getOrElse Context.empty

   def getComponents = {
     val mtComp = if (meta.isDefined) List(TypeComponent(metaC)) else Nil
     val dfComp = if (dfC.isDefined) List(DefComponent(dfC)) else Nil
     val prComp = if (paramC.isDefined) List(ParamsComponent(paramC)) else Nil
     mtComp ::: dfComp ::: prComp
   }

   def translate(newNS: DPath, newName: LocalName, translator: Translator, context : Context): Theory = {
     val npar = paramC map {c => translator.applyContext(context, c)}
     val icont = getInnerContext
     val ndf = dfC map {df => translator.applyModule(icont, df)}
     val res = new Theory(newNS, newName, mt, npar, ndf)
     getDeclarations foreach {d =>
       res.add(d.translate(res.toTerm, LocalName.empty, translator,icont))
     }
     res
   }

   def outerString = feature + " " + path + meta.map(" : " + _.toPath).getOrElse("") + paramC.get.map(" > " + _).getOrElse("")

   override def headerNodes = super.headerNodes ++ paramC.get.toList.map(p => <parameters>{p.toNode}</parameters>) 
   def toNode =
      <theory name={name.last.toPath} base={doc.toPath} meta={if (meta.isDefined) meta.get.toPath else null}>
        {headerNodes ++ innerNodes}
      </theory>
   def toNodeElab =
      <theory name={name.last.toPath} base={doc.toPath} meta={if (meta.isDefined) meta.get.toPath else null}>
        {headerNodes ++ innerNodesElab}
      </theory>
   /** overridden for streaming */
   override def toNode(rh: presentation.RenderingHandler): Unit = {
      val metaS = if (meta.isDefined) s""" meta="${meta.get.toPath}"""" else ""
      rh << s"""<theory name="${name.last.toPath}" base="${doc.toPath}"$metaS>"""
      streamInnerNodes(rh)
      rh << "</theory>"
   }
}
