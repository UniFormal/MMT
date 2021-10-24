package info.kwarc.mmt.mizar.syntax

/**
 * The following classes are used by an XML parser utility part of the MMT API to parse Mizar content already exported to esx files
 * scala case classes
 *
 * Any case class (which doesn't extend Group) represents an XML tag of the same name (but with - replaced by _)
 * arguments starting with _ represent children of the tag and arguments without correspond to XML attributes
 * arguments which are instances of case classes extending Group are used to group up several attributes or children
 * that commonly occur together in XML tags, otherwise they correspond to XML attributes of the same name
 * arguments of type List[A] or Option[A] correspond to Lists or optional attributes or children respectively
 *
 * This parsing is further documented in the class mmt.api.utils.XMLtoScala.
 *
 * The file is contains some classes to represent attributes or children we want to group up, or for which we want to implement
 * some common additional methods usually for further parsing or checking
 *
 * Traits are used to group up classes representing XML tags defining similar objects, for instance terms Patterns, ...
 *
 * The case classes (and some traits grouping them) corresponding to declaration-level XML tags are defined in the DeclarationLevel file,
 * the ones corresponding to object-level content are defined in the ObjectLevel file
 */

import info.kwarc.mmt.api.parser.{SourcePosition, SourceRef, SourceRegion}
import info.kwarc.mmt.api.ImplementationError
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.mizar.syntax.Utils._
import info.kwarc.mmt.mizar.translator.TranslationController

/**
 * A position in a Mizar source file
 * @param position A string specifying the position in the line\\column format
 */
case class Position(position:String) extends Group  {
  def parsePosition() : SourcePosition = {
    val poss = position.split('\\')
    assert(poss.length == 2 , "Expected position argument of the form line \\ column, but found argument: "+position)
    val List(line, col) = poss.toList.map(_.toInt)
    SourcePosition(-1, line, col)
  }
}

/**
 * Contains the start and end position for an item or a block in a Mizar source file
 * @param position the start position
 * @param endposition the end position in the line\\column format
 */
case class Positions(position: Position, endposition: String) extends Group {
  def startPosition() : SourcePosition = {
    position.parsePosition()
  }
  def endPosition() : SourcePosition = {
    Position(endposition).parsePosition()
  }
  def sourceRegion() : SourceRegion = {
    SourceRegion(startPosition(), endPosition())
  }
  def sourceRef(): SourceRef = {
    val container = URI(TranslationController.currentSource)
    SourceRef.apply(container, sourceRegion())
  }
}
/**
 * A label that can later be referenced
 * @param MMLId the label, typically the article followed by a running counter of id of the same type
 */
case class MMLId(MMLId:String) extends Group {
  /**
   * Given the kind of object, construct a unique identifier from the label
   * @param kind said kind
   * @return the identifier
   */
  def globalName(kind: String) = {
    val Array(aid, ln) = MMLId.split(":")
    makeGlobalName(aid, kind, ln)
  }
}
/**
 * Contains the attribute leftargscount and the child Arguments
 * used in various functor terms
 */
case class InfixedArgs(leftargscount:Int, _args:Arguments) extends Group
/**
 * Two children consisting of a claim with its justification as commonly given as arguments to Statements
 * the justification can be ommitted iff the claim is an Iterative-Equality (which contains its own justification)
 * the check method verifies that the justification is only omitted for Iterative-Equalities
 * @param _claim the claim
 * @param _just (optional) the justification for the claim
 */
case class ProvedClaim(_claim:Claim, _just:Option[Justification]) extends Group
/**
 * Two common attributes for Object (terms and types) definitions
 * @param spelling
 * @param sort
 */
case class ObjectAttrs(spelling:String, sort:String) extends Group
/**
 * A minimal list of common attributes for objects containing only spelling, sort and nr
 * used for local objects locally referencing something with the nr, e.g. Placeholder terms or Internal selector terms
 * @param pos the corresponding position in the Mizar source file
 * @param nr the nr
 * @param spelling the spelling of the object
 * @param sort the sort of the object
 */
case class RedObjAttr(pos: Position, nr: Int, spelling:String, sort:String) extends Group
/**
 * common trait for ConstrObjAttrs and ReDefObjAttrs, which define common attributes for globally referencing objects
 */
trait ReferencingConstrObjAttrs extends Group {
  def sort: String
  def spelling: String
  def pos: Position
  def globalDefAttrs: GlobalDefAttrs
}
/**
 * Common trait for globally referencing objects
 */
trait GloballyReferencingObjAttrs {
  def globalObjAttrs : GlobalObjAttrs
  def globalKind: Char

  protected def absoluteName(str: String)  = {
    val Array(aid, ln) = str.split(":") match {
      case Array(a, b) if (! (a.isEmpty || b.isEmpty)) => Array(a, b)
      case other =>
        throw ImplementationError("")
    }
    makeGlobalKindName(aid, globalKind, ln)
  }
  def globalPatternName = absoluteName(globalObjAttrs.absolutepatternMMLId)
}
/**
 * A global reference to a pattern of a (re)-definition in mizar
 * @param absolutepatternMMLId
 */
case class GlobalObjAttrs(absolutepatternMMLId: String) extends Group
/**
 * trait for objects globally referencing definitions
 */
trait GloballyReferencingDefAttrs extends GloballyReferencingObjAttrs {
  def globalDefAttrs : GlobalDefAttrs
  override def globalObjAttrs: GlobalObjAttrs = GlobalObjAttrs(globalDefAttrs.absolutepatternMMLId)

  def globalConstrName  = absoluteName(globalDefAttrs.absoluteconstrMMLId)
  protected def absolutePatConstrName[T](p: T, c: T) = (p, c) match {
    case (SimpleGlobalName(pAid, pName), SimpleGlobalName(_, cName)) =>
      SimpleGlobalName(pAid, pName+cName)
    case _ => throw ImplementationError("Error constructing the name from the identifier "+p+" and "+c)
  }
  def globalPatConstrName = absolutePatConstrName (globalPatternName, globalConstrName)
}
/**
 * global references to both a pattern and the corresponding constructor of a (re)-definition in mizar
 * @param absolutepatternMMLId
 * @param absoluteconstrMMLId
 */
case class GlobalDefAttrs(absolutepatternMMLId: String, absoluteconstrMMLId: String) extends Group
/**
 * A global reference to the initial pattern and constructor
 * of the referenced definition, in case it got redefined
 * @param absoluteorigpatternMMLId
 * @param absoluteorigconstrMMLId
 */
case class GlobalOrgAttrs(absoluteorigpatternMMLId: Option[String], absoluteorigconstrMMLId: Option[String], superfluous: Option[Int]) extends Group {
  def isDefinedPat = absoluteorigpatternMMLId.nonEmpty
  def isDefinedConstr = absoluteorigconstrMMLId.nonEmpty
}
/**
 * trait for objects globally referencing redefinable definitions
 */
trait GloballyReferencingReDefAttrs extends GloballyReferencingDefAttrs {
  def globalReDefAttrs : GlobalReDefAttrs
  override def globalDefAttrs : GlobalDefAttrs = globalReDefAttrs.globalDefAttrs

  def globalOrgPatternName  = absoluteName (globalReDefAttrs.globalOrgAttrs.absoluteorigpatternMMLId.getOrElse(globalDefAttrs.absolutepatternMMLId))
  def globalOrgConstrName = absoluteName (globalReDefAttrs.globalOrgAttrs.absoluteorigconstrMMLId.getOrElse(globalDefAttrs.absoluteconstrMMLId))
  def globalOrgPatConstrName = absolutePatConstrName (globalOrgPatternName, globalOrgConstrName)
}
/**
 * A global reference to a pattern and constructor in mizar, as well as references to the initial pattern and constructor
 * of this definition, in case it got redefined
 * @param globalDefAttrs
 * @param globalOrgAttrs
 */
case class GlobalReDefAttrs(globalDefAttrs: GlobalDefAttrs, globalOrgAttrs: GlobalOrgAttrs) extends Group {
  def hasOrigRefs = globalOrgAttrs.isDefinedPat && globalOrgAttrs.isDefinedConstr
}
/**
 * common attributes for objects globally referencing definitions
 * @param sort the sort of the object
 * @param spelling the presentation of the object
 * @param pos the corresponding position in the Mizar source file
 * @param globalDefAttrs
 */
case class ConstrObjAttrs(spelling:String, sort:String, pos: Position, globalDefAttrs: GlobalDefAttrs) extends ReferencingConstrObjAttrs
/**
 * common attributes for objects globally referencing a (re)-definitions
 * @param sort The sort of object
 * @param spelling the presentation of the object
 * @param pos the corresponding position in the Mizar source file
 * @param globalReDefAttrs
 */
case class ReDefObjAttrs(spelling:String, sort:String, pos: Position, globalReDefAttrs: GlobalReDefAttrs) extends ReferencingConstrObjAttrs {
  override def globalDefAttrs: GlobalDefAttrs = globalReDefAttrs.globalDefAttrs
}
/**
 * common attribute and children for patterns in mizar
 */
sealed trait PatDefs extends Group {
  def patAttr: PatternAttrs
  def _locis: List[Loci]
  def globalObjAttrs: GlobalObjAttrs = patAttr.globalObjAttrs
  def patDef : PatDef = PatDef(patAttr, _locis)
}
/**
 * Common attributes for patterns in mizar
 * @param formatdes
 * @param spelling
 * @param pos the corresponding position in the Mizar source file
 * @param globalObjAttrs
 */
case class PatternAttrs(formatdes:String, spelling:String, pos: Position, globalObjAttrs: GlobalObjAttrs) extends Group
/**
 * A minimal list of attributes and children for patterns in mizar
 * @param patAttr
 * @param _locis
 */
case class PatDef(patAttr:PatternAttrs, _locis:List[Loci]) extends PatDefs
/**
 * Common attributes for ConstrPatterns in mizar
 * @param patAttr
 * @param absoluteconstrMMLId
 */
case class ConstrPatAttr(patAttr:PatternAttrs, absoluteconstrMMLId: String) extends Group {
  def globalDefAttrs = GlobalDefAttrs(patAttr.globalObjAttrs.absolutepatternMMLId, absoluteconstrMMLId)
}
/**
 * Common attribute and children for ConstrPatterns in mizar
 * @param constrPatAttr
 * @param _locis
 */
case class ConstrPatDef(constrPatAttr: ConstrPatAttr, _locis:List[Loci]) extends PatDefs {
  override def patAttr: PatternAttrs = constrPatAttr.patAttr
}
/**
 * Common attributes for RedefinablePatterns in mizar
 * @param constrPatAttr
 * @param globalOrgAttrs
 */
case class RedefinablePatAttr(constrPatAttr:ConstrPatAttr, globalOrgAttrs: GlobalOrgAttrs) extends Group {
  def globalReDefAttrs = GlobalReDefAttrs(constrPatAttr.globalDefAttrs, globalOrgAttrs)
}
/**
 * Common attributes and children for RedefinablePatterns i mizar
 * @param redefPatAttr
 * @param _loci
 * @param _locis
 */
case class RedefinablePatDef(redefPatAttr: RedefinablePatAttr, _loci:List[Locus], _locis:List[Loci]) extends PatDefs {
  override def patAttr = redefPatAttr.constrPatAttr.patAttr
}

/**
 * A single case definien consisting of a single expression
 * The expression is optional, since in any instance a single case expression may be left out in favor of a case-by-case definition
 * @param _expr (optional) the expression
 */
case class SingleCaseExpr(_expr:Option[Expression]) extends Group
/**
 * A case-by-case definition consisting of a partial definiens list and a default definien
 * Both are optional since, in any instance they can also be left out in favor of a single definien
 * @param _partDefs (optional) the partial-definiens-list
 * @param _otherwise (optional) the default definien
 */
case class PartialDef(_partDefs:Option[Partial_Definiens_List], _otherwise:Option[Otherwise]) extends Group {
  def check() = {
    if (_partDefs.isDefined && _otherwise.isEmpty) {
      throw FatalExtractError("No default case given for partial definition.")
    }
    if (_partDefs.isEmpty && _otherwise.isDefined) {
      throw FatalExtractError("Default case specified for not definition which isn't case based.")
    }
  }
}
/**
 * Contains either a single definien or a case-by-case definition
 * @param singleCasedExpr (optional) if present the single definie
 * @param partialCasedExpr (optional) if present the case-by-case definition
 */
case class CaseBasedExpr(singleCasedExpr:SingleCaseExpr, partialCasedExpr:PartialDef) extends Group {
  def check() = {
    partialCasedExpr.check()
    if(partialDef().isDefined && expr().isDefined) {
      throw FatalExtractError("Single definien given as well as case based definition. ")
    }
    if(partialDef().isEmpty && expr().isEmpty) {
      throw FatalExtractError("Missing definition in Definien. ")
    }
  }
  def expr() :Option[Expression] = {singleCasedExpr._expr}
  def partialDef()= {partialCasedExpr._partDefs}
  def otherwise() = {partialCasedExpr._otherwise}
  def isPartial() = {check(); partialDef().isDefined}
  def isSingleCase() = {check(); expr().isDefined}
}

/**
 * Contains the content of an Mizar article
 * @param articleid the name of the article
 * @param articleext the article extension (usually .miz)
 * @param _items the children items with the actual content
 */
case class Text_Proper(articleid: String, articleext: String, _items: List[Item]) {
  def prettyPrint = {
    def itemsStr(items:List[Item]):String = items match {
      case Nil => ")"
      case List(it) => "\n\t"+it.toString+")\n"
      case hd::tl => "\n\t"+hd.toString+",\n"+itemsStr(tl)
    }
    "Text_Proper(ArticleId=\""+articleid+"\", artExt=\""+articleext+"\",List(\n"+itemsStr(_items)+")"
  }
}
/**
 * A self-contained toplevel, declarationlevel or even prooflevel item containing a single subitem with mizar content
 * @param pos the source region in the corresponding Mizar source file
 * @param kind the kind of the subitem
 * @param _subitem the subitem with the content
 */
case class Item(pos: Positions, kind: String, _subitem:Subitem)