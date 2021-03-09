package info.kwarc.mmt.mizar.newxml.syntax

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
 * the ones corresponding to object-level content are defined in the ObjectLEvel file
 */

import info.kwarc.mmt.api.{GlobalName, ImplementationError, LocalName}
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.mizar._
import info.kwarc.mmt.mizar.newxml.syntax.Utils._
import info.kwarc.mmt.mizar.newxml.translator.{DeclarationLevelTranslationError, ObjectLevelTranslationError, TranslationController, TranslatorUtils}
import info.kwarc.mmt.mizar.objects.{SourceRef, SourceRegion}

case class Position(position:String) extends Group  {
  def parsePosition() : objects.SourceRef = {
    val poss = position.split('\\')
    assert(poss.length == 2 )
    val List(line, col) = poss.toList.map(_.toInt)
    SourceRef(line, col)
  }
}

case class MMLId(MMLId:String) extends Group {
  def globalName(kind: String): GlobalName = {
    val Array(aid, ln) = MMLId.split(":")
    TranslationController.getTheoryPath(aid) ? LocalName(kind+ln)
  }
}

case class OriginalNrConstrNr(constrnr:Int, originalnr:Int) extends Group
case class SerialNrIdNr(idnr: Int, serialnr:Int) extends Group
/**
 * Contains the attribute leftargscount and the child Arguments
 */
case class InfixedArgs(leftargscount:Int, _args:Arguments) extends Group
/**
 * Contains the start and end position for an item or a block in Mizar
 * @param position the start position
 * @param endposition the end position
 */
case class Positions(position:Position, endposition:String) extends Group {
  def startPosition() : objects.SourceRef = {
    position.parsePosition()
  }
  def endPosition() : objects.SourceRef = {
    Position(endposition).parsePosition()
  }
  def sourceRegion() : SourceRegion = {
    SourceRegion(startPosition(), endPosition())
  }
}
/**
 * Two children consisting of a claim with its justification as commonly given as arguments to Statements
 * the justification can be ommitted iff the claim is an Iterative-Equality (which contains its own justification)
 * the check method verifies that the justification is only omitted for Iterative-Equalities
 * @param _claim the claim
 * @param _just (optional) the justification for the claim
 */
case class ProvedClaim(_claim:Claim, _just:Option[Justification]) extends Group
/**
 * Several common attributes for Object (terms and types) definitions
 * @param formatnr
 * @param patternnr
 * @param spelling
 * @param sort
 */
case class ObjectAttrs(formatnr: Int, patternnr:Int, spelling:String, sort:String) extends Group
/**
 * A minimal list of common attributes for objects containing only spelling and sort
 * @param pos Position
 * @param sort Sort
 */
trait RedObjectSubAttrs extends Group {
  def pos : Position
  def sort: String
}
/**
 * A minimal list of common attributes (extended by the further attributes position and number) for objects containing only spelling and sort
 * @param posNr
 * @param spelling
 * @param sort
 */
case class RedObjAttr(pos:Position, nr: Int, spelling:String, sort:String) extends RedObjectSubAttrs
trait ReferencingObjAttrs extends RedObjectSubAttrs {
  def nr:Int
  def formatnr:Int
  def patternnr: Int
  def spelling: String
  def globalObjAttrs: GlobalObjAttrs
}
trait ReferencingConstrObjAttrs extends ReferencingObjAttrs {
  def formatnr:Int
  def patternnr: Int
  def constrnr: Int
  def spelling: String
  def globalDefAttrs: GlobalDefAttrs
  override def globalObjAttrs: GlobalObjAttrs = GlobalObjAttrs(globalDefAttrs.absolutepatternMMLId)
}
trait GloballyReferencingObjAttrs extends {
  def globalObjAttrs : GlobalObjAttrs
  def globalKind: Char

  protected def absoluteName(str: String) : GlobalName = {
    val Array(aid, ln) = str.split(":") match {
      case Array(a, b) if (! (a.isEmpty || b.isEmpty)) => Array(a, b)
      case other =>
        throw ImplementationError("")
    }
    TranslationController.getTheoryPath(aid) ? LocalName(globalKind.toString+ln)
  }
  def globalPatternName: GlobalName = absoluteName(globalObjAttrs.absolutepatternMMLId)
}
case class GlobalObjAttrs(absolutepatternMMLId: String) extends Group
trait GloballyReferencingDefAttrs extends GloballyReferencingObjAttrs {
  def globalDefAttrs : GlobalDefAttrs
  override def globalObjAttrs: GlobalObjAttrs = GlobalObjAttrs(globalDefAttrs.absolutepatternMMLId)

  def globalConstrName : GlobalName = absoluteName(globalDefAttrs.absoluteconstrMMLId)
  protected def absolutePatConstrName(p: GlobalName, c: GlobalName) = c.module ? LocalName(p.name.toString + c.name.toString)
  def globalPatConstrName: GlobalName = absolutePatConstrName(globalPatternName, globalConstrName)
}
case class GlobalDefAttrs(absolutepatternMMLId: String, absoluteconstrMMLId: String) extends Group
case class GlobalOrgAttrs(orgconstrnr: Option[Int], absoluteorigpatternMMLId: String, absoluteorigconstrMMLId: String) extends Group {
  def isDefined = absoluteorigconstrMMLId != "" && absoluteorigpatternMMLId != ""
}
trait GloballyReferencingReDefAttrs extends GloballyReferencingDefAttrs {
  def globalReDefAttrs : GlobalReDefAttrs
  override def globalDefAttrs : GlobalDefAttrs = globalReDefAttrs.globalDefAttrs

  def globalOrgPatternName : GlobalName = absoluteName(if (globalReDefAttrs.globalOrgAttrs.isDefined) globalReDefAttrs.globalOrgAttrs.absoluteorigpatternMMLId else globalDefAttrs.absolutepatternMMLId)
  def globalOrgConstrName: GlobalName = absoluteName(if (globalReDefAttrs.globalOrgAttrs.isDefined) globalReDefAttrs.globalOrgAttrs.absoluteorigconstrMMLId else globalDefAttrs.absoluteconstrMMLId)
  def globalOrgPatConstrName: GlobalName = absolutePatConstrName(globalOrgPatternName, globalOrgConstrName)
}
case class GlobalReDefAttrs(globalDefAttrs: GlobalDefAttrs, globalOrgAttrs: GlobalOrgAttrs) extends Group {
  def hasOrigRefs = globalOrgAttrs.isDefined
}
/**
 * An extended list of common attributes for Object (terms and types) definitions
 * @param posNr
 * @param formatNr
 * @param patNr
 * @param spelling
 * @param srt
 * @param globalObjAttrs
 */
case class ExtObjAttrs(pos: Position, nr: Int, formatnr: Int, patternnr:Int, spelling:String, sort:String, globalObjAttrs: GlobalObjAttrs) extends ReferencingObjAttrs
/**
 *
 * @param posNr
 * @param formatNr
 * @param patNr
 * @param spelling
 * @param srt
 * @param constrnr
 */
case class ConstrExtObjAttrs(pos: Position, nr: Int, formatnr: Int, patternnr:Int, spelling:String, sort:String, constrnr:Int, globalDefAttrs: GlobalDefAttrs) extends ReferencingConstrObjAttrs
/**
 *
 * @param posNr
 * @param formatNr
 * @param patNr
 * @param spelling
 * @param srt
 * @param orgnNr
 * @param constrnr
 */
case class OrgnlExtObjAttrs(pos: Position, nr: Int, formatnr: Int, patternnr:Int, spelling:String, sort:String, orgnNrConstrNr:OriginalNrConstrNr, globalDefAttrs: GlobalDefAttrs) extends ReferencingConstrObjAttrs {
  override def constrnr: Int = orgnNrConstrNr.constrnr
}
/**
 *
 * @param formatdes
 * @param formatNr
 * @param spelling
 * @param pos
 * @param globalObjAttrs
 * @param patternnr
 */

case class PatternAttrs(formatdes:String, formatnr: Int, spelling:String, pos:Position, patternnr:Int, globalObjAttrs: GlobalObjAttrs) extends Group
case class ExtPatAttr(patAttr:PatternAttrs, absoluteconstrMMLId: String, constr:String) extends Group {
  def globalDefAttrs = GlobalDefAttrs(patAttr.globalObjAttrs.absolutepatternMMLId, absoluteconstrMMLId)
}
case class OrgPatDef(orgExtPatAttr: OrgExtPatAttr, _loci:List[Locus], _locis:List[Loci]) extends PatDefs {
  override def patAttr = orgExtPatAttr.extPatAttr.patAttr
}
/**
 *
 * @param formatdes
 * @param formatNr
 * @param spelling
 * @param pos
 * @param patternnr
 * @param globalObjAttrs
 * @param _locis
 */
sealed trait PatDefs {
  def patAttr: PatternAttrs
  def _locis: List[Loci]
  def globalObjAttrs: GlobalObjAttrs = patAttr.globalObjAttrs
  def patDef : PatDef = PatDef(patAttr, _locis)
}
case class PatDef(patAttr:PatternAttrs, _locis:List[Loci]) extends PatDefs
case class ExtPatDef(extPatAttr: ExtPatAttr, _locis:List[Loci]) extends PatDefs {
  override def patAttr: PatternAttrs = extPatAttr.patAttr
}
case class OrgExtPatAttr(extPatAttr:ExtPatAttr, globalOrgAttrs: GlobalOrgAttrs) extends Group {
  def globalReDefAttrs = GlobalReDefAttrs(extPatAttr.globalDefAttrs, globalOrgAttrs)
}

/**
 *
 * @param formatdes
 * @param formatNr
 * @param spelling
 * @param pos
 * @param patternnr
 * @param constr
 * @param orgconstrnr
 * @param globalOrgConstrObjAttrs
 * @param _loci
 * @param _locis
 */
case class LocalRedVarAttr(pos:Position, serialNrIdNr: SerialNrIdNr, varnr: Int) extends Group {
  def localIdentitier: LocalName = MizarRedVarName(serialNrIdNr, varnr)
}
/**
 *
 * @param pos
 * @param orgn
 * @param serNr
 * @param varnr
 */
case class LocalVarAttr(locVarAttr:LocalRedVarAttr, spelling:String, sort:String) extends Group {
  def toIdentifier : LocalName = MizarVariableName(spelling, sort, locVarAttr.serialNrIdNr, locVarAttr.varnr)
}
/**
 *
 * @param spelling
 * @param kind
 * @param redVarAttr
 */
case class VarAttrs(locVarAttr:LocalRedVarAttr, spelling:String, kind:String) extends Group {
  /**
   * compute the string value of corresponding omv
   */
  def toIdentifier: LocalName = MizarVariableName(spelling, kind, locVarAttr.serialNrIdNr, locVarAttr.varnr)
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
 * @param pos the position in the source file at which the article content starts (usually after importing some content from other files)
 * @param _items the children items with the actual content
 */
case class Text_Proper(articleid: String, articleext: String, pos: Position, _items: List[Item]) {
  def prettyPrint = {
    def itemsStr(items:List[Item]):String = items match {
      case Nil => ")"
      case List(it) => "\n\t"+it.toString+")\n"
      case hd::tl => "\n\t"+hd.toString+",\n"+itemsStr(tl)
    }
    "Text_Proper(ArticleId=\""+articleid+"\", artExt=\""+articleext+"\", position=\""+pos+"\",List(\n"+itemsStr(_items)+")"
  }
}
case class Item(kind: String, pos:Positions, _subitem:Subitem) {
  def checkKind() = {
    assert(_subitem.kind == Utils.fullClassName(kind))
  }
}

object Utils {
  def fullClassName(s: String) = {
    "info.kwarc.mmt.mizar.newxml.syntax."+s.replace("-", "_")
  }

  def MizarRedVarName(serialNrIdNr: SerialNrIdNr): LocalName = LocalName("idNr_"+serialNrIdNr.idnr.toString)
  def MizarRedVarName(serialNrIdNr: SerialNrIdNr, varnr: Int): LocalName = {
    MizarRedVarName(serialNrIdNr) //"serialNr:"+serialNrIdNr.serialnr.toString+",varNr:"+varnr.toString
  }
  def MizarVariableName(spelling: String, kind: String, serialNrIdNr: SerialNrIdNr): LocalName = {
    LocalName(spelling) / LocalName(kind) / MizarRedVarName(serialNrIdNr)
  }
  def MizarVariableName(spelling: String, kind: String, serialNrIdNr: SerialNrIdNr, varnr: Int): LocalName = {
    LocalName(spelling) / LocalName(kind) / MizarRedVarName(serialNrIdNr, varnr)
  }

  /**
   * Internal representation of Properties class
   * @param _just (optional) the proof of the property
   */
  sealed abstract class MizarProperty(_just:Option[Justification])
  //for functors
  // for binary operators
  case class Commutativity(_just:Option[Justification]) extends MizarProperty(_just:Option[Justification])
  //for binary operators
  case class Idempotence(_just:Option[Justification]) extends MizarProperty(_just:Option[Justification])
  // for unary operators
  case class Involutiveness(_just:Option[Justification]) extends MizarProperty(_just:Option[Justification])
  // being a projection operators, for unary operators
  case class Projectivity(_just:Option[Justification]) extends MizarProperty(_just:Option[Justification])

  //for predicates
  case class Reflexivity(_just:Option[Justification]) extends MizarProperty(_just:Option[Justification])
  case class Irreflexivity(_just:Option[Justification]) extends MizarProperty(_just:Option[Justification])
  case class Symmetry(_just:Option[Justification]) extends MizarProperty(_just:Option[Justification])
  case class Assymmetry(_just:Option[Justification]) extends MizarProperty(_just:Option[Justification])
  case class Connectiveness(_just:Option[Justification]) extends MizarProperty(_just:Option[Justification])

  //for modes and existential_registrations
  //only those modes (and subtypes, expanded into) can be used as types in fraenkel_terms
  case class Sethood(_just:Option[Justification]) extends MizarProperty(_just:Option[Justification])
  def matchProperty(prop: String, _just:Option[Justification]) = prop match {
    case "commutativity" => Commutativity(_just)
    case "idempotence" => Idempotence(_just)
    case "involutiveness" => Involutiveness(_just)
    case "projectivity" => Projectivity(_just)
    case "reflexivity" => Reflexivity(_just)
    case "irreflexivity" => Irreflexivity(_just)
    case "symmetry" => Symmetry(_just)
    case "assymmetry" => Assymmetry(_just)
    case "connectiveness" => Connectiveness(_just)
    case "sethood" => Sethood(_just)
  }
  sealed abstract class DeclarationKinds extends PatternKinds
  case class ModeKind() extends DeclarationKinds
  case class StructureKind() extends DeclarationKinds
  case class FunctorKind() extends DeclarationKinds
  case class AttributeKind() extends DeclarationKinds
  case class PredicateKind() extends DeclarationKinds
  sealed abstract class PatternKinds
  case class StrictKind() extends PatternKinds
  case class AggregateKind() extends PatternKinds
  case class SelectorKind() extends PatternKinds
  case class ForgetfulKind() extends PatternKinds
  def shortKind(kind: PatternKinds): Char = kind match {
    case ModeKind() => 'M'
    case StructureKind() => 'L'
    case FunctorKind() => 'K'
    case AttributeKind() => 'V'
    case PredicateKind() => 'R'
    case StrictKind() => 'V'
    case AggregateKind() => 'G'
    case SelectorKind() => 'J'
    case ForgetfulKind() => 'U'
  }
}