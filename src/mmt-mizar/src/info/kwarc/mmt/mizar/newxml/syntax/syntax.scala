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
 * The file is roughly structures as follows:
 * First we define some classes to represent attributes or children we want to group up, or for which we want to implement
 * additional methods usually for further parsing or checking
 *
 * Traits are used to group up classes representing XML tags defining similar objects, for instance terms Patterns, ...
 *
 * Afterwards the case classes corresponding to the XML tags are defined, roughly from Top-Level downwards and grouped by
 * the traits they extend (if any)
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
  def mizarSemiGlobalName():MizarSemiGlobalName = {
    val gns = MMLId.split(':')
    assert(gns.length == 2 )
    val List(aidStr, nrStr) = gns.toList
    MizarSemiGlobalName(aidStr, nrStr.toInt)
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
  def pos() : Position
  def sort() : String
}
/**
 * A minimal list of common attributes (extended by the further attributes position and number) for objects containing only spelling and sort
 * @param posNr
 * @param spelling
 * @param sort
 */
case class RedObjAttr(pos:Position, nr: Int, spelling:String, sort:String) extends RedObjectSubAttrs
trait referencingObjAttrs extends RedObjectSubAttrs {
  def nr:Int
  def formatnr:Int
  def patternnr: Int
  def spelling: String
  def globalPatternName(aid: String, refSort: String, constrnr: Int): MizarGlobalName = {
    MizarGlobalName(aid, refSort, patternnr)
  }
}
trait referencingConstrObjAttrs extends referencingObjAttrs {
  def formatnr:Int
  def patternnr: Int
  def constrnr: Int
  def spelling: String
}
trait globallyReferencingObjAttrs {
  def globalObjAttrs : GlobalObjAttrs
  def globalKind = globalObjAttrs.globalKind
  def globalPatternFile = globalObjAttrs.globalPatternFile
  def globalPatternNr = globalObjAttrs.globalPatternNr

  def globalPatternName() : MizarGlobalName = MizarGlobalName(globalPatternFile, globalKind, globalPatternNr)
}
case class GlobalObjAttrs(globalKind: String, globalPatternFile: String, globalPatternNr:Int) extends Group
trait globallyReferencingDefAttrs extends globallyReferencingObjAttrs {
  def globalDefAttrs : GlobalDefAttrs
  override def globalObjAttrs: GlobalObjAttrs = GlobalObjAttrs(globalDefAttrs.globalKind, globalDefAttrs.globalPatternFile, globalDefAttrs.globalPatternNr)
  def globalConstrFile = globalDefAttrs.globalConstrFile
  def globalConstrNr = globalDefAttrs.globalConstrNr

  def globalConstrName() : MizarGlobalName = MizarGlobalName(globalConstrFile, globalKind, globalConstrNr)
}
case class GlobalDefAttrs(globalKind: String, globalPatternFile: String, globalPatternNr:Int, globalConstrFile: String, globalConstrNr: Int) extends Group
trait globallyReferencingReDefAttrs extends globallyReferencingDefAttrs {
  def globalReDefAttrs : GlobalReDefAttrs
  override def globalDefAttrs : GlobalDefAttrs = globalReDefAttrs.globalDefAttrs
  def globalOrgPatternFile = globalReDefAttrs.globalOrgPatternFile
  def globalOrgPatternNr = globalReDefAttrs.globalOrgPatternNr
  def globalOrgConstrFile = globalReDefAttrs.globalOrgConstrFile
  def globalOrgConstrNr = globalReDefAttrs.globalOrgConstrNr

  def globalOrgPatternName() : MizarGlobalName = MizarGlobalName(globalOrgConstrFile, globalKind, globalOrgPatternNr)
  def globalOrgConstrName() : MizarGlobalName = MizarGlobalName(globalOrgConstrFile, globalKind, globalOrgConstrNr)
}
case class GlobalReDefAttrs(globalDefAttrs: GlobalDefAttrs, globalOrgPatternFile: String, globalOrgPatternNr:Int, globalOrgConstrFile: String, globalOrgConstrNr: Int) extends Group
/**
 * An extended list of common attributes for Object (terms and types) definitions
 * @param posNr
 * @param formatNr
 * @param patNr
 * @param spelling
 * @param srt
 * @param globalObjAttrs
 */
case class ExtObjAttrs(pos: Position, nr: Int, formatnr: Int, patternnr:Int, spelling:String, sort:String, globalObjAttrs: GlobalObjAttrs) extends globallyReferencingObjAttrs with referencingObjAttrs
/**
 *
 * @param posNr
 * @param formatNr
 * @param patNr
 * @param spelling
 * @param srt
 * @param constrnr
 */
case class ConstrExtObjAttrs(pos: Position, nr: Int, formatnr: Int, patternnr:Int, spelling:String, sort:String, constrnr:Int, globalDefAttrs: GlobalDefAttrs) extends globallyReferencingDefAttrs with referencingConstrObjAttrs
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
case class OrgnlExtObjAttrs(pos: Position, nr: Int, formatnr: Int, patternnr:Int, spelling:String, sort:String, orgnNrConstrNr:OriginalNrConstrNr, globalReDefAttrs: GlobalReDefAttrs) extends globallyReferencingReDefAttrs with referencingConstrObjAttrs {
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
case class ExtPatAttr(patAttr:PatternAttrs, globalConstrFile: String, globalConstrNr: Int, constr:String) extends Group {
  def globalDefAttrs = GlobalDefAttrs(patAttr.globalObjAttrs.globalKind, patAttr.globalObjAttrs.globalPatternFile, patAttr.globalObjAttrs.globalPatternNr,
    globalConstrFile, globalConstrNr)
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
sealed trait PatDefs extends globallyReferencingObjAttrs {
  def patAttr: PatternAttrs
  def _locis: List[Loci]
  def globalObjAttrs: GlobalObjAttrs = patAttr.globalObjAttrs
  def patDef : PatDef = PatDef(patAttr, _locis)
}
case class PatDef(patAttr:PatternAttrs, _locis:List[Loci]) extends PatDefs
case class ExtPatDef(extPatAttr: ExtPatAttr, _locis:List[Loci]) extends PatDefs {
  override def patAttr: PatternAttrs = extPatAttr.patAttr
}
case class OrgExtPatAttr(extPatAttr:ExtPatAttr, orgconstrnr:Int, globalOrgPatternFile: String, globalOrgPatternNr:Int, globalOrgConstrFile: String, globalOrgConstrNr: Int) extends Group {
  def globalReDefAttrs = GlobalReDefAttrs(extPatAttr.globalDefAttrs, globalOrgPatternFile, globalOrgPatternNr, globalOrgConstrFile, globalOrgConstrNr)
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
case class LocalRedVarAttr(pos:Position, origin:String, serialNrIdNr: SerialNrIdNr, varnr: Int) extends Group {
  def localIdentitier(localId: Boolean = false) : String = MizarRedVarName(serialNrIdNr, varnr, localId)
}
/**
 *
 * @param pos
 * @param orgn
 * @param serNr
 * @param varnr
 */
case class LocalVarAttr(locVarAttr:LocalRedVarAttr, spelling:String, sort:String) extends Group {
  def toIdentifier(localId : Boolean = false) : String = MizarVariableName(spelling, sort, locVarAttr.serialNrIdNr, locVarAttr.varnr, localId)
}
/**
 *
 * @param spelling
 * @param kind
 * @param redVarAttr
 */
case class VarAttrs(locVarAttr:LocalRedVarAttr, spelling:String, kind:String) extends Group {
  /**
   * @param localId if set use the idNr as identifier instead of serialNr and varNr
   * @return
   */
  def toIdentifier(localId : Boolean = false) : String = MizarVariableName(spelling, kind, locVarAttr.serialNrIdNr, locVarAttr.varnr, localId)
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
  case class MizarSemiGlobalName(aid:String, nr:Int) {
    def makeGlobalName(kind:String) : MizarGlobalName = {MizarGlobalName(aid, kind, nr)}
  }
  case class MizarGlobalName(aid:String, kind: String, nr:Int)

  def MizarRedVarName(serialNrIdNr: SerialNrIdNr): String = "idNr: "+serialNrIdNr.idnr.toString
  def MizarRedVarName(serialNrIdNr: SerialNrIdNr, varnr: Int, localId: Boolean = false): String = {
    if (localId) {MizarRedVarName(serialNrIdNr)} else
      "serialNr:"+serialNrIdNr.serialnr.toString+",varNr:"+varnr.toString
  }
  def MizarVariableName(spelling: String, kind: String, serialNrIdNr: SerialNrIdNr): String = {
    spelling + "/" +kind+"/"+ MizarRedVarName(serialNrIdNr)
  }
  def MizarVariableName(spelling: String, kind: String, serialNrIdNr: SerialNrIdNr, varnr: Int, localId: Boolean = false): String = {
    spelling + "/" +kind+"/"+ MizarRedVarName(serialNrIdNr, varnr, localId)
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
}