package info.kwarc.mmt.mizar.newxml.syntax

/**
 * The following classes are used by an XML parser utility part of the MMT API to parse Mizar content already exported to esx files
 * scala case classes
 *
 * Any case class (which doesn't extend Group) represents an XML tag of the same name (but with - replaces by _)
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

import info.kwarc.mmt.api.utils._
import scala.xml.Attribute

case class Position(position:String) extends Group  {
  def parsePosition() : (Int, Int) = {
    val poss = position.split('\\')
    assert(poss.length == 2 )
    val List(line, col) = poss.toList.map(_.toInt)
    (line, col)
  }
}
case class FormatNr(formatnr:Int) extends Group
case class PatternNr(patternnr:Int) extends Group
case class Spelling(spelling:String) extends Group
case class Sort(sort:String) extends Group
case class MMLId(MMLId:String) extends Group
case class IdNr(idnr:Int) extends Group
case class Nr(nr:Int) extends Group
case class VarNr(varnr:Int) extends Group
case class ConstrNr(constrnr:Int) extends Group
case class OriginalNr(constrNr:ConstrNr, originalnr:Int) extends Group
case class Origin(origin:String) extends Group
case class SerialNr(idnr: IdNr, serialnr:Int) extends Group

case class InfixedArgs(leftargscount:Int, _args:Arguments) extends Group
/**
 * Contains the start and end position for an item or a block in Mizar
 * @param position the start position
 * @param endposition the end position
 */
case class Positions(position:Position, endposition:String) extends Group {
  def startPosition() : (Int, Int) = {
    position.parsePosition()
  }
  def endPosition() : (Int, Int) = {
    Position(endposition).parsePosition()
  }
}
/**
 * Two children consisting of a claim with its justification as commonly given as arguments to Statements
 * the justification can be ommitted iff the claim is an Iterative-Equality (which contains its own justification)
 * the check method verifies that the justification is only omitted for Iterative-Equalities
 * @param _claim the claim
 * @param _just (optional) the justification for the claim
 */
case class ProvedClaim(_claim:Claim, _just:Option[Justification]) extends Group {
  /**
   * An empty justification is only allowed if the claim is an iterative equality
   */
  def check(): Unit = {
    if (_just.isEmpty) {
      assert(_claim.getClass.getName == checkUtils.fullClassName("Iterative-Equality"))
    }
  }
}
/**
 * Several common attributes for Object (terms and types) definitions
 * @param formatNr
 * @param patNr
 * @param spell
 * @param srt
 */
case class ObjectAttrs(formatNr: FormatNr, patNr:PatternNr, spell:Spelling, srt:Sort) extends Group
case class RedObjectSubAttrs(spell:Spelling, srt:Sort) extends Group
case class PosNr(pos:Position, nr:Nr) extends Group
case class RedObjAttr(posNr:PosNr, objAt:RedObjectSubAttrs) extends Group
case class ExtObjAttrs(posNr:PosNr, objAt:ObjectAttrs) extends Group
case class ConstrExtObjAttrs(extAttrs:ExtObjAttrs, constrNr:ConstrNr) extends Group
case class OrgnlExtObjAttrs(extAttrs:ExtObjAttrs, orgnNr:OriginalNr) extends Group
case class ConstrOrgnlExtObjAttrs(extAttrs:ExtObjAttrs, constrNr:ConstrNr, orgnNr:OriginalNr) extends Group
case class PatternAttrs(formatdes:String, formatNr:FormatNr, spell:Spelling, pos:Position, patternnr:PatternNr) extends Group
case class ExtPatAttrs(patAttrs:PatternAttrs, constr:String) extends Group
case class OrgPatAttrs(extPatAttrs:ExtPatAttrs, orgconstrnr:Int) extends Group
case class PatDef(patAttr:PatternAttrs, _loci:List[Loci]) extends Group
case class ExtPatDef(extPatAttr:ExtPatAttrs, _loci:List[Loci]) extends Group
case class OrgPatDef(orgPatAttr:OrgPatAttrs, _loci:List[Locus], _locis:List[Loci]) extends Group
case class RedVarAttrs(pos:Position, orgn:Origin, serNr:SerialNr, varnr:VarNr) extends Group
case class VarAttrs(spell:Spelling, kind:String, redVarAttr:RedVarAttrs) extends Group

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
case class CaseBasedExpr(singleCasedExpr:SingleCaseExpr, partialCasedExpr:PartialDef) extends Group{
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
  def isPartial() = {check(); partialDef.isDefined}
  def isSingleCase() = {check(); expr.isDefined}
}

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
    assert(_subitem.kind == checkUtils.fullClassName(kind))
  }
}

sealed trait Subitem {
  def kind:String = {
    this.getClass.getName
  }
}
case class Reservation(_reservationSegments: List[Reservation_Segment]) extends Subitem
case class Definition_Item(_block:Block) extends Subitem
case class Section_Pragma() extends Subitem
case class Pragma(_notionName: Option[Pragmas]) extends Subitem
case class Loci_Declaration(_qualSegms:Qualified_Segments, _conds:Option[Conditions]) extends Subitem
case class Cluster(_registrs:List[Registrations]) extends Subitem
case class Correctness(_correctnessCond:Correctness_Conditions, _just:Justification) extends Subitem
case class Correctness_Condition(_cond:CorrectnessConditions, _just:Option[Justification]) extends Subitem
case class Exemplification(_exams:List[Exemplifications]) extends Subitem
case class Assumption(_ass:Assumptions) extends Claim with Subitem
case class Identify(_pats:List[Patterns], _lociEqns:Loci_Equalities) extends Subitem
case class Generalization(_qual:Qualified_Segments, _conds:Option[Claim]) extends Subitem // let
case class Reduction(_tm:Term, _tm2:Term) extends Subitem
case class Scheme_Block_Item(MmlId: MMLId, _blocks:List[Block]) extends Subitem
case class Property(_props:Properties, _just:Option[Justification]) extends Subitem
case class Per_Cases(_just:Justification) extends Subitem
case class Case_Block(_block:Block) extends Subitem

sealed trait Heads extends Subitem
case class Scheme_Head(_sch:Scheme, _vars:Schematic_Variables, _form:Formula, _provForm:Option[Provisional_Formulas]) extends Heads
case class Suppose_Head(_ass:Assumptions) extends Heads
case class Case_Head(_ass:Assumptions) extends Heads

sealed trait Nyms extends Subitem
case class Pred_Antonym(_predPats:List[Predicate_Pattern]) extends Nyms
case class Pred_Synonym(_predPats:List[Predicate_Pattern]) extends Nyms
case class Attr_Synonym(_predPats:List[Attribute_Pattern]) extends Nyms
case class Attr_Antonym(_predPats:List[Attribute_Pattern]) extends Nyms
case class Func_Synonym(_predPats:List[FunctorPatterns]) extends Nyms
case class Func_Antonym(_predPats:List[FunctorPatterns]) extends Nyms
case class Mode_Synonym(_predPats:List[Mode_Pattern]) extends Nyms

sealed trait Statement extends Subitem
case class Conclusion(prfClaim:ProvedClaim) extends Statement
case class Type_Changing_Statement(_eqList:Equalities_List, _tp:Type, _just:Justification) extends Statement
case class Regular_Statement(prfClaim:ProvedClaim) extends Statement
case class Theorem_Item(MmlId:MMLId, prfClaim:ProvedClaim) extends Statement
case class Choice_Statement(_qual:Qualified_Segments, prfClaim:ProvedClaim) extends Statement

sealed trait Definition extends Subitem
case class Structure_Definition(_ancestors:Ancestors, _strPat:Structure_Pattern, _fieldSegms:List[Field_Segments], _rendering:Structure_Patterns_Rendering) extends Definition
case class Attribute_Definition(MmlId:MMLId, _redef:Redefine, _attrPat:Attribute_Pattern, _def:Option[Definiens]) extends Definition
case class Constant_Definition(_children:List[Equating]) extends Definition
case class Functor_Definition(MmlId:MMLId, _redefine:Redefine, _pat:Patterns, _tpSpec:Option[Type_Specification], _def:Option[Definiens]) extends Definition
case class Mode_Definition(_redef:Redefine, _pat:Mode_Pattern, _expMode:Modes) extends Definition
case class Private_Functor_Definition(_var:Variable, _tpList:Type_List, _tm:Term) extends Definition
case class Private_Predicate_Definition(_var:Variable, _tpList:Type_List, _form:Formula) extends Definition
case class Predicate_Definition(MmlId:MMLId, _redefine:Redefine, _predPat:Predicate_Pattern, _def:Option[Definiens]) extends Definition

sealed trait VariableSegments
case class Free_Variable_Segment(pos:Position, _var:Variable, _tp:Type) extends VariableSegments
case class Implicitly_Qualified_Segment(pos:Position, _var:Variable, _rescDesc:ReservedDscr_Type) extends VariableSegments
case class Explicitly_Qualified_Segment(pos:Position, _vars:Variables, _tp:Type) extends VariableSegments
case class Qualified_Segments(_children:List[VariableSegments])

sealed trait Type extends Expression
case class ReservedDscr_Type(idnr: IdNr, nr: Nr, srt: Sort, _subs:Substitutions, _tp:Type) extends Type
case class Clustered_Type(srt:Sort, pos: Position, _adjClust:Adjective_Cluster, _tp:Type) extends Type
case class Standard_Type(tpAttrs:ExtObjAttrs, noocc: Option[Boolean], origNr: OriginalNr, _args:List[Arguments]) extends Type
case class Struct_Type(tpAttrs:ConstrExtObjAttrs, _args:Arguments) extends Type

sealed trait Expression
sealed trait Term extends Expression
case class Simple_Term(varAttr:RedVarAttrs, srt:Sort) extends Term
case class Aggregate_Term(tpAttrs:ConstrExtObjAttrs, _args:Arguments) extends Term
case class Selector_Term(tpAttrs:ConstrExtObjAttrs, _args:List[Term]) extends Term
case class Circumfix_Term(tpAttrs:OrgnlExtObjAttrs, _symbol:Right_Circumflex_Symbol, _args:Arguments) extends Term
case class Numeral_Term(posNr:PosNr, srt:Sort, varnr:VarNr) extends Term
case class it_Term(pos:Position, srt:Sort) extends Term
case class Internal_Selector_Term(redObjAttr: RedObjAttr, varnr:VarNr) extends Term
case class Infix_Term(tpAttrs:OrgnlExtObjAttrs, infixedArgs: InfixedArgs) extends Term
case class Global_Choice_Term(srt:Sort, pos:Position, _tp:Type) extends Term
case class Placeholder_Term(redObjAttr: RedObjAttr, varnr:Int) extends Term
case class Private_Functor_Term(redObjAttr: RedObjAttr, serialnr:SerialNr, _args:Arguments) extends Term
case class Fraenkel_Term(pos:Position, srt:Sort, _varSegms:Variable_Segments, _tm:Term, _form:Formula) extends Term
case class Simple_Fraenkel_Term(pos:Position, srt:Sort, _varSegms:Variable_Segments, _tm:Term) extends Term
case class Qualification_Term(pos:Position, srt:Sort, _tm:Term, _tp:Type) extends Term
case class Forgetful_Functor_Term(constrExtObjAttrs: ConstrExtObjAttrs, _tm:Term) extends Term

sealed trait Patterns
case class Structure_Pattern(extPatDef: ExtPatDef)
case class Attribute_Pattern(orgPatDef: OrgPatDef) extends Patterns
case class Mode_Pattern(patDef:PatDef) extends Patterns
case class Predicate_Pattern(orgPatDef:OrgPatDef) extends Patterns
case class Strict_Pattern(orgPatDef: OrgPatDef) extends Patterns
sealed trait FunctorPatterns extends Patterns
case class AggregateFunctor_Pattern(extPatDef: ExtPatDef) extends FunctorPatterns
case class ForgetfulFunctor_Pattern(extPatDef: ExtPatDef) extends FunctorPatterns
case class InfixFunctor_Pattern(orgPatDef: OrgPatDef) extends FunctorPatterns
case class CircumfixFunctor_Pattern(orgPat: OrgPatAttrs, _right_Circumflex_Symbol: Right_Circumflex_Symbol, _loci:List[Locus], _locis:List[Loci]) extends FunctorPatterns
case class SelectorFunctor_Pattern(extPatDef:ExtPatDef) extends FunctorPatterns

sealed trait Registrations
case class Functorial_Registration(pos:Position, _aggrTerm:Term, _adjCl:Adjective_Cluster, _tp:Option[Type]) extends Registrations
case class Existential_Registration(pos:Position, _adjClust:Adjective_Cluster, _tp:Type) extends Registrations
case class Conditional_Registration(pos:Position, _adjClusts:List[Adjective_Cluster], _tp:Type) extends Registrations
case class Property_Registration(_props:Properties, _blck:Block) extends Registrations with Subitem

sealed trait CorrectnessConditions
case class coherence() extends CorrectnessConditions
case class existence() extends CorrectnessConditions
case class uniqueness() extends CorrectnessConditions
case class reducibility() extends CorrectnessConditions
case class compatibility() extends CorrectnessConditions
case class consistency() extends CorrectnessConditions

sealed trait Justification
case class Straightforward_Justification(pos:Position, _refs:List[Reference]) extends Justification
case class Block(kind: String, pos:Positions, _items:List[Item]) extends Justification
case class Scheme_Justification(posNr:PosNr, idnr:IdNr, schnr:Int, spell:Spelling, _refs:List[Reference]) extends Justification

sealed trait Claim
case class Proposition(pos:Position, _label:Label, _thesis:Claim) extends Claim
case class Thesis(pos:Position, srt:Sort) extends Claim
case class Diffuse_Statement(spell:Spelling, serialnr:SerialNr, labelnr:Int, _label:Label) extends Claim
case class Conditions(_props:List[Proposition]) extends Claim
case class Iterative_Equality(_label:Label, _formula:Formula, _just:Justification, _iterSteps:List[Iterative_Step]) extends Claim

sealed trait Formula extends Claim with Expression
case class Existential_Quantifier_Formula(srt:Sort, pos:Position, _vars:Variable_Segments, _expression:Claim) extends Formula
case class Relation_Formula(objectAttrs: OrgnlExtObjAttrs, infixedArgs: InfixedArgs) extends Formula
case class Universal_Quantifier_Formula(srt:Sort, pos:Position, _vars:Variable_Segments, _restrict:Option[Restriction], _expression:Claim) extends Formula
case class Multi_Attributive_Formula(srt:Sort, pos:Position, _tm:Term, _clusters:List[Adjective_Cluster]) extends Formula
case class Conditional_Formula(srt:Sort, pos:Position, _formulae:List[Claim]) extends Formula
case class Conjunctive_Formula(srt:Sort, pos:Position, _formulae:List[Claim]) extends Formula
case class Biconditional_Formula(srt:Sort, pos:Position, _frstFormula:Claim, _sndFormula:Claim) extends Formula
case class Disjunctive_Formula(srt:Sort, pos:Position, _formulae:List[Claim]) extends Formula
case class Negated_Formula(srt:Sort, pos:Position, _formula:Claim) extends Formula
case class Contradiction(srt:Sort, pos:Position) extends Formula
case class Qualifying_Formula(srt:Sort, pos:Position, _tm:Term, _tp:Type) extends Formula
case class Private_Predicate_Formula(redObjAttr:RedObjAttr, serialNr: SerialNr, constrNr: ConstrNr, _args:Arguments) extends Formula
case class FlexaryDisjunctive_Formula(srt:Sort, pos:Position, _formulae:List[Claim]) extends Formula
case class FlexaryConjunctive_Formula(srt:Sort, pos:Position, _formulae:List[Claim]) extends Formula
case class Multi_Relation_Formula(srt:Sort, pos:Position, _relForm:Relation_Formula, _rhsOfRFs:List[RightSideOf_Relation_Formula]) extends Formula

case class RightSideOf_Relation_Formula(objAttr:ConstrOrgnlExtObjAttrs, infixedArgs: InfixedArgs)

sealed trait Assumptions
case class Single_Assumption(pos:Position, _prop:Proposition) extends Assumptions
case class Collective_Assumption(pos:Position, _cond:Conditions) extends Assumptions
case class Existential_Assumption(_qualSegm:Qualified_Segments, _cond:Conditions) extends Assumptions with Subitem


sealed trait Exemplifications
case class ImplicitExemplification(_term:Term) extends Exemplifications
case class ExemplifyingVariable(_var:Variable, _simplTm:Simple_Term) extends Exemplifications
case class Example(_var:Variable, _tm:Term) extends Exemplifications

sealed trait Reference
case class Local_Reference(pos:Position, spell:Spelling, serialnumber:SerialNr, labelnr:Int) extends Reference
case class Definition_Reference(posNr:PosNr, spell:Spelling, number:Int) extends Reference
case class Link(pos:Position, labelnr:Int) extends Reference
case class Theorem_Reference(posNr:PosNr, spell:Spelling, number:Int) extends Reference

sealed trait Modes
case class Expandable_Mode(_tp:Type) extends Modes
case class Standard_Mode(_tpSpec:Option[Type_Specification], _def:Option[Definiens]) extends Modes

sealed trait Segments
case class Functor_Segment(pos:Position, _vars:Variables, _tpList:Type_List, _tpSpec:Option[Type_Specification]) extends Segments
case class Predicate_Segment(pos:Position, _vars:Variables, _tpList:Type_List) extends Segments

sealed trait EqualityTr
case class Equality(_var:Variable, _tm:Term) extends EqualityTr
case class Equality_To_Itself(_var:Variable, _tm:Term) extends EqualityTr

sealed trait Pragmas
case class Unknown(pos:Position, inscription:String) extends Pragmas
case class Notion_Name(pos:Position, inscription:String) extends Pragmas
case class Canceled(MmlId:MMLId, amount:Int, kind:String, position:Position) extends Pragmas

case class Scheme(idNr: IdNr, spell:Spelling, nr:Nr)
case class Schematic_Variables(_segms:List[Segments])
case class Reservation_Segment(pos: Position, _vars:Variables, _varSegm:Variable_Segments, _tp:Type)
case class Variables(_vars:List[Variable])
case class Variable_Segments(_vars:List[VariableSegments])
case class Variable(varAttr:VarAttrs)
case class Substitutions(_childs:List[Substitution])
case class Substitution(freevarnr:Int, kind:String, varnr:VarNr)
case class Adjective_Cluster(_attrs: List[Attribute])
case class Attribute(orgnlExtObjAttrs: OrgnlExtObjAttrs, noocc: Option[Boolean], _args:List[Arguments])
case class Arguments(_children:List[Term])
case class Ancestors(_structTypes:List[Struct_Type])
case class Loci(_loci:List[Locus])
case class Locus(varkind:String, varAttr:VarAttrs)
case class Field_Segments(_fieldSegments:List[Field_Segment])
case class Field_Segment(pos:Position, _selectors:Selectors, _tp:Type)
case class Selectors(posNr:PosNr, spell:Spelling, _loci:List[Selector])
case class Selector(posNr:PosNr, spell:Spelling, _loci:List[Locus])
case class Structure_Patterns_Rendering(_aggrFuncPat:AggregateFunctor_Pattern, _forgetfulFuncPat:ForgetfulFunctor_Pattern, _strFuncPat:Strict_Pattern, _selList:Selectors_List)
case class Selectors_List(_children:List[Patterns])
case class Correctness_Conditions(_cond:List[CorrectnessConditions])
case class Properties(property:Option[String], _cond:List[Properties], _tp:Option[Type])
case class Redefine(occurs:Boolean)
case class Type_Specification(_types:List[Type])
case class Definiens(pos:Position, kind:String, shape:String, _label:Label, _expr:CaseBasedExpr)
case class Label(spell:Spelling, pos:Position, serialnr:SerialNr, labelnr:Int)
case class Restriction(_formula:Formula)
case class Right_Circumflex_Symbol(posNr:PosNr, formatnr:FormatNr, spell:Spelling)
case class Equating(_var:Variable, _tm:Term)
case class Loci_Equalities(_lociEqns:List[Loci_Equality])
case class Loci_Equality(pos:Position, _loci:List[Locus])
case class Equalities_List(_eqns:List[EqualityTr])
case class Iterative_Step(pos:Position, _tm:Term, _just:Justification)
case class Type_List(_tps:List[Type])
case class Provisional_Formulas(_props:List[Proposition])
case class Partial_Definiens_List(_partDef:List[Partial_Definiens])
case class Partial_Definiens(_expr:Expression, _form:Formula)
case class Otherwise(_expr:Option[Expression])

object checkUtils {
  def fullClassName(s: String) = {
    "info.kwarc.mmt.mizar.newxml.syntax."+s.replace("-", "_")
  }
}