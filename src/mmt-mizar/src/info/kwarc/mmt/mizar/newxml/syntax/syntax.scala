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

import info.kwarc.mmt.api.utils._

import info.kwarc.mmt.mizar._
import info.kwarc.mmt.mizar.newxml.syntax.Utils._
import info.kwarc.mmt.mizar.objects.{SourceRef, SourceRegion}

case class Position(position:String) extends Group  {
  def parsePosition() : objects.SourceRef = {
    val poss = position.split('\\')
    assert(poss.length == 2 )
    val List(line, col) = poss.toList.map(_.toInt)
    SourceRef(line, col)
  }
}
case class FormatNr(formatnr:Int) extends Group
case class PatternNr(patternnr:Int) extends Group
case class Spelling(spelling:String) extends Group
case class Sort(sort:String) extends Group
case class MMLId(MMLId:String) extends Group {
  def mizarSemiGlobalName():MizarSemiGlobalName = {
    val gns = MMLId.split(':')
    assert(gns.length == 2 )
    val List(aidStr, nrStr) = gns.toList
    MizarSemiGlobalName(aidStr, nrStr.toInt)
  }
}
case class IdNr(idnr:Int) extends Group
case class Nr(nr:Int) extends Group
case class VarNr(varnr:Int) extends Group
case class ConstrNr(constrnr:Int) extends Group
case class OriginalNrConstrNr(constrNr:ConstrNr, originalnr:Int) extends Group
case class Origin(origin:String) extends Group
case class SerialNrIdNr(idnr: IdNr, serialnr:Int) extends Group
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
case class ProvedClaim(_claim:Claim, _just:Option[Justification]) extends Group {
  /**
   * An empty justification is only allowed if the claim is an iterative equality
   */
  def check(): Unit = {
    if (_just.isEmpty) {
      assert(_claim.getClass.getName == Utils.fullClassName("Iterative-Equality"))
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
/**
 * A minimal list of common attributes for objects containing only spelling and sort
 * @param pos Position
 * @param sort Sort
 */
trait RedObjectSubAttrs extends Group {
  def pos() : Position
  def sort() : Sort
}
case class RedObjSubAttrs(pos: Position, sort: Sort) extends RedObjectSubAttrs
case class PosNr(pos:Position, nr:Nr) extends Group
/**
 * A minimal list of common attributes (extended by the further attributes position and number) for objects containing only spelling and sort
 * @param posNr
 * @param spell
 * @param sort
 */
case class RedObjAttr(posNr:PosNr, spell:Spelling, sort:Sort) extends RedObjectSubAttrs {
  override def pos(): Position = posNr.pos
}
trait referencingObjAttrs extends RedObjectSubAttrs {
  def posNr:PosNr
  def formatNr:FormatNr
  def patternNr: PatternNr
  def spell: Spelling
  override def pos(): Position = posNr.pos
  def globalName(): MizarGlobalName = {
    // requires global id to be added to the esx files by Artur
    ???
  }
}
/**
 * An extended list of common attributes for Object (terms and types) definitions
 * @param posNr
 * @param formatNr
 * @param patNr
 * @param spell
 * @param srt
 */
case class ExtObjAttrs(posNr:PosNr, formatNr: FormatNr, patternNr:PatternNr, spell:Spelling, sort:Sort) extends referencingObjAttrs
/**
 *
 * @param posNr
 * @param formatNr
 * @param patNr
 * @param spell
 * @param srt
 * @param constrNr
 */
case class ConstrExtObjAttrs(posNr:PosNr, formatNr: FormatNr, patternNr:PatternNr, spell:Spelling, sort:Sort, constrNr:ConstrNr) extends referencingObjAttrs
/**
 *
 * @param posNr
 * @param formatNr
 * @param patNr
 * @param spell
 * @param srt
 * @param orgnNr
 * @param constrNr
 */
case class OrgnlExtObjAttrs(posNr:PosNr, formatNr: FormatNr, patternNr:PatternNr, spell:Spelling, sort:Sort, orgnNrConstrNr:OriginalNrConstrNr) extends referencingObjAttrs
/**
 *
 * @param formatdes
 * @param formatNr
 * @param spell
 * @param pos
 * @param patternnr
 */
case class PatternAttrs(formatdes:String, formatNr:FormatNr, spell:Spelling, pos:Position, patternnr:PatternNr) extends Group
/**
 *
 * @param formatdes
 * @param formatNr
 * @param spell
 * @param pos
 * @param patternnr
 * @param constr
 */
case class ExtPatAttrs(patAttr:PatternAttrs, constr:String) extends Group
/**
 *
 * @param formatdes
 * @param formatNr
 * @param spell
 * @param pos
 * @param patternnr
 * @param constr
 * @param orgconstrnr
 */
case class OrgPatAttrs(extPatAttrs:ExtPatAttrs, orgconstrnr:Int) extends Group
/**
 *
 * @param formatdes
 * @param formatNr
 * @param spell
 * @param pos
 * @param patternnr
 * @param _loci
 */
case class PatDef(patAttr:PatternAttrs, _loci:List[Loci]) extends Group
/**
 *
 * @param formatdes
 * @param formatNr
 * @param spell
 * @param pos
 * @param patternnr
 * @param constr
 * @param _loci
 */
case class ExtPatDef(extPatAttr:ExtPatAttrs, _loci:List[Loci]) extends Group
/**
 *
 * @param formatdes
 * @param formatNr
 * @param spell
 * @param pos
 * @param patternnr
 * @param constr
 * @param orgconstrnr
 * @param _loci
 * @param _locis
 */
case class OrgPatDef(orgPatAttr:OrgPatAttrs, _loci:List[Locus], _locis:List[Loci]) extends Group
/**
 *
 * @param pos
 * @param orgn
 * @param serNr
 * @param varnr
 */
case class RedVarAttrs(pos:Position, orgn:Origin, serNr:SerialNrIdNr, varnr:VarNr) extends Group
/**
 *
 * @param spell
 * @param kind
 * @param redVarAttr
 */
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
    assert(_subitem.kind == Utils.fullClassName(kind))
  }
}

sealed trait Subitem {
  def kind:String = {
    this.getClass.getName
  }
}
sealed trait MMLIdSubitem extends Subitem {
  def MmlId: MMLId
  def mizarGlobalName():MizarGlobalName = {
    val sgn = this.MmlId.mizarSemiGlobalName()
    sgn.makeGlobalName(this.kind)
  }
}
case class Reservation(_reservationSegments: List[Reservation_Segment]) extends Subitem
case class Definition_Item(_block:Block) extends Subitem {
  def check() = {
    assert(Utils.fullClassName(_block.kind) == this.getClass.getName)
  }
}
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
case class Scheme_Block_Item(MmlId: MMLId, _blocks:List[Block]) extends MMLIdSubitem
//telling Mizar to remember these properties for proofs later
case class Property(_props:Properties, _just:Option[Justification]) extends Subitem {
  def matchProperty(pr:property) : property = {
    val prop = _props.property.get
    prop match {
      case "commutativity" => commutativity(_just)
      case "idempotence" => idempotence(_just)
      case "involutiveness" => involutiveness(_just)
      case "projectivity" => projectivity(_just)
      case "reflexivity" => reflexivity(_just)
      case "irreflexivity" => irreflexivity(_just)
      case "symmetry" => symmetry(_just)
      case "assymmetry" => assymmetry(_just)
      case "connectiveness" => connectiveness(_just)
      case "sethood" => sethood(_just)
    }
  }
}
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
/**
 * corresponsds to a recosider
 * followed by a list of assignments
  */
case class Type_Changing_Statement(_eqList:Equalities_List, _tp:Type, _just:Justification) extends Statement
case class Regular_Statement(prfClaim:ProvedClaim) extends Statement
case class Theorem_Item(MmlId:MMLId, prfClaim:ProvedClaim) extends Statement with MMLIdSubitem
case class Choice_Statement(_qual:Qualified_Segments, prfClaim:ProvedClaim) extends Statement

sealed trait Definition extends Subitem
case class Attribute_Definition(MmlId:MMLId, _redef:Redefine, _attrPat:Attribute_Pattern, _def:Option[Definiens]) extends Definition with MMLIdSubitem
case class Functor_Definition(MmlId:MMLId, _redefine:Redefine, _pat:Patterns, _tpSpec:Option[Type_Specification], _def:Option[Definiens]) extends Definition with MMLIdSubitem
case class Predicate_Definition(MmlId:MMLId, _redefine:Redefine, _predPat:Predicate_Pattern, _def:Option[Definiens]) extends Definition with MMLIdSubitem

/**
 * definition of a structure, takes ancestors (a list of structures it inherits from),
 * structure-Pattern contains several loci, corresponds to the universes the structure is over,
 * a list of field segments, which first needs to repeat the fieldsSegments of the inherited structures
 * then the new ones
 * a field segment is a list of selectors of the same type
 * a selector is a field of the structure (or an inherited one)
 * finally there is a Structure_Patterns_Rendering defining the new notations (namely introducing the selectors)
 * @param _ancestors
 * @param _strPat
 * @param _fieldSegms
 * @param _rendering
 */
case class Structure_Definition(_ancestors:Ancestors, _strPat:Structure_Pattern, _fieldSegms:List[Field_Segments], _rendering:Structure_Patterns_Rendering) extends Definition
case class Constant_Definition(_children:List[Equating]) extends Definition
case class Mode_Definition(_redef:Redefine, _pat:Mode_Pattern, _expMode:Modes) extends Definition
/**
 * 1-1 corresponds to a deffunc definition, its uses are private_functor_terms
 * used as shortcut and visible within its block
 * @param _var
 * @param _tpList
 * @param _tm
 */
case class Private_Functor_Definition(_var:Variable, _tpList:Type_List, _tm:Term) extends Definition
case class Private_Predicate_Definition(_var:Variable, _tpList:Type_List, _form:Formula) extends Definition

sealed trait VariableSegments {
  def _tp(): Type
  def _vars() : List[Variable]
}
case class Free_Variable_Segment(pos:Position, _var:Variable, _tp:Type) extends VariableSegments {
  override def _vars(): List[Variable] = List(_var)
}
case class Implicitly_Qualified_Segment(pos:Position, _var:Variable, _tp:ReservedDscr_Type) extends VariableSegments {
  override def _vars(): List[Variable] = List(_var)
}
case class Explicitly_Qualified_Segment(pos:Position, _variables:Variables, _tp:Type) extends VariableSegments {
  def _vars() = {_variables._vars}
}
case class Qualified_Segments(_children:List[VariableSegments])

sealed trait Expression
sealed trait Type extends Expression
/*
  Non expandable types are mode with implicit definition (using means), cannot expand
  Expandable types are modes with explicit definitions introduced by is
 */
/**
 * refers to the type of a term whoose type is declared earlier
 * @param idnr
 * @param nr
 * @param srt
 * @param _subs
 * @param _tp
 */
case class ReservedDscr_Type(idnr: IdNr, nr: Nr, srt: Sort, _subs:Substitutions, _tp:Type) extends Type
/**
 * predicate subtypes
 * @param srt
 * @param pos
 * @param _adjClust
 * @param _tp
 */
case class Clustered_Type(redObjSubAttrs: RedObjSubAttrs, _adjClust:Adjective_Cluster, _tp:Type) extends Type
/**
 * Any noun
 * @param tpAttrs
 * @param noocc
 * @param origNr
 * @param _args
 */
case class Standard_Type(tpAttrs:ExtObjAttrs, noocc: Option[Boolean], origNr: OriginalNrConstrNr, _args:Arguments) extends Type {
  def mizarGlobalName() = tpAttrs.globalName()
}
/**
 * the type of a structure
 * @param tpAttrs
 * @param _args
 */
case class Struct_Type(tpAttrs:ConstrExtObjAttrs, _args:Arguments) extends Type

/**
 * In Mizar Terminology a complex term is any Expression
 */
sealed trait Term extends Expression {
  def sort() : Sort
  def pos() : Position
}
sealed trait ComplexTerm extends Term {
  def objAttr() : RedObjectSubAttrs
  override def sort() : Sort = objAttr().sort
  override def pos(): Position = objAttr().pos
}
/*
Single variable using constant
in exemplification use
take tm
need to show tm has correct type

use reconsider t
reconsider x = 1 ->

Denotes a constant
 */
/**
 * A constant term
 * @param varAttr
 * @param sort
 */
case class Simple_Term(varAttr:RedVarAttrs, sort:Sort) extends Term {
  override def pos(): Position = varAttr.pos
}
/**
 * introduction form of a structure
 * the arguments (arguments and field values of the selectors) to the structure are specified
 * in the _args child
 * @param tpAttrs
 * @param _args
 */
case class Aggregate_Term(objAttr:ConstrExtObjAttrs, _args:Arguments) extends ComplexTerm
/**
 * result of applying a selector (specified as attributes) to a term (as child),
 * whoose tp is a structure including this selector
 * @param tpAttrs
 * @param _args
 */
case class Selector_Term(objAttr:ConstrExtObjAttrs, _args:Term) extends ComplexTerm
/**
 * an expression containing an circumfix operator -> an OMA
 * spelling contains the left delimiter, right circumflex symbol the right delimiter
 * @param tpAttrs
 * @param _symbol
 * @param _args
 */
case class Circumfix_Term(objAttr:OrgnlExtObjAttrs, _symbol:Right_Circumflex_Symbol, _args:Arguments) extends ComplexTerm
/**
 * An integer value, the value is stored in the attribute number
 * @param pos
 * @param nr
 * @param sort
 * @param varnr
 */
case class Numeral_Term(objAttr: RedObjAttr, nr:Int, varnr:VarNr) extends ComplexTerm
/**
 * Corresponds to the it in an implicit definition for functors and modes
 * @param pos
 * @param sort
 */
case class it_Term(objAttr: RedObjSubAttrs) extends ComplexTerm
case class Internal_Selector_Term(objAttr: RedObjAttr, varnr:VarNr) extends ComplexTerm
/**
 * an expression containing an infix operator -> an OMA
 * @param tpAttrs
 * @param infixedArgs
 */
case class Infix_Term(objAttr:OrgnlExtObjAttrs, infixedArgs: InfixedArgs) extends ComplexTerm
/**
 * generated by
 * the tp
 * it gives us a globally unique (always the same) non-fixed element of tp
 *
 * @param sort
 * @param pos
 * @param _tp
 */
case class Global_Choice_Term(objAttr: RedObjSubAttrs, _tp:Type) extends ComplexTerm
// corresponds to a $num in a (local) definition, referring to the num-th argument to the functor
case class Placeholder_Term(objAttr: RedObjAttr, varnr:Int) extends ComplexTerm
// deffunc
case class Private_Functor_Term(objAttr: RedObjAttr, serialnr:SerialNrIdNr, _args:Arguments) extends ComplexTerm
/**
 * invoking specification axiom for sets
 * generated by
 * {tm where a, b,... is Element of bigger_universe : form }
 * if we have no contradition for form, we can also write equivalently
 * the set of all tm where a, b, ... is Element of bigger_universe
 * which generates a simple fraenkel term
 *
 * @param pos
 * @param sort
 * @param _varSegms
 * @param _tm
 * @param _form
 */
case class Fraenkel_Term(objAttr: RedObjSubAttrs, _varSegms:Variable_Segments, _tm:Term, _form:Formula) extends ComplexTerm
/**
 * invoking specification axiom for sets
 * generated by
 * the set of all formula where a, b, ... is Element of bigger_universe
 * @param pos
 * @param sort
 * @param _varSegms
 * @param _tm
 */
case class Simple_Fraenkel_Term(objAttr: RedObjSubAttrs, _varSegms:Variable_Segments, _tm:Term) extends ComplexTerm
/**
 * generated by
 * term qua tp
 * checks whether term has type tp, otherwise gives 116 error
 * if it does it returns the term as an element of the tp
 *
 * if mizar can't proof it itself, one can use reconsider x = tm as tp by prf instead
 * and provide a proof (but no proof block allowed here)
 * @param pos
 * @param sort
 * @param _tm
 * @param _tp
 */
case class Qualification_Term(objAttr: RedObjSubAttrs, _tm:Term, _tp:Type) extends ComplexTerm
/**
 * given structure T inheriting from structure S, t instance of structure T
 * writing in Mizar
 * the S of t
 * returns an instance of S with same selectors and arguments of the corresponding arguments and selectors of t
 * @param constrExtObjAttrs
 * @param _tm
 */
case class Forgetful_Functor_Term(objAttr: ConstrExtObjAttrs, _tm:Term) extends ComplexTerm

sealed trait Formula extends Claim with Expression
/*
primitive FOL formuli and relational formula, Multi_Attributive_Formula and Qualifying_Formula
 */
case class Existential_Quantifier_Formula(redObjectSubAttrs: RedObjSubAttrs, _vars:Variable_Segments, _expression:Claim) extends Formula
/**
 * Applying a relation to its arguments (and possibly negating the result) //An assignment to a variable
 * @param objectAttrs
 * @param infixedArgs
 */
case class Relation_Formula(objectAttrs: OrgnlExtObjAttrs, antonymic:Option[Boolean], infixedArgs: InfixedArgs) extends Formula
/**
 * forall
 * @param srt
 * @param pos
 * @param _vars
 * @param _restrict
 * @param _expression
 */
case class Universal_Quantifier_Formula(redObjectSubAttrs: RedObjSubAttrs, _vars:Variable_Segments, _restrict:Option[Restriction], _expression:Claim) extends Formula

/**
 * generated by
 * is adjective1, adjective2, ...
 * @param srt
 * @param pos
 * @param _tm
 * @param _clusters
 */
case class Multi_Attributive_Formula(redObjectSubAttrs: RedObjSubAttrs, _tm:Term, _clusters:List[Adjective_Cluster]) extends Formula
/**
 * implication
 * @param srt
 * @param pos
 * @param _formulae
 */
case class Conditional_Formula(redObjectSubAttrs: RedObjSubAttrs, _assumption:Claim, _conclusion:Claim) extends Formula
/**
 * Multinary and
 * @param srt
 * @param pos
 * @param _formulae
 */
case class Conjunctive_Formula(redObjectSubAttrs: RedObjSubAttrs, _frstConjunct:Claim, _sndConjunct:Claim) extends Formula
/**
 * Doube implication
 * @param srt
 * @param pos
 * @param _frstFormula
 * @param _sndFormula
 */
case class Biconditional_Formula(redObjectSubAttrs: RedObjSubAttrs, _frstFormula:Claim, _sndFormula:Claim) extends Formula
/**
 * Multinary or
 * @param srt
 * @param pos
 * @param _formulae
 */
case class Disjunctive_Formula(redObjectSubAttrs: RedObjSubAttrs, _frstDisjunct: Claim, _sndDisjunct:Claim) extends Formula
/**
 * negation
 * @param sort
 * @param pos
 * @param _formula
 */
case class Negated_Formula(redObjectSubAttrs: RedObjSubAttrs, _formula:Claim) extends Formula
/**
  an contradiction object
 */
case class Contradiction(redObjectSubAttrs: RedObjSubAttrs) extends Formula
/**
  generated by
  is tp
 */
case class Qualifying_Formula(redObjectSubAttrs: RedObjSubAttrs, _tm:Term, _tp:Type) extends Formula
case class Private_Predicate_Formula(redObjAttr:RedObjAttr, serialNr: SerialNrIdNr, constrNr: ConstrNr, _args:Arguments) extends Formula
/**
 * A disjunctive formula form1 or ... or formn
 * it gets elaborated by mizar to either a (expanded) disjunctive formula or
 * an existential quantification formula stating that for some arguments of such shape (in below case in the integer range)
 * the corresponding disjunct holds
 *
 * example:
 * 1=1 or ... or 5=5 -> 1=1 or 2=2 or 3=3 or 4=4 or 5=5
 * 1=1 or ... or 100 = 100 -> ex i being natural number st 0 <= i <= 100 & i=i
 *
 * @param srt
 * @param pos
 * @param _formulae
 */
case class FlexaryDisjunctive_Formula(redObjSubAttrs: RedObjSubAttrs, _formulae:List[Claim]) extends Formula
/**
 * A conjunctive formula form1 & ... & formn
 * it gets elaborated by mizar to either a (expanded) conjunctive formula or
 * a universal quantification formula stating that for some arguments of such shape (in below case in the integer range)
 * the corresponding disjunct holds
 * @param srt
 * @param pos
 * @param _formulae
 */
case class FlexaryConjunctive_Formula(redObjSubAttrs: RedObjSubAttrs, _formulae:List[Claim]) extends Formula
case class Multi_Relation_Formula(redObjSubAttrs: RedObjSubAttrs, _relForm:Relation_Formula, _rhsOfRFs:List[RightSideOf_Relation_Formula]) extends Formula

case class RightSideOf_Relation_Formula(objAttr:OrgnlExtObjAttrs, infixedArgs: InfixedArgs)

sealed trait Claim
case class Proposition(pos:Position, _label:Label, _thesis:Claim) extends Claim
/**
  whatever still remains to be proven in a proof
  thesis is changed by
  let cuts a univ quantifier
  take cut ex quantifier
  assume cuts an assumption
  thus cuts a conjunct
  hence works as thus followed by then, uses previous statement to kill a conjunct of thesis
  given cuts an assumption that certain object exists

  conjunct need to be proven in order of claim, so
  now ... end or thus ... proof ... end
  proofs the next conjunct, so thesis is changed to respective conjunct or claim
  hereby is abbreviation for thus + now
 */
case class Thesis(redObjSubAttrs: RedObjSubAttrs) extends Claim
/** corresponds to a  now ... end block
* makes the statements inside known to mizar, even if unproven or even false
* this is never needed, but often convenient
*/
case class Diffuse_Statement(spell:Spelling, serialnr:SerialNrIdNr, labelnr:Int, _label:Label) extends Claim
case class Conditions(_props:List[Proposition]) extends Claim
case class Iterative_Equality(_label:Label, _formula:Formula, _just:Justification, _iterSteps:List[Iterative_Step]) extends Claim

sealed trait Assumptions
case class Single_Assumption(pos:Position, _prop:Proposition) extends Assumptions
case class Collective_Assumption(pos:Position, _cond:Conditions) extends Assumptions
case class Existential_Assumption(_qualSegm:Qualified_Segments, _cond:Conditions) extends Assumptions with Subitem

sealed trait Justification
case class Straightforward_Justification(pos:Position, _refs:List[Reference]) extends Justification
case class Block(kind: String, pos:Positions, _items:List[Item]) extends Justification
case class Scheme_Justification(posNr:PosNr, idnr:IdNr, schnr:Int, spell:Spelling, _refs:List[Reference]) extends Justification

/** Notations */
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
/**
 * saying that a term tm has some properties
 * @param pos
 * @param _aggrTerm
 * @param _adjCl
 * @param _tp
 */
case class Functorial_Registration(pos:Position, _aggrTerm:Term, _adjCl:Adjective_Cluster, _tp:Option[Type]) extends Registrations
case class Existential_Registration(pos:Position, _adjClust:Adjective_Cluster, _tp:Type) extends Registrations
/**
 * showing that some attributes implies other attributes
 * @param pos
 * @param _adjClusts
 * @param _tp
 */
case class Conditional_Registration(pos:Position, _adjClusts:List[Adjective_Cluster], _tp:Type) extends Registrations
/**
 * registering properties for a mode
 */
case class Property_Registration(_props:Properties, _block:Block) extends Registrations with Subitem

/**
 * Well-definedness conditions that need to be proven along with definitions
 */
sealed trait CorrectnessConditions
/**
 * non-emptyness of non-expandable typed (modes) or clustered_types in registrations of attributes
 */
case class existence() extends CorrectnessConditions
/**
 * uniqueness for functors
 */
case class uniqueness() extends CorrectnessConditions
/**
 * can define functor using means or equals
 * if defined using equals need coherence to correct type
 */
case class coherence() extends CorrectnessConditions
/**
 * reduce x * 1.L to x
 * reducibility by Def6
 */
case class reducibility() extends CorrectnessConditions
case class compatibility() extends CorrectnessConditions
/**
 * for overlap of case-by-case (complex) defns
 */
case class consistency() extends CorrectnessConditions
/**
 * conjunction of all necessary correctness conditions, doesn't appear in esx files
 */
case class correctness() extends CorrectnessConditions

sealed trait Exemplifications
case class ImplicitExemplification(_term:Term) extends Exemplifications
case class ExemplifyingVariable(_var:Variable, _simplTm:Simple_Term) extends Exemplifications
case class Example(_var:Variable, _tm:Term) extends Exemplifications

sealed trait Reference
case class Local_Reference(pos:Position, spell:Spelling, serialnumber:SerialNrIdNr, labelnr:Int) extends Reference
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
case class Label(spell:Spelling, pos:Position, serialnr:SerialNrIdNr, labelnr:Int)
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

case class According_Expansion(_attrs:List[Attribute])

object Utils {
  def fullClassName(s: String) = {
    "info.kwarc.mmt.mizar.newxml.syntax."+s.replace("-", "_")
  }
  case class MizarSemiGlobalName(aid:String, nr:Int) {
    def makeGlobalName(kind:String) : MizarGlobalName = {MizarGlobalName(aid, kind, nr)}
  }
  case class MizarGlobalName(aid:String, kind: String, nr:Int)

  /**
   * Internal representation of Properties class
   * @param _just (optional) the proof of the property
   */
  sealed abstract class property(_just:Option[Justification])
  //for functors
  // for bin op
  case class commutativity(_just:Option[Justification]) extends property(_just:Option[Justification])
  //for bin op
  case class idempotence(_just:Option[Justification]) extends property(_just:Option[Justification])
  // for un op
  case class involutiveness(_just:Option[Justification]) extends property(_just:Option[Justification])
  // being a proj op, for un op
  case class projectivity(_just:Option[Justification]) extends property(_just:Option[Justification])

  //for predicate
  case class reflexivity(_just:Option[Justification]) extends property(_just:Option[Justification])
  case class irreflexivity(_just:Option[Justification]) extends property(_just:Option[Justification])
  case class symmetry(_just:Option[Justification]) extends property(_just:Option[Justification])
  case class assymmetry(_just:Option[Justification]) extends property(_just:Option[Justification])
  case class connectiveness(_just:Option[Justification]) extends property(_just:Option[Justification])

  //for modes and existential_registrations
  //only those modes (and subtypes, expanded into) can be used as types in fraenkel_terms
  case class sethood(_just:Option[Justification]) extends property(_just:Option[Justification])
}