package info.kwarc.mmt.mizar.newxml.syntax

import scala.tools.cmd.Meta.Opt
import scala.xml.Attribute

case class Text_Proper(articleid: String, articleext: String, position: String, _items: List[Item])
case class Item(kind: String, position: String, endposition:String, _subitems:List[Subitem])
trait Subitem
case class Reservation(_reservationSegments: List[Reservation_Segment]) extends Subitem
case class Definition_Item(_block:Block) extends Subitem
case class Section_Pragma() extends Subitem
case class Pragma(_canceled:Canceled) extends Subitem
case class Loci_Declaration(_qualSegms:Qualified_Segments) extends Subitem
case class Cluster(_registrs:List[Registrations]) extends Subitem
case class Correctness(_correctnessCond:Correctness_Conditions, _just:Justification) extends Subitem
case class Correctness_Condition(_cond:CorrectnessConditions, _just:Option[Justification]) extends Subitem
case class Exemplification(_exams:List[Exemplifications]) extends Subitem
case class Assumption(_ass:Assumptions) extends Claim with Subitem
case class Identify(_pats:List[Patterns], _lociEqns:Loci_Equalities) extends Subitem
case class Generalization(_qual:Qualified_Segments, _conds:Option[Claim]) extends Subitem // let
case class Reduction(_infTm:Infix_Term, _simpTm:Simple_Term) extends Subitem
case class Func_Synonym(_infFuncPats:List[InfixFunctor_Pattern]) extends Subitem

trait Statement extends Subitem
case class Conclusion(_prop:Claim, _just:Option[Justification]) extends Statement
case class Type_Changing_Statement(_eqList:Equalities_List, _tp:Type, _just:Justification) extends Statement
case class Regular_Statement(_claim:Claim, _just:Option[Justification]) extends Statement
case class Theorem_Item(MMLID:String, _claim:Claim, _just:Option[Justification]) extends Statement
case class Choice_Statement(_qual:Qualified_Segments, _claim:Claim, _just:Option[Justification]) extends Statement

trait Definition extends Subitem
case class Structure_Definition(_ancestors:Ancestors, _strPat:Structure_Pattern, _fieldSegms:List[Field_Segments], _rendering:Structure_Patterns_Rendering) extends Definition
case class Attribute_Definition(MMLId:String, _redef:Redefine, _attrPat:Attribute_Pattern, _def:Definiens) extends Definition
case class Constant_Definition(_children:List[Equating]) extends Definition
case class Functor_Definition(MMLId:String, _redefine:Redefine, _pat:Patterns, _tpSpec:Option[Type_Specification], _def:Definiens) extends Definition
case class Mode_Definition(_redef:Redefine, _pat:Mode_Pattern, _expMode:Expandable_Mode) extends Definition
case class Private_Functor_Definition(_var:Variable, _tpList:Type_List, _tm:Term) extends Definition

trait VariableSegments
case class Free_Variable_Segment(position:String, _var:Variable, _tp:Type) extends VariableSegments
case class Implicitly_Qualified_Segment(position:String, _var:Variable, _rescDesc:ReservedDscr_Type) extends VariableSegments
case class Explicitly_Qualified_Segment(position:String, _vars:Variables, _tp:Type) extends VariableSegments
case class Qualified_Segments(_children:List[VariableSegments])

trait Type extends Expression
case class ReservedDscr_Type(idnr: Int, nr: Int, sort: String, _subs:Substitutions, _tp:Type) extends Type
case class Clustered_Type(sort:String, position: String, _adjClust:Adjective_Cluster, _tp:Type) extends Type
case class Standard_Type(nr: Int, formatnr: Int, patternnr:Int, spelling: String, noocc: Option[Boolean], sort:String, constrnr: Int, originalnr: Int, position:String, _args:List[Arguments]) extends Type
case class Struct_Type(nr:Int, formatnr:Int, patternnr:Int, spelling:String, sort:String, constrnr:Int, position:String, _args:Arguments) extends Type

trait Expression
trait Term extends Expression
case class Simple_Term(idnr:Int, spelling:String, position:String, origin:String, sort:String, serialnr:Int, varnr:Int) extends Term
case class Aggregate_Term(nr:Int, formatnr:Int, patternnr:Int, spelling:String, sort:String, constrnr:Int, position:String, _args:Arguments) extends Term
case class Selector_Term(nr:Int, formatnr:Int, patternnr:Int, spelling:String, sort:String, constrnr:Int, position:String, _args:List[Term]) extends Term
case class Circumfix_Term(nr:Int, formatnr:Int, patternnr:Int, spelling:String, sort:String, constrnr:Int, originalnr:Int, position:String, _symbol:Right_Circumflex_Symbol, _args:Arguments) extends Term
case class Numeral_Term(nr:Int, sort:String, varnr:Int, position:String) extends Term
case class it_Term(position:String, sort:String) extends Term
case class Internal_Selector_Term(nr:Int, sort:String, varnr:Int, spelling:String, position:String) extends Term
case class Infix_Term(nr:Int, formatnr:Int, patternnr:Int, leftargscount:Int, spelling:String, sort:String, constrnr:Int, originalnr:Int, position:String, _args:Arguments) extends Term
case class Global_Choice_Term(sort:String, position:String, _tp:Type) extends Term
case class Placeholder_Term(nr:Int, spelling:String, sort:String, varnr:Int, position:String) extends Term
case class Private_Functor_Term(idnr:Int, spelling:String, position:String, sort:String, serialnr:Int, nr:Int, _args:Arguments) extends Term

trait Patterns
case class Structure_Pattern(formatdes:String, formatnr:Int, spelling:String, position:String, patternnr:Int, constr:String, _loci:List[Loci])
case class AggregateFunctor_Pattern(formatdes:String, formatnr:Int, spelling:String, position:String, patternnr:Int, constr:String, _loci:List[Loci]) extends Patterns
case class ForgetfulFunctor_Pattern(formatdes:String, formatnr:Int, spelling:String, position:String, patternnr:Int, constr:String, _loci:List[Loci]) extends Patterns
case class Strict_Pattern(formatdes:String, formatnr:Int, spelling:String, position:String, patternnr:Int, constr:String, orgconstrnr:Int, _loci:List[Locus], _locis:List[Loci]) extends Patterns
case class Attribute_Pattern(formatdes:String, formatnr:Int, spelling:String, position:String, patternnr:Int, constr:String, orgconstrnr:Int, _loci:List[Locus], _locis:List[Loci]) extends Patterns
case class InfixFunctor_Pattern(formatdes:String, formatnr:Int, spelling:String, position:String, patternnr:Int, constr:String, orgconstrnr:Int, _loci:List[Locus], _locis:List[Loci]) extends Patterns
case class SelectorFunctor_Pattern(formatdes:String, formatnr:Int, spelling:String, position:String, patternnr:Int, constr:String, _loci:List[Loci]) extends Patterns
case class Mode_Pattern(formatdes:String, formatnr:Int, spelling:String, position:String, patternnr:Int, _loci:List[Loci]) extends Patterns

trait Registrations
case class Functorial_Registration(position:String, _aggrTerm:Term, _adjCl:Adjective_Cluster) extends Registrations
case class Existential_Registration(position:String, _adjClust:Adjective_Cluster, _tp:Type) extends Registrations
case class Conditional_Registration(position:String, _adjClusts:List[Adjective_Cluster], _tp:Type) extends Registrations

trait CorrectnessConditions
case class coherence() extends CorrectnessConditions
case class existence() extends CorrectnessConditions
case class uniqueness() extends CorrectnessConditions
case class reducibility() extends CorrectnessConditions
case class compatibility() extends CorrectnessConditions

trait Justification
case class Straightforward_Justification(position:String, _refs:List[Reference]) extends Justification
case class Block(kind: String, position: String, endposition: String, _items:List[Item]) extends Justification
case class Scheme_Justification(nr:Int, idnr:Int, schnr:Int, spellinig:String, position:String, endposition:String) extends Justification

trait Claim
case class Proposition(position:String, _label:Label, _thesis:Claim) extends Claim
case class Thesis(position:String, sort:String) extends Claim
case class Diffuse_Statement(idnr:Int, spelling:String, serialnr:Int, labelnr:Int, _label:Label) extends Claim
case class Conditions(_props:List[Proposition]) extends Claim
case class Iterative_Equality(_label:Label, _formula:Formula, _just:Justification, _iterSteps:List[Iterative_Step]) extends Claim

trait Formula extends Claim with Expression
case class Existential_Quantifier_Formula(sort:String, position:String, _vars:Variable_Segments, _expression:Formula) extends Formula
case class Relation_Formula(nr:Int, formatnr:Int, patternnr:Int, leftargscount:Int, spelling:String, sort:String, constrnr:Int, originalnr:Int, position:String, _args:Arguments) extends Formula
case class Universal_Quantifier_Formula(sort:String, position:String, _vars:Variable_Segments, _restrict:Option[Restriction], _expression:Formula) extends Formula
case class Multi_Attributive_Formula(sort:String, position:String, _tm:Term, _clusters:List[Adjective_Cluster]) extends Formula
case class Conditional_Formula(sort:String, position:String, _formulae:List[Formula]) extends Formula
case class Conjunctive_Formula(sort:String, position:String, _formulae:List[Formula]) extends Formula
case class Biconditional_Formula(sort:String, position:String, _frstFormula:Formula, _sndFormula:Formula) extends Formula
case class Disjunctive_Formula(sort:String, position:String, _formulae:List[Formula]) extends Formula
case class Negated_Formula(sort:String, position:String, _formula:Claim) extends Formula
case class Contradiction(sort:String, position:String) extends Formula
case class Qualifying_Formula(sort:String, position:String, _tm:Term, _tp:Type) extends Formula

trait Assumptions
case class Single_Assumption(position:String, _prop:Proposition) extends Assumptions
case class Collective_Assumption(postion:String, _cond:Conditions) extends Assumptions

trait Exemplifications
case class ImplicitExemplification(_term:Term) extends Exemplifications
case class ExemplifyingVariable(_var:Variable, _simplTm:Simple_Term) extends Exemplifications
case class Example(_var:Variable, _tm:Term) extends Exemplifications

trait Reference
case class Local_Reference(position:String, idnr:Int, spelling:String, serialnumber:Int, labelnr:Int) extends Reference
case class Definition_Reference(position:String, nr:Int, spelling:String, number:Int) extends Reference
case class Link(position:String, labelnr:Int) extends Reference
case class Theorem_Reference(position:String, nr:Int, spelling:String, number:Int) extends Reference

trait Modes
case class Expandable_Mode(_tp:Type) extends Modes

case class Reservation_Segment(position: String, _vars:Variables, _varSegm:Variable_Segments, _tp:Type)
case class Variables(_vars:List[Variable])
case class Variable_Segments(_vars:List[VariableSegments])
case class Variable(idnr: Int, spelling: String, position: String, origin:Option[String], kind:String, serialnr: Int, varnr:Int)
case class Substitutions(_childs:List[Substitution])
case class Substitution(freevarnr:Int, kind:String, varnr:Int)
case class Canceled(MMLId:String, amount:Int, kind:String, position:String)
case class Adjective_Cluster(_attrs: List[Attribute])
case class Attribute(nr: Int, formatnr: Int, patternnr:Int, spelling: String, noocc: Option[Boolean], sort:String, constrnr: Int, originalnr: Int, position:String, _args:List[Arguments])
case class Arguments(_children:List[Term])
case class Ancestors(_structTypes:List[Struct_Type])
case class Loci(_loci:List[Locus])
case class Locus(idnr:Int, varidkind:String, spelling:String, position:String, origin:String, kind:String, serialnr:Int, varnr:Int)
case class Field_Segments(_fieldSegments:List[Field_Segment])
case class Field_Segment(position:String, _selectors:Selectors, _standardType:Standard_Type)
case class Selectors(nr:Int, spelling:String, position:String, _loci:List[Selector])
case class Selector(nr:Int, spelling:String, position:String, _loci:List[Locus])
case class Structure_Patterns_Rendering(_aggrFuncPat:AggregateFunctor_Pattern, _forgetfulFuncPat:ForgetfulFunctor_Pattern, _strFuncPat:Strict_Pattern, _selList:Selectors_List)
case class Selectors_List(_children:List[Patterns])
case class Correctness_Conditions(_cond:List[CorrectnessConditions])
case class Redefine(occurs:Boolean)
case class Type_Specification(_types:List[Type])
case class Definiens(position:String, kind:String, shape:String, _label:Label, _expr:Expression)
case class Label(idnr:Int, spelling:String, position:String, serialnr:Int, labelnr:Int)
case class Restriction(_formula:Formula)
case class Right_Circumflex_Symbol(nr:Int, formatnr:Int, spelling:String, position:String)
case class Equating(_var:Variable, _tm:Term)
case class Loci_Equalities(_lociEqns:List[Loci_Equality])
case class Loci_Equality(position:String, _loci:List[Locus])
case class Equalities_List(_eqns:List[Equality])
case class Equality(_var:Variable, _tm:Term)
case class Iterative_Step(position:String, _tm:Term, _just:Justification)
case class Type_List(_tps:List[Type])