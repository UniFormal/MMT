package info.kwarc.mmt.mizar.newxml.syntax

import info.kwarc.mmt.api.{GlobalName, LocalName}
import info.kwarc.mmt.mizar.newxml.syntax.Utils.MizarVariableName
import info.kwarc.mmt.mizar.newxml.translator.{TranslationController, TranslatorUtils}

sealed trait ObjectLevel
sealed trait Claim extends ObjectLevel
sealed trait TypeUnchangingClaim extends Claim
sealed trait VariableSegments extends ObjectLevel {
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
case class Qualified_Segments(_children:List[VariableSegments]) extends ObjectLevel

sealed trait Expression extends ObjectLevel {
  def ThisType() = this.getClass.toString
}
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
case class ReservedDscr_Type(idnr: Int, nr: Int, sort: String, pos: Position, _subs:Substitutions, _tp:Type) extends Type
/**
 * predicate subtypes
 * @param srt
 * @param pos
 * @param _adjClust
 * @param _tp
 */
case class Clustered_Type(pos: Position, sort: String, _adjClust:Adjective_Cluster, _tp:Type) extends Type
sealed trait objRefAttrs extends GloballyReferencingObjAttrs with ObjectLevel {
  def objAttrs: ExtObjAttrs
  override def globalObjAttrs: GlobalObjAttrs = objAttrs.globalObjAttrs
}
/**
 * Any noun
 * A (standard) type is called expandable iff it is explicitely defined
 * @param tpAttrs
 * @param noocc
 * @param origNr
 * @param _args
 */
case class Standard_Type(objAttrs:ExtObjAttrs, _args:Arguments) extends Type with objRefAttrs {
  override def globalKind = Utils.shortKind(Utils.ModeKind())
}
/**
 * the type of a structure (applied to the arguments in _args)
 * @param tpAttrs referencing the (type definitino of the) structure
 * @param _args the arguments
 */
case class Struct_Type(tpAttrs:ConstrExtObjAttrs, _args:Arguments) extends Type with GloballyReferencingDefAttrs {
  override def globalKind = Utils.shortKind(Utils.StructureKind())
  override def globalDefAttrs: GlobalDefAttrs = tpAttrs.globalDefAttrs
}

/**
 * Named MizTerm to avoid name-clashes with api.objects.Term
 */
sealed trait MizTerm extends Expression {
  def sort : String
  def pos : Position
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
 * usually local within some block (e.g. an argument to a definition)
 * @param varAttr
 * @param sort
 */
case class Simple_Term(varAttr:LocalVarAttr) extends MizTerm {
  override def pos: Position = varAttr.locVarAttr.pos
  override def sort: String = varAttr.sort
}

/**
 * In Mizar Terminology a complex term is any Expression
 */
sealed trait ComplexTerm extends MizTerm
sealed trait ObjAttrsComplexTerm extends ComplexTerm {
  def objAttr : RedObjectSubAttrs
  override def sort: String = objAttr.sort
  override def pos: Position = objAttr.pos
}
sealed trait globalRefObjAttrsComplexTerm extends ObjAttrsComplexTerm with GloballyReferencingDefAttrs {
  def objAttr: ReferencingConstrObjAttrs
  override def globalDefAttrs = objAttr.globalDefAttrs
}
/**
 * introduction form of a structure
 * the arguments (arguments and field values of the selectors) to the structure are specified
 * in the _args child
 * @param tpAttrs
 * @param _args
 */
case class Aggregate_Term(objAttr:ConstrExtObjAttrs, _args:Arguments) extends globalRefObjAttrsComplexTerm {
  override def globalKind = Utils.shortKind(Utils.AggregateKind())
}
/**
 * result of applying a selector (specified as attributes) to a term _arg,
 * whose tp is a structure including this selector
 * @param tpAttrs
 * @param _arg
 */
case class Selector_Term(objAttr:ConstrExtObjAttrs, _arg:MizTerm) extends globalRefObjAttrsComplexTerm {
  override def globalKind = Utils.shortKind(Utils.SelectorKind())
}
/**
 * an expression containing an circumfix operator -> an OMA in MMT Terminology
 * spelling contains the left delimiter, right circumflex symbol the right delimiter
 * @param tpAttrs references the function to apply
 * @param _symbol contains the right delimiter
 * @param _args the arguments to apply the function to
 */
case class Circumfix_Term(objAttr:OrgnlExtObjAttrs, _symbol:Right_Circumflex_Symbol, _args:Arguments) extends globalRefObjAttrsComplexTerm {
  override def globalKind = Utils.shortKind(Utils.FunctorKind())
}
/**
 * An integer value, the value is stored in the attribute number
 * @param objAttr
 * @param nr
 * @param varnr
 */
case class Numeral_Term(number:Int, sort:String, pos: Position) extends MizTerm
/**
 * Corresponds to the it in an implicit definition for functors and modes
 * @param pos
 * @param sort
 */
case class it_Term(pos: Position, sort: String) extends MizTerm
/**
 * Refers to a selector term of a selector already defined within a
 * definition of a new mizar structure
 * @param objAttr
 * @param varnr
 */
case class Internal_Selector_Term(objAttr: RedObjAttr, varnr:Int) extends ObjAttrsComplexTerm
/**
 * an expression containing an infix operator -> an OMA
 * @param tpAttrs
 * @param infixedArgs
 */
case class Infix_Term(objAttr:OrgnlExtObjAttrs, infixedArgs: InfixedArgs) extends globalRefObjAttrsComplexTerm {
  override def globalKind = Utils.shortKind(Utils.FunctorKind())
}
/**
 * generated by
 * the tp
 * it gives us a globally unique (always the same) non-fixed element of tp
 *
 * @param sort
 * @param pos
 * @param _tp
 */
case class Global_Choice_Term(pos: Position, sort: String, _tp:Type) extends ComplexTerm
// corresponds to a $num in a (local) definition, referring to the num-th argument to the functor
case class Placeholder_Term(objAttr: RedObjAttr, varnr:Int) extends ObjAttrsComplexTerm
// deffunc
case class Private_Functor_Term(objAttr: RedObjAttr, serialnr:SerialNrIdNr, _args:Arguments) extends ObjAttrsComplexTerm
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
case class Fraenkel_Term(pos: Position, sort: String, _varSegms:Variable_Segments, _tm:MizTerm, _form:Formula) extends ComplexTerm
/**
 * invoking specification axiom for sets
 * generated by
 * the set of all formula where a, b, ... is Element of bigger_universe
 * @param pos
 * @param sort
 * @param _varSegms
 * @param _tm
 */
case class Simple_Fraenkel_Term(pos: Position, sort: String, _varSegms:Variable_Segments, _tm:MizTerm) extends ComplexTerm
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
case class Qualification_Term(pos: Position, sort: String, _tm:MizTerm, _tp:Type) extends ComplexTerm
/**
 * given structure T inheriting from structure S, t instance of structure T
 * writing in Mizar
 * the S of t
 * returns an instance of S with same selectors and arguments of the corresponding arguments and selectors of t
 * @param constrExtObjAttrs
 * @param _tm
 */
case class Forgetful_Functor_Term(objAttr: ConstrExtObjAttrs, _tm:MizTerm) extends globalRefObjAttrsComplexTerm {
  override def globalKind: Char = Utils.shortKind(Utils.FunctorKind())
}
/**
primitive FOL formuli and relational formula, Multi_Attributive_Formula and Qualifying_Formula
 */
sealed trait Formula extends TypeUnchangingClaim with Expression
/**
 * The body of a quantifier formula
 * @param _expr the content of said body
 */
case class Scope(_expr: Claim) extends Formula
/**
 * Existentially quantify the expression _expression over the variables contained in _vars
 * @param pos
 * @param sort
 * @param _vars
 * @param _expression
 */
case class Existential_Quantifier_Formula(pos: Position, _vars:Variable_Segments, _restrict:Option[Restriction], _body: Claim) extends Formula
/**
 * Applying a relation to its arguments (and possibly negating the result) //An assignment to a variable
 * @param objectAttrs
 * @param infixedArgs
 */
case class Relation_Formula(defAttrs: OrgnlExtObjAttrs, antonymic:Option[Boolean], infixedArgs: InfixedArgs) extends Formula with defRefAttrs {
  override def globalKind = Utils.shortKind(Utils.PredicateKind())
}
/**
 * forall
 * @param sort
 * @param pos
 * @param _vars
 * @param _restrict further assumptions on the variable quantified over
 * @param _expression the body of the formula
 */
case class Universal_Quantifier_Formula(pos: Position, _vars:Variable_Segments, _restrict:Option[Restriction], _body:Claim) extends Formula
/**
 * generated by
 * is adjective1, adjective2, ...
 * @param sort
 * @param pos
 * @param _tm
 * @param _cluster
 */
case class Multi_Attributive_Formula(pos: Position, _tm:MizTerm, _cluster:Adjective_Cluster) extends Formula
/**
 * implication
 * @param sort
 * @param pos
 * @param _assumption
 * @param _conclusion
 */
case class Conditional_Formula(pos: Position, _assumption:Claim, _conclusion:Claim) extends Formula
/**
 * and
 * @param sort
 * @param pos
 * @param _frstConjunct
 * @param _sndConjunct
 */
case class Conjunctive_Formula(pos: Position, _frstConjunct:Claim, _sndConjunct:Claim) extends Formula
/**
 * iff
 * @param sort
 * @param pos
 * @param _frstFormula
 * @param _sndFormula
 */
case class Biconditional_Formula(pos: Position, _frstFormula:Claim, _sndFormula:Claim) extends Formula
/**
 * or
 * @param sort
 * @param pos
 * @param _frstDisjunct
 * @param _sndDisjunct
 */
case class Disjunctive_Formula(pos: Position, _frstDisjunct: Claim, _sndDisjunct: Claim) extends Formula
/**
 * negation
 * @param sort
 * @param pos
 * @param _formula
 */
case class Negated_Formula(pos: Position, _formula:Claim) extends Formula
/**
an contradiction object
 */
case class Contradiction(pos: Position) extends Formula
/**
generated by
  is tp
 */
case class Qualifying_Formula(pos: Position, _tm:MizTerm, _tp:Type) extends Formula
case class Private_Predicate_Formula(redObjAttr:RedObjAttr, serialNr: SerialNrIdNr, constrnr: Int, _args:Arguments) extends Formula
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
 * @param sort
 * @param pos
 * @param _formulae
 */
case class FlexaryDisjunctive_Formula(pos: Position, _formulae:List[Claim]) extends Formula
/**
 * A conjunctive formula form1 & ... & formn
 * it gets elaborated by mizar to either a (expanded) conjunctive formula or
 * a universal quantification formula stating that for some arguments of such shape (in below case in the integer range)
 * the corresponding disjunct holds
 * @param srt
 * @param pos
 * @param _formulae
 */
case class FlexaryConjunctive_Formula(pos: Position, _formulae:List[Claim]) extends Formula

/**
 * Apply relations <_i in a sequence to a list of arguments a_1, ..., a_p leading to
 * a_1 <_1 a_2 <_2 ... <_1 a_n_1 <_2 a_n_1+1 <_2 ... <_3 ... <_m a_p
 * @param pos
 * @param sort
 * @param _relForm the relation formula a_1 <_1 a_2
 * @param _rhsOfRFs contains the remaining relations <_2 ... <_m and the arguments those are applied to
 */
case class Multi_Relation_Formula(pos: Position, _relForm:Relation_Formula, _rhsOfRFs:List[RightSideOf_Relation_Formula]) extends Formula
/**
 * see Multi_Relation_Formula
 * @param objAttr
 * @param infixedArgs
 */
case class RightSideOf_Relation_Formula(objAttr:OrgnlExtObjAttrs, infixedArgs: InfixedArgs) extends ObjectLevel

/**
 * A labelled claim
 * within proofs the labels might be deferred to with local references
 * immediately after Therorem, ...
 * @param pos
 * @param _label
 * @param _thesis
 */
case class Proposition(pos:Position, _label:Label, _thesis:Claim) extends TypeUnchangingClaim with ProvenFactReference {
  def referencedLabel = TranslationController.currentTheoryPath ? LocalName(_label.spelling)
}
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
case class Thesis(pos: Position) extends TypeUnchangingClaim
/** corresponds to a  now ... end block
 * makes the statements inside known to mizar, even if unproven or even false
 * this is never needed, but often convenient
 */
case class Diffuse_Statement(_label:Label) extends TypeUnchangingClaim
case class Conditions(_props:List[Proposition]) extends TypeUnchangingClaim
case class Iterative_Equality(_label:Label, _formula:Relation_Formula, _just:Justification, _iterSteps:List[Iterative_Step]) extends TypeUnchangingClaim
private[newxml] case class Type_Changing_Claim(_eqList:Equalities_List, _tp:Type) extends Claim

sealed trait Justification extends ObjectLevel
case class Straightforward_Justification(pos:Position, _refs:List[Reference]) extends Justification
case class Block(kind: String, pos:Positions, _items:List[Item]) extends Justification
case class Scheme_Justification(position: Position, nr: Int, idnr:Int, schnr:Int, spelling:String, _refs:List[ProvenFactReference]) extends Justification

/** Notations */
sealed trait Patterns extends ObjectLevel with GloballyReferencingObjAttrs {
  def patternAttrs: PatternAttrs
  def _locis: List[Loci]
  def patDef: PatDefs = PatDef(patternAttrs, _locis)
  def patKind: Utils.PatternKinds

  override def globalKind: Char = Utils.shortKind(patKind)
  override def globalObjAttrs: GlobalObjAttrs = patDef.globalObjAttrs
}
case class Pattern_Shaped_Expression(_pat: Patterns) extends ObjectLevel
case class Mode_Pattern(patternAttrs: PatternAttrs, _locis: List[Loci]) extends Patterns {
  override def patKind: Utils.PatternKinds = Utils.ModeKind()
}
sealed trait ConstrPattern extends Patterns with GloballyReferencingDefAttrs {
  def extPatAttr: ExtPatAttr
  def _locis:List[Loci]
  def extPatDef: ExtPatDef = ExtPatDef(extPatAttr, _locis)
  override def patternAttrs: PatternAttrs = extPatDef.extPatAttr.patAttr
  override def globalDefAttrs: GlobalDefAttrs = extPatAttr.globalDefAttrs
}
sealed trait RedefinablePatterns extends ConstrPattern with GloballyReferencingReDefAttrs {
  def orgExtPatAttr: OrgExtPatAttr
  def _loci: List[Locus]
  def orgPatDef: OrgPatDef = OrgPatDef(orgExtPatAttr, _loci, _locis)
  override def extPatAttr: ExtPatAttr = orgExtPatAttr.extPatAttr
  override def globalReDefAttrs = orgExtPatAttr.globalReDefAttrs
  def hasOrigRefs = globalReDefAttrs.hasOrigRefs
}
case class Structure_Pattern(extPatAttr: ExtPatAttr, _locis:List[Loci]) extends ConstrPattern {
  override def extPatDef = ExtPatDef(extPatAttr, _locis)
  override def patKind = Utils.StructureKind()
}
case class Attribute_Pattern(orgExtPatAttr: OrgExtPatAttr, _loci: List[Locus], _locis:List[Loci]) extends RedefinablePatterns {
  override def patKind = Utils.AttributeKind()
}
case class Predicate_Pattern(orgExtPatAttr: OrgExtPatAttr, _loci: List[Locus], _locis:List[Loci]) extends RedefinablePatterns {
  override def patKind = Utils.PredicateKind()
}
case class Strict_Pattern(orgExtPatAttr: OrgExtPatAttr, _loci: List[Locus], _locis:List[Loci]) extends RedefinablePatterns {
  override def patKind: Utils.PatternKinds = Utils.StrictKind()
}
sealed trait Functor_Patterns extends Patterns with ConstrPattern
case class AggregateFunctor_Pattern(extPatAttr: ExtPatAttr, _locis:List[Loci]) extends Functor_Patterns {
  override def patKind: Utils.PatternKinds = Utils.AggregateKind()
}
case class ForgetfulFunctor_Pattern(extPatAttr: ExtPatAttr, _locis:List[Loci]) extends Functor_Patterns {
  override def patKind: Utils.PatternKinds = Utils.ForgetfulKind()
}
case class SelectorFunctor_Pattern(extPatAttr: ExtPatAttr, _locis:List[Loci]) extends Functor_Patterns {
  override def patKind: Utils.PatternKinds = Utils.SelectorKind()
}
sealed trait RedefinableFunctor_Patterns extends Functor_Patterns with RedefinablePatterns {
  override def patKind: Utils.PatternKinds = Utils.FunctorKind()
}
case class InfixFunctor_Pattern(rightargsbracketed: Option[Boolean], orgExtPatAttr: OrgExtPatAttr, _loci: List[Locus], _locis:List[Loci]) extends RedefinableFunctor_Patterns
case class CircumfixFunctor_Pattern(orgExtPatAttr: OrgExtPatAttr, _right_Circumflex_Symbol: Right_Circumflex_Symbol, _loci: List[Locus], _locis:List[Loci]) extends RedefinableFunctor_Patterns

sealed trait Exemplifications extends ObjectLevel {
  def _tm: MizTerm
  def varO: Option[Variable] = None
}
case class ImplicitExemplification(_tm:MizTerm) extends Exemplifications
case class ExemplifyingVariable(_var:Variable, _tm:Simple_Term) extends Exemplifications {
  override def varO: Option[Variable] = Some(_var)
}
case class Example(_var:Variable, _tm:MizTerm) extends Exemplifications {
  override def varO: Option[Variable] = Some(_var)
}

sealed trait Reference extends ObjectLevel
trait ProvenFactReference extends Reference {
  def referencedLabel: GlobalName
}
case class Local_Reference(pos:Position, spelling:String, serialnumber:SerialNrIdNr, labelnr:Int) extends ProvenFactReference {
  def referencedLabel = TranslationController.currentTheoryPath ? LocalName(spelling)
}
case class Definition_Reference(position: Position, nr: Int, spelling:String, number:Int) extends ProvenFactReference {
  def referencedLabel = TranslationController.getTheoryPath(spelling) ? LocalName("Def"+number)
}
/**
 * I don't really understand the semantics of a link, so far they seem to not have any meaning at all
 * @param pos
 * @param labelnr
 */
case class Link(pos:Position, labelnr:Int) extends Reference
case class Theorem_Reference(position: Position, nr: Int, spelling:String, number:Int) extends ProvenFactReference {
  override def referencedLabel: GlobalName = {
    TranslationController.getTheoryPath(spelling) ? LocalName("Theorem-Item"+number)
  }
}

sealed trait Modes extends ObjectLevel
case class Expandable_Mode(_tp:Type) extends Modes

/**
 * Always at least of the _tpSpec and _def are defined
 * @param _tpSpec
 * @param _def
 */
case class Standard_Mode(_tpSpec:Option[Type_Specification], _def:Option[Definiens]) extends Modes
sealed trait EqualityTr extends ObjectLevel {
  def _var: Variable
  def _tm: MizTerm
}
/**
 * Introduce a new variable equal to the given Term, show its coherence to another type
 * and consequently view the variable as a variable of that type
 * @param _var
 * @param _tm
 */
case class Equality(_var:Variable, _tm:MizTerm) extends EqualityTr
/**
 * Shows that a variable is coherent also to another type and view it as an element of that type from now on
 * @param _var
 * @param _tm
 */
case class Equality_To_Itself(_var:Variable, _tm:MizTerm) extends EqualityTr

case class Schematic_Variables(_segms:List[Segments]) extends ObjectLevel
case class Variables(_vars:List[Variable]) extends ObjectLevel
case class Variable_Segments(_vars:List[VariableSegments]) extends ObjectLevel
case class Variable(varAttr:VarAttrs) extends ObjectLevel
case class Substitutions(_childs:List[Substitution]) extends ObjectLevel
case class Substitution(freevarnr:Int, kind:String, varnr:Int) extends ObjectLevel
case class Adjective_Cluster(_attrs: List[Attribute]) extends ObjectLevel
sealed trait defRefAttrs extends GloballyReferencingDefAttrs with objRefAttrs {
  def defAttrs: OrgnlExtObjAttrs
  override def objAttrs: ExtObjAttrs = ExtObjAttrs(defAttrs.pos, defAttrs.nr, defAttrs.formatnr, defAttrs.patternnr, defAttrs.spelling, defAttrs.sort, GlobalObjAttrs(defAttrs.globalDefAttrs.absolutepatternMMLId))
  override def globalDefAttrs: GlobalDefAttrs = defAttrs.globalDefAttrs
}
case class Attribute(defAttrs: OrgnlExtObjAttrs, noocc: Option[Boolean], _args:Arguments) extends defRefAttrs {
  override def globalKind = Utils.shortKind(Utils.AttributeKind())
}
case class Arguments(_children:List[MizTerm]) extends ObjectLevel
case class Ancestors(_structTypes:List[Struct_Type]) extends ObjectLevel
case class Loci(_loci:List[Locus]) extends ObjectLevel

/**
 *
 * @param varidkind
 * @param locVarAttr
 * @param spelling
 * @param kind
 * @precondition whenever spelling is empty, varidkind="It" and we are within a pattern in a structure patterns rendering,
 * in this case we only parse the given notation of the parent pattern and will not try to translate this
 */
case class Locus(varidkind:String, locVarAttr:LocalRedVarAttr, spelling:Option[String], kind:String) extends ObjectLevel {
  def toIdentifier: LocalName = MizarVariableName(spelling getOrElse "It", kind, locVarAttr.serialNrIdNr, locVarAttr.varnr)
}
case class Field_Segments(_fieldSegments:List[Field_Segment]) extends ObjectLevel
case class Field_Segment(pos:Position, _selectors:Selectors, _tp:Type) extends ObjectLevel
case class Selectors(_loci:List[Selector]) extends ObjectLevel
case class Selector(pos: Position, nr: Int, spelling:String, _loci:Locus) extends ObjectLevel
case class Structure_Patterns_Rendering(_aggrFuncPat:AggregateFunctor_Pattern, _forgetfulFuncPat:ForgetfulFunctor_Pattern, _strFuncPat:Strict_Pattern, _selList:Selectors_List) extends ObjectLevel
case class Selectors_List(_children:List[SelectorFunctor_Pattern]) extends ObjectLevel

/**
 * There are two kinds of Properties tags in Mizar esx files (unfortunately of same name):
 * 1) within Property-Registrations
 * 2) within definitions
 * In the first case the parameters sort (which within the entire MML is always sethood) and _tp are given
 * In the second case exactly property and _cond (a proof of it) are given
 * @param sort
 * @param property
 * @param _cond
 * @param _tp
 */
case class Properties(sort: Option[String], property:Option[String], _cond:List[Properties], _tp:Option[Type]) extends ObjectLevel {
  def matchProperty(_just: Option[Justification] = None ): Option[Utils.MizarProperty] = {
    property map (Utils.matchProperty(_, _just))
  }
}
case class Redefine(occurs:Boolean)
case class Type_Specification(_types:Type) extends ObjectLevel
case class Definiens(pos:Position, kind:String, shape:String, _label:Label, _expr:CaseBasedExpr) extends ObjectLevel
case class Label(spelling:String, pos:Position, serialnr:SerialNrIdNr, labelnr:Int) extends ObjectLevel
case class Restriction(_formula:Formula) extends ObjectLevel
case class Right_Circumflex_Symbol(position: Position, spelling:String) extends ObjectLevel
case class Equating(_var:Variable, _tm:MizTerm) extends ObjectLevel
case class Loci_Equalities(_lociEqns:List[Loci_Equality]) extends ObjectLevel
case class Loci_Equality(pos:Position, _frstLocus:Locus, _sndLocus:Locus) extends ObjectLevel
case class Equalities_List(_eqns:List[EqualityTr]) extends ObjectLevel
case class Iterative_Step(pos:Position, _tm:MizTerm, _just:Justification) extends ObjectLevel
case class Type_List(_tps:List[Type]) extends ObjectLevel
case class Provisional_Formulas(_props:List[Proposition]) extends ObjectLevel
case class Partial_Definiens_List(_partDef:List[Partial_Definiens]) extends ObjectLevel

/**
 * A case of a case-based-definien
 * @param _expr The definien of the case
 * @param _form The condition of the case
 */
case class Partial_Definiens(_expr:Expression, _form:Formula) extends ObjectLevel
case class Otherwise(_expr:Option[Expression]) extends ObjectLevel

case class According_Expansion(_attrs:List[Attribute]) extends ObjectLevel

sealed trait Pragmas extends ObjectLevel
case class Unknown(pos:Position, inscription:String) extends Pragmas
case class Notion_Name(pos:Position, inscription:String) extends Pragmas
case class Canceled(MmlId:MMLId, amount:Int, kind:String, position:Position) extends Pragmas

case class Reservation_Segment(pos: Position, _vars:Variables, _varSegm:Variable_Segments, _tp:Type) extends ObjectLevel

sealed trait Segments extends ObjectLevel {
  def _vars: Variables
  def _tpList: Type_List
  def _tpSpec: Option[Type_Specification]
}
case class Functor_Segment(pos:Position, _vars:Variables, _tpList:Type_List, _tpSpec:Option[Type_Specification]) extends Segments
case class Predicate_Segment(pos:Position, _vars:Variables, _tpList:Type_List) extends Segments {
  override def _tpSpec: Option[Type_Specification] = None
}