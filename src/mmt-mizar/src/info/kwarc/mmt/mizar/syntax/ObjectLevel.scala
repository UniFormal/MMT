package info.kwarc.mmt.mizar.syntax

import info.kwarc.mmt.api.parser.SourcePosition
import info.kwarc.mmt.mizar.syntax.Utils._
import info.kwarc.mmt.mizar.translator.TranslationController

/**
 * trait for any object level mizar content, e.g. expressions, claims, patterns and variables
 */
sealed trait ObjectLevel
/**
 * a claim in mizar, e.g. a formula
 */
sealed trait Claim extends ObjectLevel
/**
 * Any claim that is not a type_changing_claim
 */
sealed trait TypeUnchangingClaim extends Claim
/**
 * A segment of variable segments, allows binding several variables of different types
 */
sealed trait ContextSegments extends ObjectLevel {
  def _children: List[VariableSegments]
  def variables: List[(List[Variable], Type)] = _children map (child => (child._vars(), child._tp()))
}
/**
 * trait for various kinds of variable segments
 * used in binders to bind new variables
 */
sealed trait VariableSegments extends ObjectLevel {
  def _tp(): Type
  def _vars() : List[Variable]
}
/**
 * A variable segment binding a single new free variable
 * @param _var the variable
 * @param _tp its type
 */
case class Free_Variable_Segment(_var:Variable, _tp:Type) extends VariableSegments {
  override def _vars(): List[Variable] = List(_var)
}
/**
 * A variable segment binding a single variable with already reserved type
 * @param _var
 * @param _tp
 */
case class Implicitly_Qualified_Segment(_var:Variable, _tp:ReservedDscr_Type) extends VariableSegments {
  override def _vars(): List[Variable] = List(_var)
}
/**
 * A variables segment binding a number of variables of same type
 * @param _variables the variables to be bound
 * @param _tp their type
 */
case class Explicitly_Qualified_Segment(_variables:Variables, _tp:Type) extends VariableSegments {
  def _vars() = {_variables._vars}
}
/**
 * A segment of variable segments, allows binding several variables of different types
 * @param _children the contatined variable segments
 */
case class Qualified_Segments(_children:List[VariableSegments]) extends ContextSegments
/**
 * A segment containing several variable segments
 * @param _children
 */
case class Variable_Segments(_children:List[VariableSegments]) extends ContextSegments

/**
 * An expression (type, term or formula) in mizar
 */
sealed trait Expression extends ObjectLevel {
  def ThisType() = this.getClass.toString
}
/**
 * trait for types in mizar
 */
sealed trait Type extends Expression
/**
 * refers to the type of a term whoose type is declared earlier
 * @param serialnr
 * @param nr
 * @param sort
 * @param _subs
 * @param _tp
 */
case class ReservedDscr_Type(_subs:Substitutions, _tp:Type) extends Type
/**
 * predicate subtypes
 * @param srt
 * @param pos
 * @param _adjClust
 * @param _tp
 */
case class Clustered_Type(_adjClust:Adjective_Cluster, _tp:Type) extends Type
sealed trait objRefAttrs extends GloballyReferencingObjAttrs with ObjectLevel {
  def objAttrs: ObjectAttrs
  def globalObjAttrs: GlobalObjAttrs
}
/**
 * Any noun
 * A (standard) type is called expandable iff it is explicitely defined
 * @param objAttrs
 * @param globalObjAttrs
 * @param _args
 */
case class Standard_Type(objAttrs: ObjectAttrs, globalObjAttrs: GlobalObjAttrs, _args:Arguments) extends Type with objRefAttrs {
  override def globalKind = Utils.shortKind(Utils.ModeKind())
}
/**
 * the type of a structure (applied to the arguments in _args)
 * @param tpAttrs referencing the (type definitino of the) structure
 * @param _args the arguments
 */
case class Struct_Type(tpAttrs:ConstrObjAttrs, _args:Arguments) extends Type with GloballyReferencingDefAttrs {
  override def globalKind = Utils.shortKind(Utils.StructureKind())
  override def globalDefAttrs: GlobalDefAttrs = tpAttrs.globalDefAttrs
}

/**
 * Terms in mizar
 * Named MizTerm to avoid name-clashes with api.objects.Term in mmt
 */
sealed trait MizTerm extends Expression {
  def sort : String
}
/**
 * A constant term
 * usually local within some block (e.g. an argument to a definition)
 * @param serialnr the serialnr of the term
 * @param spelling the spelling of the term (name in case of a variable)
 * @param sort the sort of the term,
 */
case class Simple_Term(serialnr: Int, spelling:String, sort:String) extends MizTerm {
 def toIdentifier = MizarVariableName(spelling, sort, serialnr)
}

/**
 * In Mizar Terminology a complex term is any Expression,
 * here we mean expressions which are terms (objects in mizar terminology)
 */
sealed trait ComplexTerm extends MizTerm
/**
 * Convenience trait defining the sort as to be contained in the ConstrObjAttrs
 */
sealed trait ObjAttrsComplexTerm extends ComplexTerm {
  def objAttr : RedObjAttr
  override def sort: String = objAttr.sort
}
/**
 * a complex term globally referencing a definition
 */
sealed trait globalDefAttrsComplexTerm extends ComplexTerm with GloballyReferencingDefAttrs {
  def objAttr: ReferencingConstrObjAttrs
  override def sort = objAttr.sort
  override def globalDefAttrs = objAttr.globalDefAttrs
}
/**
 * introduction form of a structure in mizar
 * the arguments (arguments and field values of the selectors) to the structure are specified
 * in the _args child
 * @param objAttr
 * @param _args
 */
case class Aggregate_Term(objAttr:ConstrObjAttrs, _args:Arguments) extends globalDefAttrsComplexTerm {
  override def globalKind = Utils.shortKind(Utils.AggregateKind())
}
/**
 * result of applying a selector (specified as attributes) to a term _arg,
 * whose tp is a structure including this selector
 * @param objAttr
 * @param _arg
 */
case class Selector_Term(objAttr:ConstrObjAttrs, _arg:MizTerm) extends globalDefAttrsComplexTerm {
  override def globalKind = Utils.shortKind(Utils.SelectorKind())
}
/**
 * an expression containing an circumfix operator -> an OMA in MMT Terminology,
 * as well as arguments to apply the operator to
 * spelling contains the left delimiter, right circumflex symbol the right delimiter
 * @param objAttr references the function to apply
 * @param _symbol contains the right delimiter
 * @param _args the arguments to apply the function to
 */
case class Circumfix_Term(objAttr:ReDefObjAttrs, _symbol:Right_Circumflex_Symbol, _args:Arguments) extends globalDefAttrsComplexTerm {
  override def globalKind = Utils.shortKind(Utils.FunctorKind())
}
/**
 * An integer value, the value is stored in the attribute number
 * @param number
 * @param sort
 */
case class Numeral_Term(number:Int, sort:String) extends MizTerm
/**
 * Corresponds to the it in an implicit definition for functors and modes
 * @param sort
 */
case class it_Term(sort: String) extends MizTerm
/**
 * Refers to a selector term (by its nr) of a selector already defined within a
 * definition of a new mizar structure
 * @param objAttr
 */
case class Internal_Selector_Term(objAttr: RedObjAttr) extends ObjAttrsComplexTerm
/**
 * an expression containing an infix operator and arguments to apply it to
 * @param objAttr
 * @param infixedArgs
 */
case class Infix_Term(objAttr:ReDefObjAttrs, infixedArgs: InfixedArgs) extends globalDefAttrsComplexTerm {
  override def globalKind = Utils.shortKind(Utils.FunctorKind())
}
/**
 * generated by
 * the tp
 * it gives us an unchanging globally unique non-fixed element of tp
 *
 * @param sort
 * @param _tp
 */
case class Global_Choice_Term(sort: String, _tp:Type) extends ComplexTerm
/**
 * reference to the objAttr.nr-th argument in a local definition
 * @param objAttr
 */
case class Placeholder_Term(objAttr: RedObjAttr) extends ObjAttrsComplexTerm
/**
 * corresponds to a deffunc
 * functor definition that is local within a proof or definition block
 * @param objAttr
 * @param serialnr
 * @param _args
 */
case class Private_Functor_Term(objAttr: RedObjAttr, serialnr: Int, _args:Arguments) extends ObjAttrsComplexTerm
/**
 * invoking specification axiom for sets
 * generated by
 * {tm where a, b,... is Element of bigger_universe : form }
 * if we have no contradition for form, we can also write equivalently
 * the set of all tm where a, b, ... is Element of bigger_universe
 * which generates a simple fraenkel term
 *
 * @param sort
 * @param _varSegms
 * @param _tm
 * @param _form
 */
case class Fraenkel_Term(sort: String, _varSegms:Variable_Segments, _tm:MizTerm, _form:Formula) extends ComplexTerm
/**
 * invoking specification axiom for sets
 * generated by
 * the set of all formula where a, b, ... is Element of bigger_universe
 * @param sort
 * @param _varSegms
 * @param _tm
 */
case class Simple_Fraenkel_Term(sort: String, _varSegms:Variable_Segments, _tm:MizTerm) extends ComplexTerm
/**
 * generated by
 * term qua tp
 * checks whether term has type tp, otherwise gives 116 error
 * if it does it returns the term as an element of the tp
 *
 * if mizar can't proof it itself, one can use reconsider x = tm as tp by prf instead
 * and provide a proof (but no proof block allowed here)
 * @param sort
 * @param _tm
 * @param _tp
 */
case class Qualification_Term(sort: String, _tm:MizTerm, _tp:Type) extends ComplexTerm
/**
 * given structure T inheriting from structure S, t instance of structure T
 * writing in Mizar
 * the S of t
 * returns an instance of S with same selectors and arguments of the corresponding arguments and selectors of t
 * @param objAttr references the structure type declaration of the S
 * @param _tm the instance t
 */
case class Forgetful_Functor_Term(objAttr: ConstrObjAttrs, _tm:MizTerm) extends globalDefAttrsComplexTerm {
  override def globalKind: Char = Utils.shortKind(Utils.ForgetfulKind())
}

/**
 * Formulae in mizar, specifically primitive FOL formulae, relational formulae, Multi_Attributive_Formulae and Qualifying_Formulae
 */
sealed trait Formula extends TypeUnchangingClaim with Expression
/**
 * The body of a quantifier formula
 * @param _expr the content of said body
 */
case class Scope(_expr: Claim) extends Formula
/**
 * Existentially quantify the expression _body over the variables contained in _vars
 * @param _vars the variables
 * @param _restrict (optional) further conditions on the variables whose existence is asserted here
 * @param _body the body of the formula
 */
case class Existential_Quantifier_Formula(_vars:Variable_Segments, _restrict:Option[Restriction], _body: Claim) extends Formula
/**
 * Applying a relation to its arguments (and possibly negating the result)
 * @param defAttrs
 * @param infixedArgs
 */
case class Relation_Formula(defAttrs: ReDefObjAttrs, antonymic:Option[Boolean], infixedArgs: InfixedArgs) extends Formula with defRefAttrs {
  override def globalKind = Utils.shortKind(Utils.PredicateKind())
}
/**
 * Universally quantify the expression _body over the variables contained in _vars
 * @param _vars
 * @param _restrict further assumptions on the variable quantified over
 * @param _body the body of the formula
 */
case class Universal_Quantifier_Formula(_vars:Variable_Segments, _restrict:Option[Restriction], _body:Claim) extends Formula
/**
 * generated by
 * is adjective1, adjective2, ...
 * assets that some attributes hold on a term
 * @param _tm
 * @param _cluster
 */
case class Multi_Attributive_Formula(_tm:MizTerm, _cluster:Adjective_Cluster) extends Formula
/**
 * implication
 * @param _assumption
 * @param _conclusion
 */
case class Conditional_Formula(_assumption:Claim, _conclusion:Claim) extends Formula
/**
 * and
 * @param _frstConjunct
 * @param _sndConjunct
 */
case class Conjunctive_Formula(_frstConjunct:Claim, _sndConjunct:Claim) extends Formula
/**
 * iff
 * @param _frstFormula
 * @param _sndFormula
 */
case class Biconditional_Formula(_frstFormula:Claim, _sndFormula:Claim) extends Formula
/**
 * or
 * @param _frstDisjunct
 * @param _sndDisjunct
 */
case class Disjunctive_Formula(_frstDisjunct: Claim, _sndDisjunct: Claim) extends Formula
/**
 * negation
 * @param _formula
 */
case class Negated_Formula(_formula:Claim) extends Formula
/**
an contradiction object
 */
case class Contradiction() extends Formula
/**
 * generated by
 * is tp
 * Asserts that _tm has type _tp
 * @param _tm
 * @param _tp
 */
case class Qualifying_Formula(_tm:MizTerm, _tp:Type) extends Formula
/**
 * corresponds to a defpred
 * a predicate definition that is local within a proof or definition block
 * @param redObjAttr
 * @param serialnr
 * @param _args
 */
case class Private_Predicate_Formula(redObjAttr:RedObjAttr, serialnr: Int, _args:Arguments) extends Formula
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
 * @param _formulae
 */
case class FlexaryDisjunctive_Formula(_formulae:List[Claim]) extends Formula
/**
 * A conjunctive formula form_1 & ... & form_n
 * it gets elaborated by mizar to either a (expanded) conjunctive formula or
 * a universal quantification formula stating that for some arguments of such shape (in below case in the integer range)
 * the corresponding disjunct holds
 * @param _formulae
 */
case class FlexaryConjunctive_Formula(_formulae:List[Claim]) extends Formula
/**
 * Apply relations <_i in a sequence to a list of arguments a_1, ..., a_m+1 leading to
 * a_1 <_1 a_2 <_2 a_3 <_3 ... <_m a_m+1
 * @param _relForm the relation formula a_1 <_1 a_2
 * @param _rhsOfRFs contains the remaining relations <_i applied to their arguments a_i and a_i+1
 */
case class Multi_Relation_Formula(_relForm:Relation_Formula, _rhsOfRFs:List[RightSideOf_Relation_Formula]) extends Formula
/**
 * Applying a relation to its arguments (and possibly negating the result), like a relation formula
 * can only occur within a Multi_Relation_Formula
 * @param objAttr
 * @param infixedArgs
 */
case class RightSideOf_Relation_Formula(defAttrs: ReDefObjAttrs, antonymic:Option[Boolean], infixedArgs: InfixedArgs) extends Formula with defRefAttrs {
  override def globalKind = Utils.shortKind(Utils.PredicateKind())
}

/**
 * A labelled claim
 * within proofs the labels might be deferred to with local references
 * immediately after Therorem, ...
 * @param _label
 * @param _thesis
 */
case class Proposition(_label:Label, _thesis:Claim) extends TypeUnchangingClaim {
  def referenceableLabel = Utils.makeNewSimpleGlobalName(_label.spelling)
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
case class Thesis() extends TypeUnchangingClaim
/** corresponds to a  now ... end block
 * makes the statements inside known to mizar, even if unproven or even false
 * this is often convenient, although never necessary
 */
case class Diffuse_Statement(_label:Label) extends TypeUnchangingClaim
/**
 * Making several claims at once, semantically this is equivalent to a conjunction
 * this is used especially within assumptions or choice_statements
 * @param _props
 */
case class Conditions(_props:List[Proposition]) extends TypeUnchangingClaim
/**
 * a claim of the form a_1 = a_2 = a_3 = ...,
 * where each of the eaualities has its own proof
 * @param _label a label with which this claim can be referenced
 * @param _formula the first equality
 * @param _just the proof for the first equality
 * @param _iterSteps the remaining equalities and their proofs
 */
case class Iterative_Equality(_label:Label, _formula:Relation_Formula, _just:Justification, _iterSteps: Iterative_Steps_List) extends TypeUnchangingClaim
/**
 * a list of one or several iterative steps
 * @param _iterSteps
 */
case class Iterative_Steps_List(_iterSteps:List[Iterative_Step]) extends ObjectLevel
/**
 * one additional equality with proof within an iterative equality
 * @param _tm
 * @param _just
 */
case class Iterative_Step(_tm:MizTerm, _just:Justification) extends ObjectLevel
/**
 * The claim of a type-changing statement
 * this is a utility class and not part of the xml
 * @param _eqList
 * @param _tp
 */
private[mizar] case class Type_Changing_Claim(_eqList:Equalities_List, _tp:Type) extends Claim

/**
 * A justification for a claim
 */
sealed trait Justification extends ObjectLevel
/**
 * The proof is obvious using the referenced facts
 * @param _refs
 */
case class Straightforward_Justification(_refs:List[Reference]) extends Justification
/**
 * A block, a self-contained context containing items with all kinds of content
 * there are several kinds of blocks, in particular
 * definition, registration and notation blocks, case blocks, scheme blocks and proof blocks
 * @param kind
 * @param _items
 */
case class Block(pos: Positions, kind: String, _items:List[Item]) extends Justification
/**
 * A scheme, containing a nr allowing to reference it by its name withing the same article
 * @param spelling the spelling of the scheme
 * @param nr the number to reference the scheme by
 */
case class Scheme(spelling:Option[String], nr:Int) extends ObjectLevel {
  def name = globalName.name.toString
  def globalNameO = spelling map (makeNewGlobalName("Scheme", _))
  def globalName = globalNameO getOrElse(makeNewGlobalName("AnonymousScheme", nr.toString))
}
/**
 * A Justification using a referenced scheme applied to some referenced facts
 * @param nr
 * @param idnr
 * @param schnr
 * @param spelling
 * @param _refs
 */
case class Scheme_Justification(nr: Int, idnr:Int, schnr:Int, spelling:String, _refs:List[Reference]) extends Justification {
  def referencedScheme = {
    val localReference = makeNewGlobalName("Scheme", spelling)
    val locallyReferenced = TranslationController.locallyDeclared(localReference)
    val referencedAid = if (locallyReferenced) TranslationController.currentAid else spelling
    MMLId(referencedAid+":"+idnr).globalName("Scheme")
  }
}

/** Notations */
sealed trait Patterns extends ObjectLevel with GloballyReferencingObjAttrs {
  def patternAttrs: PatternAttrs
  def _locis: List[Loci]
  def patDef: PatDefs = PatDef(patternAttrs, _locis)
  def patKind: Utils.PatternKinds

  override def globalKind: Char = Utils.shortKind(patKind)
  override def globalObjAttrs: GlobalObjAttrs = patDef.globalObjAttrs
}
/**
 * Used to reference a definition
 * it contains a pattern referencing it
 * @param _pat
 */
case class Pattern_Shaped_Expression(_pat: Patterns) extends ObjectLevel
/**
 * Patterns for mode definitions
 * @param patternAttrs
 * @param _locis
 */
case class Mode_Pattern(patternAttrs: PatternAttrs, _locis: List[Loci]) extends Patterns {
  override def patKind: Utils.DeclarationKinds = Utils.ModeKind()
}
/**
 * Patterns for definitions that define both a pattern and a constr
 */
sealed trait ConstrPattern extends Patterns with GloballyReferencingDefAttrs {
  def constrPatDef: ConstrPatDef
  def _locis:List[Loci] = constrPatDef._locis
  def constrPatAttr: ConstrPatAttr = constrPatDef.constrPatAttr
  override def patternAttrs: PatternAttrs = constrPatDef.constrPatAttr.patAttr
  override def globalDefAttrs: GlobalDefAttrs = constrPatAttr.globalDefAttrs
}
/**
 * Patterns for redefinable definitions defining both pattern and constr
 */
sealed trait RedefinablePatterns extends ConstrPattern with GloballyReferencingReDefAttrs {
  def redefPatDef: RedefinablePatDef
  def _loci: List[Locus] = redefPatDef._loci
  def redefPatAttr: RedefinablePatAttr = redefPatDef.redefPatAttr
  override def _locis: List[Loci] = redefPatDef._locis
  override def constrPatDef: ConstrPatDef = ConstrPatDef(redefPatAttr.constrPatAttr, _locis)
  override def globalReDefAttrs = redefPatAttr.globalReDefAttrs
  def hasOrigRefs = globalReDefAttrs.hasOrigRefs
  override def patKind: DeclarationKinds
}
/**
 * a pattern of a structure definition
 * @param constrPatAttr
 * @param _locis
 */
case class Structure_Pattern(constrPatDef: ConstrPatDef) extends ConstrPattern {
  override def patKind = Utils.StructureKind()
}
/**
 * a pattern of an attribute definition
 * @param redefPatAttr
 * @param _loci
 * @param _locis
 */
case class Attribute_Pattern(redefPatDef: RedefinablePatDef) extends RedefinablePatterns {
  override def patKind = Utils.AttributeKind()
}
/**
 * a pattern of a predicate definition
 * @param redefPatAttr
 * @param _loci
 * @param _locis
 */
case class Predicate_Pattern(redefPatDef: RedefinablePatDef) extends RedefinablePatterns {
  override def patKind = Utils.PredicateKind()
}
/**
 * provides notation and pattern to reference, for a strict declaration of a structure definition
 * @param redefPatAttr
 * @param _loci
 * @param _locis
 */
case class Strict_Pattern(redefPatDef: RedefinablePatDef) extends ConstrPattern {
  override def patKind: Utils.PatternKinds = Utils.StrictKind()
  override def constrPatDef: ConstrPatDef = ConstrPatDef(redefPatDef.redefPatAttr.constrPatAttr, redefPatDef._locis)
}
/**
 * ConstrPatterns of functor definitions and functor like declarations of structure definitions
 */
sealed trait Functor_Patterns extends Patterns with ConstrPattern
/**
 * a pattern of the introduction declaration of a structure
 * @param constrPatAttr
 * @param _locis
 */
case class AggregateFunctor_Pattern(constrPatDef: ConstrPatDef) extends Functor_Patterns {
  override def patKind: Utils.StructurePatternKinds = Utils.AggregateKind()
}
/**
 * pattern of the (forgetful) projection from a structure to a (not necessarily proper) substructure
 * @param constrPatAttr
 * @param _locis
 */
case class ForgetfulFunctor_Pattern(constrPatDef: ConstrPatDef) extends Functor_Patterns {
  override def patKind: Utils.StructurePatternKinds = Utils.ForgetfulKind()
}
/**
 * pattern of a selector (field) declaration of a structure
 * @param constrPatAttr
 * @param _locis
 */
case class SelectorFunctor_Pattern(constrPatDef: ConstrPatDef) extends Functor_Patterns {
  override def patKind: Utils.StructurePatternKinds = Utils.SelectorKind()
}
/**
 * patterns which are both functor patterns and redefinable patterns
 */
sealed trait RedefinableFunctor_Patterns extends Functor_Patterns with RedefinablePatterns {
  override def patKind = Utils.FunctorKind()
}
/**
 * pattern of a functor definition with infix notation
 * @param rightargsbracketed
 * @param redefPatAttr
 * @param _loci
 * @param _locis
 */
case class InfixFunctor_Pattern(rightargsbracketed: Option[Boolean], redefPatDef: RedefinablePatDef) extends RedefinableFunctor_Patterns
/**
 * pattern of a functor definition with circumfix notation
 * @param redefPatAttr
 * @param _right_Circumflex_Symbol
 * @param _loci
 * @param _locis
 */
case class CircumfixFunctor_Pattern(override val redefPatAttr: RedefinablePatAttr, _right_Circumflex_Symbol: Right_Circumflex_Symbol, override val _loci: List[Locus], override val _locis:List[Loci]) extends RedefinableFunctor_Patterns {
  override def redefPatDef: RedefinablePatDef = RedefinablePatDef(redefPatAttr, _loci, _locis)
}

/**
 * a traits for exemplification (existance proofs by giving an example)
 * providing one within a proofs, removes the corresponding existential quantifier from the thesis
 */
sealed trait Exemplifications extends ObjectLevel {
  def _tm: MizTerm
  def varO: Option[Variable] = None
}
/**
 * exemplification, where the existentially quantified variable to remove from the thesis is left implicit
 * @param _tm
 */
case class ImplicitExemplification(_tm:MizTerm) extends Exemplifications
/**
 * exemplification with a simple term
 * @param _var
 * @param _tm
 */
case class ExemplifyingVariable(_var:Variable, _tm:Simple_Term) extends Exemplifications {
  override def varO: Option[Variable] = Some(_var)
}
/**
 * a generic explicit exemplification
 * @param _var
 * @param _tm
 */
case class Example(_var:Variable, _tm:MizTerm) extends Exemplifications {
  override def varO: Option[Variable] = Some(_var)
}

/**
 * common trait for various kinds of (local or global) references
 */
sealed trait Reference extends ObjectLevel {
  def referencedLabel: Object
}
/**
 * a local reference to a proven fact from within the same proof
 * @param spelling
 * @param idnr
 * @param labelnr
 */
case class Local_Reference(spelling:String, idnr: Int, labelnr:Int) extends Reference {
  override def referencedLabel = TranslationController.getLocalPath(spelling)
}
/**
 * I don't really understand the semantics of a link, but they don't seem to be important for translation
 * @param labelnr
 */
case class Link(labelnr:Int) extends Reference {
  override def referencedLabel = ???
}
/**
 * A global (i.e. visible outside of the article in which it is declared) reference
 */
sealed trait GlobalReference extends Reference {
  def number: Int
  def kind: String
  def spelling: String
  override def referencedLabel = TranslationController.getPath(spelling, kind+number.toInt)
  def referencedItem = info.kwarc.mmt.api.objects.OMS(referencedLabel)
}
/**
 * a global definition reference
 * @param nr
 * @param spelling
 * @param number
 */
case class Definition_Reference(nr: Int, spelling:String, number:Int) extends GlobalReference {
  override def kind = "Def"
}
/**
 * global reference to a theorem
 * @param nr
 * @param spelling
 * @param number
 */
case class Theorem_Reference(nr: Int, spelling:String, number:Int) extends GlobalReference {
  override def kind = "Theorem-Item"
}

/**
 * various kinds of definiens in a mode definition
 */
sealed trait Modes extends ObjectLevel
/**
 * corresponds to an expandable type
 * @param _tp
 */
case class Expandable_Mode(_tp:Type) extends Modes
/**
 * Provides definiens using either a type specification or a definiens
 * Always at least of the _tpSpec and _def are defined
 * @param _tpSpec
 * @param _def
 */
case class Standard_Mode(_tpSpec:Option[Type_Specification], _def:Option[Definiens]) extends Modes
/**
 * An equality within a type-changing-claim
 */
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
/**
 * a list of equalities within a type changing statement
 * @param _eqns
 */
case class Equalities_List(_eqns:List[EqualityTr]) extends ObjectLevel

/**
 * A variable to a scheme
 * @param _segms
 */
case class Schematic_Variables(_segms:List[Segments]) extends ObjectLevel
/**
 * Contains several variables
 * @param _vars
 */
case class Variables(_vars:List[Variable]) extends ObjectLevel
/**
 * a single (local) variable
 * @param pos the position in corresponding Mizar source file
 * @param serialnr the number by which the variable (combined with its spelling and kind) can be referenced
 * @param spelling the name of the variable
 * @param kind the kind of the variable
 */
case class Variable(pos: Position, serialnr: Int, spelling:String, kind:String) extends ObjectLevel {
  def toIdentifier = MizarVariableName(spelling, kind, serialnr)
}
case class Substitutions(_childs:List[Substitution]) extends ObjectLevel
case class Substitution(freevarnr:Int, kind:String, varnr:Int) extends ObjectLevel
/**
 * the conjunction of several attributes
 * @param _attrs
 */
case class Adjective_Cluster(_attrs: List[Attribute]) extends ObjectLevel
/**
 * objects referencing redefinable definitions
 */
sealed trait defRefAttrs extends GloballyReferencingReDefAttrs with objRefAttrs {
  def defAttrs: ReDefObjAttrs
  override def objAttrs = ObjectAttrs(defAttrs.spelling, defAttrs.sort)
  override def globalObjAttrs: GlobalObjAttrs = GlobalObjAttrs(defAttrs.globalDefAttrs.absolutepatternMMLId)
  override def globalDefAttrs: GlobalDefAttrs = defAttrs.globalDefAttrs
  override def globalReDefAttrs: GlobalReDefAttrs = defAttrs.globalReDefAttrs
}
/**
 * an attribute, i.e. a reference to an attribute definition
 * @param defAttrs
 * @param noocc
 * @param _args
 */
case class Attribute(defAttrs: ReDefObjAttrs, noocc: Option[Boolean], _args:Arguments) extends defRefAttrs {
  override def globalKind = Utils.shortKind(Utils.AttributeKind())
}
/**
 * a number of arguments (terms) to which to apply a function or predicate
 * @param _children
 */
case class Arguments(_children:List[MizTerm]) extends ObjectLevel
/**
 * the substructures of a structure in mizar
 * @param _structTypes
 */
case class Ancestors(_structTypes:List[Struct_Type]) extends ObjectLevel
/**
 * local arguments
 * @param _loci
 */
case class Loci(_loci:List[Locus]) extends ObjectLevel

/**
 * local arguments
 * @param varidkind
 * @param spelling
 * @param kind
 * @precondition whenever spelling is empty, varidkind="It" and we are within a pattern in a structure patterns rendering,
 * in this case we only parse the given notation of the parent pattern and will not try to translate this
 */
case class Locus(varidkind:String, serialnr: Int, spelling: Option[String], kind:String) extends ObjectLevel {
  def toIdentifier = {
    val spell = if (spelling.nonEmpty) spelling.get else varidkind
    MizarVariableName(spell, kind, serialnr)
  }
}
/**
 * contains the field segments containing the selectors of a structure
 * @param _fieldSegments
 */
case class Field_Segments(_fieldSegments:List[Field_Segment]) extends ObjectLevel
/**
 * a field segment containing selectors of a structure
 * @param _selectors
 * @param _tp
 */
case class Field_Segment(_selectors:Selectors, _tp:Type) extends ObjectLevel
/**
 * several selectors (fields) of a structure
 * @param _loci
 */
case class Selectors(_loci:List[Selector]) extends ObjectLevel
/**
 * a selector of a structure
 * @param nr
 * @param spelling
 * @param _loci
 */
case class Selector(nr: Int, spelling:String, _loci:Locus) extends ObjectLevel
/**
 * contains the patterns giving notations and referenceable labels for the various declarations of the structure
 * @param _aggrFuncPat
 * @param _forgetfulFuncPat
 * @param _strFuncPat
 * @param _selList
 */
case class Structure_Patterns_Rendering(_aggrFuncPat:AggregateFunctor_Pattern, _forgetfulFuncPat:ForgetfulFunctor_Pattern, _strFuncPat:Strict_Pattern, _selList:Selectors_List) extends ObjectLevel
/**
 * contains the SelectorFunctor_Patterns of a structure
 * @param _children
 */
case class Selectors_List(_children:List[SelectorFunctor_Pattern]) extends ObjectLevel
/**
 * There are two kinds of Properties tags in Mizar esx files (unfortunately of same name):
 * 1) within Property-Registrations
 * 2) within definitions
 * In the first case exactly the parameters sort (which within the entire MML is always sethood) and _tp are given
 * In the second case exactly property and _just (a proof of it) are given
 * @param sort (optional)
 * @param property (optional) the property
 * @param _tp
 */
case class Properties(sort: Option[String], property: Option[String], _tp:Option[Type]) extends ObjectLevel {//, _cond:List[Properties]
  def matchProperty(pos: SourcePosition, _just: Option[Justification] = None ) = Utils.matchProperty(pos, property.getOrElse(sort.get), _just, _tp)
}
/**
 * whether a redefinable definition is a redefinition
 * @param occurs
 */
case class Redefine(occurs:Boolean)
/**
 * specifies a type
 * @param _types
 */
case class Type_Specification(_types:Type) extends ObjectLevel
/**
 * the (potentially case-by-case) definien of a definition
 * @param kind
 * @param _label
 * @param _expr
 */
case class Definiens(kind:String, _label:Label, _expr:CaseBasedExpr) extends ObjectLevel
/**
 * provides a label with which an item can be referenced (using the labelnr) withing the same context
 * @param spelling
 * @param idnr
 * @param labelnr
 */
case class Label(spelling:String, idnr: Int, labelnr:Int) extends ObjectLevel
/**
 * Additional conditions for a variable quantified over
 * @param _formula
 */
case class Restriction(_formula:Formula) extends ObjectLevel
/**
 * ther right delimiter of a circumfix notation
 * @param spelling
 */
case class Right_Circumflex_Symbol(spelling:String) extends ObjectLevel
/**
 * an assignment to a variable as part of a constant definition
 * @param _var
 * @param _tm
 */
case class Equating(_var:Variable, _tm:MizTerm) extends ObjectLevel
/**
 * the equality assumptions of an identify
 * @param _lociEqns
 */
case class Loci_Equalities(_lociEqns:List[Loci_Equality]) extends ObjectLevel
/**
 * an equality assumption of an identify
 * @param _frstLocus
 * @param _sndLocus
 */
case class Loci_Equality(_frstLocus:Locus, _sndLocus:Locus) extends ObjectLevel
/**
 * a type list within a predicate or functor segment
 * @param _tps
 */
case class Type_List(_tps:List[Type]) extends ObjectLevel
/**
 * a list of assumptions of a scheme definition
 * @param _props
 */
case class Provisional_Formulas(_props:List[Proposition]) extends ObjectLevel

/**
 * a list of partial definiens (cases in a case-by-case definien)
 * @param _partDef
 */
case class Partial_Definiens_List(_partDef:List[Partial_Definiens]) extends ObjectLevel
/**
 * A case of a case-based-definien
 * @param _expr The definien of the case
 * @param _form The condition of the case
 */
case class Partial_Definiens(_expr:Expression, _form:Formula) extends ObjectLevel
/**
 * the default case in a partial definien
 * @param _expr
 */
case class Otherwise(_expr:Option[Expression]) extends ObjectLevel

/**
 * a (functor or predicate) segment within a scheme definition
 */
sealed trait Segments extends ObjectLevel {
  def _vars: Variables
  def _tpList: Type_List
  def _tpSpec: Option[Type_Specification]
}
/**
 * a functor segment within a scheme definition, defining functor arguments to the scheme
 * @param _vars
 * @param _tpList
 * @param _tpSpec
 */
case class Functor_Segment(_vars:Variables, _tpList:Type_List, _tpSpec:Option[Type_Specification]) extends Segments
/**
 * a predicate segment defining predicate arguments to a scheme
 * @param _vars
 * @param _tpList
 */
case class Predicate_Segment(_vars:Variables, _tpList:Type_List) extends Segments {
  override def _tpSpec: Option[Type_Specification] = None
}

//The following is currently ignored during translation
case class According_Expansion(_attrs:List[Attribute]) extends ObjectLevel

sealed trait Pragmas extends ObjectLevel
case class Unknown(inscription:String) extends Pragmas
case class Notion_Name(inscription:String) extends Pragmas
case class Canceled(MmlId:MMLId, amount:Int, kind:String) extends Pragmas

/**
 * the variable for which to reserve a type and their types, doesn't need to be translated as explained in the comment for Reservations
 * @param _vars
 * @param _varSegm
 * @param _tp
 */
case class Reservation_Segment(_vars:Variables, _varSegm:Variable_Segments, _tp:Type) extends ObjectLevel