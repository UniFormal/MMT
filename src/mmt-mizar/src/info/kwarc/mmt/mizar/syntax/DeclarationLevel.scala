package info.kwarc.mmt.mizar.syntax

import info.kwarc.mmt.api.ImplementationError
import info.kwarc.mmt.api.parser.SourceRegion
import info.kwarc.mmt.mizar.syntax.Utils._
import info.kwarc.mmt.mizar.translator.DeclarationLevelTranslationError

// any subitem of an item, be it TopLevel, DeclarationLevel or ProofLevel
sealed trait Subitem {
  def kind:String = {
    this.getClass.getName
  }
  def shortKind: String = kind.split('.').lastOption.getOrElse(kind).replace('_','-')
}
/**
 * A subitem that can occur on toplevel
 */
sealed trait TopLevel extends Subitem
/**
 * A subitem that can occur on declaration level, i.e. in a block that is not a proof,
 * namely in a definitional-block, a registration-block, a notation-block or a scheme-block
 */
sealed trait DeclarationLevel extends Subitem
/**
 * A subitem that can occur on either top or declaration level
 */
sealed trait TopOrDeclarationLevel extends TopLevel with DeclarationLevel
/**
 * A subitem that can occur on proof level
 */
sealed trait ProofLevel extends Subitem
/**
 * A subitem with an mmlid, so it can be globally referenced
 */
sealed trait MMLIdSubitem extends TopLevel {
  def mmlId: MMLId
  def globalName = mmlId.globalName(this.shortKind)
}
/**
 * A main subitem that can occur within a definitional-block, registration-block or notation-block (inside a definition-item)
 * main subitems are what needs to be translated and will later be visible to the outside, everything else in such a block
 * declares arguments for such main items, proofs properties to be used for them or gives properties or correctness conditions
 *
 * the main subitems in definitional-blocks are Definitions, the main subitems in registration-blocks are RegistrationSubitems (either clusters (containing several) or single registrations)
 * the main items in notation-blocks are nyms
 */
sealed trait BlockSubitem extends DeclarationLevel
/**
 * A subitem that can occur within a registration-block
 */
sealed trait RegistrationSubitems extends BlockSubitem
/**
 * An actual registration
 */
sealed trait Registrations extends RegistrationSubitems
/**
 * A definition
 */
sealed trait Definition extends BlockSubitem {
  def pat: Option[Patterns]
}
/**
 * A Definition which is not a private definition and consequently has a pattern
 */
sealed trait PublicDefinition extends Definition {
  def _pat: Patterns
  override def pat: Option[Patterns] = Some(_pat)
}
/**
 * A local (functor or predicate) definition
 */
sealed trait PrivateDefinition extends Definition {
  override def pat: Option[Patterns] = None
}
/**
 * A subitem containing a definitional, registration or notation block
 * @param _block the definition, registration or notation block
 */
case class Definition_Item(_block:Block) extends TopLevel {
  //verify assumptions for translation
  def blockKind(): String = {
    val kind = _block.kind
    if (! allowedKinds.contains(kind)) {
      throw new DeclarationLevelTranslationError("Expected a definition item of one of the kinds: "+allowedKinds+
        "\nBut found: "+kind, this)
    }
    kind
  }
  def allowedKinds = List("Definitional-Block", "Registration-Block", "Notation-Block")
}
/**
 * Starts a new section in the article
 */
case class Section_Pragma() extends TopLevel

/**
 * Argument list in the beginning of a definitional-, notation- or registration-block
 * @param _qualSegms contains the variables
 * @param _conds assumptions on the variables
 */
case class Loci_Declaration(_qualSegms:Qualified_Segments, _conds:Option[Conditions]) extends DeclarationLevel
/**
 * Contains one or several assumptions
 * @param _ass
 */
case class Assumption(_ass:Assumptions) extends DeclarationLevel with ProofLevel
/**
 * A common trait for different kinds of assumptions
 */
sealed trait Assumptions extends DeclarationLevel with ProofLevel
/**
 * A single generic assumption
 * @param _prop the claim of the assumption
 */
case class Single_Assumption(_prop:Proposition) extends Assumptions
/**
 * Several assumptions
 * @param _cond contains the assumption claims
 */
case class Collective_Assumption(_cond:Conditions) extends Assumptions
/**
 * Assumption that variables satisfying sertain conditions exist
 * @param _qualSegm contains the variables
 * @param _cond the conditions
 */
case class Existential_Assumption(_qualSegm:Qualified_Segments, _cond:Conditions) extends Assumptions
/**
 * a subitem containing one or several assumptions
 * @param _ass the assumption(s)
 */
case class Suppose_Head(_ass:Assumptions) extends ProofLevel with Assumptions

/**
 * Definitions which have a label and may get redefined
 * @precondition _def.isEmpty => redefinition
 */
sealed trait RedefinableLabeledDefinition extends PublicDefinition with MMLIdSubitem {
  def mmlIdO: Option[MMLId]
  def _redef: Redefine
  def _pat: RedefinablePatterns
  def _def: Option[Definiens]
  def redefinition: Boolean = _redef.occurs
  def check: Unit = {
    if (! (_def.isDefined || redefinition)) {
      throw ImplementationError ("Wrong parsing assumption. ")
    }
  }

  override def mmlId: MMLId = mmlIdO.get
  def defKind: Char = this match {
    case ad: Attribute_Definition => Utils.shortKind(Utils.AttributeKind())
    case fd: Functor_Definition => Utils.shortKind(Utils.FunctorKind())
    case pd: Predicate_Definition => Utils.shortKind(Utils.PredicateKind())
  }
  override def globalName = {
    val Array(aid, ln) = mmlId.MMLId.split(":")
    makeGlobalKindName(aid, defKind, ln)
  }
}
/**
 * An attribute definition
 * @param mmlIdO (optional) the mmlid to reference this attribute definition by
 * @param _redef whether the definition is a redefinition
 * @param _pat the attribute pattern giving name and notation
 * @param _def (optional) the definien
 */
case class Attribute_Definition(mmlIdO:Option[MMLId], _redef:Redefine, _pat:Attribute_Pattern, _def:Option[Definiens]) extends RedefinableLabeledDefinition
/**
 * A functor definition
 * @param mmlIdO (optional) the mmlId to reference this functor definition by
 * @param _redef whether this is a redefinition
 * @param _pat the functor pattern giving name and notation of the newly defined functor
 * @param _tpSpec (optional) the return type of the functor
 * @param _def (optional) the definien
 * @precondition unless _redef.occurs both _tpSpec and _def are given
 */
case class Functor_Definition(mmlIdO:Option[MMLId], _redef:Redefine, _pat:RedefinableFunctor_Patterns, _tpSpec:Option[Type_Specification], _def:Option[Definiens]) extends RedefinableLabeledDefinition
/**
 * A predicate definition
 * @param mmlIdO (optional) the mmlid to reference this predicate definition by
 * @param _redef whether this is a redefinition
 * @param _pat the predicate pattern giving name and notation
 * @param _def (optional) the definien
 */
case class Predicate_Definition(mmlIdO:Option[MMLId], _redef:Redefine, _pat:Predicate_Pattern, _def:Option[Definiens]) extends RedefinableLabeledDefinition
/**
 * A structure definition, takes ancestors (a list of structures it inherits from),
 * structure-Pattern contains several loci, corresponds to the universes the structure is over,
 * a list of field segments, which first needs to repeat the fieldsSegments of the inherited structures
 * then the new ones
 * a field segment is a list of selectors of the same type
 * a selector is a field of the structure (or an inherited one)
 * finally there is a Structure_Patterns_Rendering defining the new notations (namely introducing the selectors)
 * @param _ancestors a list of structures (whoose selectors') to import
 * @param _pat the structure pattern
 * @param _fieldSegms a list of field segments containing the selectors (fields) of the structure
 * @param _rendering contains patterns with notations for the selectors, the restriction
 */
case class Structure_Definition(_ancestors:Ancestors, _pat:Structure_Pattern, _fieldSegms:Field_Segments, _rendering:Structure_Patterns_Rendering) extends PublicDefinition
/**
 * A mode definition
 * @param _redef whether it is a redefinition
 * @param _pat the mode pattern giving name and notation
 * @param _mode the definien
 */
case class Mode_Definition(_redef:Redefine, _pat:Mode_Pattern, _mode:Modes) extends PublicDefinition
/**
 * Local definition of a constant variable
 * @param _children the variable and its value
 */
case class Constant_Definition(_children:List[Equating]) extends PrivateDefinition
/**
 * Corresponds to a deffunc definition, its uses are private_functor_terms
 * used as shortcut and visible within its block
 * @param _var its variable
 * @param _tpList contains the type of the variable
 * @param _tm the definien
 */
case class Private_Functor_Definition(_var:Variable, _tpList:Type_List, _tm:MizTerm) extends PrivateDefinition
/**
 * Corresponds to a defpred definition, its uses are private_functor_terms, it is used as shortcut and visible within its block
 * @param _var its variable
 * @param _tpList contains the type of the variable
 * @param _form the definien
 */
case class Private_Predicate_Definition(_var:Variable, _tpList:Type_List, _form:Formula) extends PrivateDefinition
/**
 * Common trait for correctness conditions and properties following a declaration
 */
abstract class Property_or_Correctness_Condition(is_correctness_condition: Boolean) extends DeclarationLevel
/**
 * All correctness conditions for the preceding definition or registration
 * @param _correctnessCond the correctness conditions
 * @param _just the justification for them
 */
case class Correctness(_correctnessCond:Correctness_Conditions, _just:Justification) extends Property_or_Correctness_Condition(true)
/**
 * Several correctness conditions (which need to contain their own proofs)
 * @param _cond the correctness conditions
 */
case class Correctness_Conditions(_cond:List[CorrectnessConditions]) extends Property_or_Correctness_Condition(true)
/**
 * A single correctness condition with its justification
 * @param _cond the correctness condition
 * @param _just its justification
 */
case class Correctness_Condition(_cond:CorrectnessConditions, _just:Option[Justification]) extends Property_or_Correctness_Condition(true)
/**
 * Well-definedness conditions that need to be proven along with definitions
 */
sealed abstract class CorrectnessConditions(_sort: String) extends DeclarationLevel {
  def sort = _sort
}
/**
 * Non-emptyness of non-expandable types (modes) or clustered_types in registrations of attributes, as well as existence for implicit functor definitions
 */
case class existence() extends CorrectnessConditions("existence")
/**
 * uniqueness for functors
 */
case class uniqueness() extends CorrectnessConditions("uniqueness")
/**
 * can define functor using means or equals
 * if defined using equals need coherence to correct type
 */
case class coherence() extends CorrectnessConditions("coherence")
/**
 * States that a the term and its subterm in a reduction are actually equal
 */
case class reducibility() extends CorrectnessConditions("reducibility")
/**
 * States compatibility of definiens, can occur only in a redefinition or an identify
 */
case class compatibility() extends CorrectnessConditions("compatibility")
/**
 * for overlap of case-by-case definiens
 */
case class consistency() extends CorrectnessConditions("consistency")
/**
 * A property is an additional claim about a proceeding definition that is shown along with said definition
 * @param _props the property
 * @param _just
 */
case class Property(_props:Properties, _just:Option[Justification]) extends Property_or_Correctness_Condition(false) {
  def matchProperty(pos: Position) : MizarProperty = _props.matchProperty(pos.parsePosition(), _just)
}

/**
 * Common trait for both synonyms and antonyms
 */
abstract class Nyms(isAntonym: Boolean) extends BlockSubitem {
  def reg: Positions

  def _patNew: Patterns
  def _patRefOld: Pattern_Shaped_Expression
  def antonymic: Boolean = isAntonym
  def nymKind = _patNew.patKind
}
/**
 * A synonym (a new notation for an existing notion)
 */
abstract class Synonym extends Nyms(false)
/**
 * An antonym (a new notation for the negation of an existing notion)
 */
abstract class Antonym extends Nyms(true)
/**
 * A predicate synonym
 */
case class Pred_Synonym(reg: Positions, _patNew:Predicate_Pattern, _patRefOld: Pattern_Shaped_Expression) extends Synonym
/**
 * A predicate antonym
 */
case class Pred_Antonym(reg: Positions, _patNew:Predicate_Pattern, _patRefOld: Pattern_Shaped_Expression) extends Antonym
/**
 * An attribute synonym
 */
case class Attr_Synonym(reg: Positions, _patNew:Attribute_Pattern, _patRefOld:Pattern_Shaped_Expression) extends Synonym
/**
 * An attribute antonym
 */
case class Attr_Antonym(reg: Positions, _patNew:Attribute_Pattern, _patRefOld:Pattern_Shaped_Expression) extends Antonym
/**
 * A functor synonym
 */
case class Func_Synonym(reg: Positions, _patNew:Functor_Patterns, _patRefOld:Pattern_Shaped_Expression) extends Synonym
/**
 * A mode synonym
 */
case class Mode_Synonym(reg: Positions, _patNew:Mode_Pattern, _patRefOld:Pattern_Shaped_Expression) extends Synonym

/**
 * A cluster containing several registrations
 * @param _registrs the registrations
 */
case class Cluster(_registrs:List[Registrations]) extends RegistrationSubitems
/**
 * States that a term tm has some properties
 * @param _aggrTerm the term
 * @param _adjCl its properties
 * @param _tp (optional) qualifies
 */
case class Functorial_Registration(pos: Position, _aggrTerm:MizTerm, _adjCl:Adjective_Cluster, _tp:Option[Type]) extends Registrations
/**
 * States that there is an element for which some attributes hold on a type
 * @param _adjClust the cluster containing the attributes
 * @param _tp the type they are applied to
 */
case class Existential_Registration(pos: Position, _adjClust:Adjective_Cluster, _tp:Type) extends Registrations
/**
 * States that some attributes attrs implies another attribute at
 * @param _attrs the attributes attrs
 * @param _at the implied attribute
 * @param _tp the type the attributes are applied to
 */
case class Conditional_Registration(pos: Position, _attrs:Adjective_Cluster, _at: Adjective_Cluster, _tp:Type) extends Registrations
/**
 * Registering sethood property (with _tp given) for a mode
 * @param _props the property to register (currently always sethood)
 * @param _block the proof of the property
 */
case class Property_Registration(_props:Properties, _block:Block) extends Registrations
/**
 * State that if the _lociEqns are satesfied, then the pattern_shaped_expressions should be identified as equal
 * @param _firstPat the first pattern-shaped expression, to be identified
 * @param _sndPat the second pattern-shaped expression, to be identified
 * @param _lociEqns the loci equations wiht conditions for the identification
 */
case class Identify(_firstPat:Pattern_Shaped_Expression, _sndPat:Pattern_Shaped_Expression, _lociEqns:Loci_Equalities) extends Registrations with ProofLevel
/**
 * Stating that a term can be simplifies to a subterm (this can then be used by the checker in mizar)
 * expects a correctness condition reducibility
 * @param _predecessor the term that can be simplified
 * @param _successor the simplified subterm
 */
case class Reduction(_predecessor:MizTerm, _successor:MizTerm) extends Registrations with ProofLevel

/**
 * Contains a single block containing one subitem being the scheme head for this scheme and some futher subitems jointly forming the proof of it
 * @param mmlId the mmlId of the contained scheme
 * @param _block a block containing the scheme
 */
case class Scheme_Block_Item(pos: Positions, mmlId: MMLId, _block:Block) extends MMLIdSubitem {
  override def kind: String = "Scheme"
  def scheme_head: Scheme_Head = {
    assert(_block.kind == "Scheme-Block")
    val scheme_head_item = _block._items.head
    assert(scheme_head_item.kind == "Scheme-Head")
    scheme_head_item._subitem match {
      case scheme_Head: Scheme_Head => scheme_Head
      case _ => throw ImplementationError("Scheme head expected as first item in Scheme-Block-Item. ")
    }
  }
  def notationBasedReference = scheme_head._sch.globalNameO
  /**
   * Statement and proof of the scheme
   * @return
   */
  def provenSentence: ProvedClaim = {
    val justItems = _block._items.tail
    ProvedClaim(scheme_head._sentence, Some(Block(pos, "Proof", justItems)))
  }
}
/**
 * the head (first subitem) of a scheme-block, followed by a number of justification items for the scheme sentence
 * @param _sch the scheme, containing the scheme nr, allowing to reference it (however, it is redundant as the scheme-block-item already contains the same information)
 * @param _vars variables to the scheme (they can be functions or predicates)
 * @param _sentence the scheme sentence
 * @param _provForm assumptions to the scheme
 */
case class Scheme_Head(_sch:Scheme, _vars:Schematic_Variables, _sentence:Formula, _provForm:Option[Provisional_Formulas]) extends DeclarationLevel

/**
 * a proven claim
 */
sealed trait Statement {
  def prfClaim: ProvedClaim
}
/**
 * the last item in a proof
 * @param prfClaim the claim and its justification
 */
case class Conclusion(prfClaim:ProvedClaim) extends Statement with ProofLevel
/**
 * corresponds to a reconsider
 * followed by a list of assignments
 *
 * this statement states that an argument has certain type, the changed type is used afterwards
 */
case class Type_Changing_Statement(_eqList:Equalities_List, _tp:Type, _just:Justification) extends Statement with TopOrDeclarationLevel with ProofLevel {
  def claim: Claim = Type_Changing_Claim(_eqList, _tp)
  override def prfClaim: ProvedClaim = ProvedClaim(claim, Some(_just))
}
/**
 * An anonymous statement
 * @param prfClaim the provedClaim of the statement
 */
case class Regular_Statement(prfClaim:ProvedClaim) extends Statement with TopOrDeclarationLevel with ProofLevel
/**
 * A named toplevel statement
 * @param mmlId themmlId by which the theorem can be referenced
 * @param _prop the claim
 * @param _just its justification
 */
case class Theorem_Item(mmlId:MMLId, _prop: Proposition, _just: Justification) extends Statement with MMLIdSubitem {
  override def prfClaim: ProvedClaim = ProvedClaim(_prop, Some(_just))
  def labelled: Boolean = _prop._label.spelling != ""
  def referenceableLabel = if (labelled) _prop.referenceableLabel else globalName
}
/**
 * Statement that variables satisfying certain conditions exist
 * @param _qual the context segment defining the variables
 * @param prfClaim the conditions, followed by a justification for them
 */
case class Choice_Statement(_qual:Qualified_Segments, prfClaim:ProvedClaim) extends Statement with TopOrDeclarationLevel with ProofLevel
/**
 * the head (first subitem) of a case-block, followed by a number of justification items for that case
 * @param _ass the assumptions defining the case in the case distinction
 */
case class Case_Head(_ass:Assumptions) extends ProofLevel
/**
 * Corresponds to a case distinction in a proof
 * Only occurs within proofs
 * @param _just the remaining Proof, starting with the distinguished cases (as Case_Blocks)
 */
case class Per_Cases(_just:Justification) extends ProofLevel
/**
 * A single case in a case distinction
 * @param _block a block containing a case-head followed by justification items for that case
 */
case class Case_Block(_block:Block) extends ProofLevel
/**
 * Contains several exemplifications
 * @param _exams the exemplifications
 */
case class Exemplification(_exams: List[Exemplifications]) extends ProofLevel
/**
 * Fix variables that are universally quantified over in a proof
 * @param _qual variable segments containing the variables
 * @param _conds (optional) assumptions on the variables
 */
case class Generalization(_qual:Qualified_Segments, _conds:Option[Claim]) extends ProofLevel
/**
 * Fix variables in a proof
 * @param _qual variable segments containing the variables
 * @param _conds (optional) assumptions on the variables
 */
case class Default_Generalization(_qual:Qualified_Segments, _conds:Option[Claim]) extends ProofLevel
//The following is currently ignored during translation
case class Pragma(_notionName: Option[Pragmas]) extends TopOrDeclarationLevel

/**
 * Reserves some types for some variables, later uses of the varibales then use ReservedDscr_Type
 * as those ReservedDscr_Type still contain the types used, there is no need to translate these
 * @param _reservationSegments
 */
case class Reservation(_reservationSegments: List[Reservation_Segment]) extends TopOrDeclarationLevel