package info.kwarc.mmt.mizar.newxml.syntax

import info.kwarc.mmt.api.{GlobalName, ImplementationError, LocalName}
import info.kwarc.mmt.mizar.newxml.syntax.Claim
import info.kwarc.mmt.mizar.newxml.syntax.Utils._
import info.kwarc.mmt.mizar.newxml.translator.{DeclarationLevelTranslationError, TranslationController, TranslatorUtils}

sealed trait Subitem {
  def kind:String = {
    this.getClass.getName
  }
  def shortKind: String = kind.split('.').lastOption.getOrElse(kind).replace('_','-')
}
sealed trait TopLevel extends Subitem
sealed trait DeclarationLevel extends Subitem
sealed trait TopOrDeclarationLevel extends TopLevel with DeclarationLevel
sealed trait ProofLevel extends DeclarationLevel
sealed trait MMLIdSubitem extends TopOrDeclarationLevel {
  def mmlId: MMLId
  def globalName: GlobalName = {
    val Array(aid, ln) = mmlId.MMLId.split(":")
    TranslationController.getTheoryPath(aid) ? LocalName(this.shortKind+ln)
  }
}
sealed trait BlockSubitem extends TopOrDeclarationLevel
sealed trait RegistrationSubitems extends BlockSubitem
sealed trait Registrations extends RegistrationSubitems
sealed trait Definition extends BlockSubitem
sealed trait PrivateDefinition extends Definition

case class Reservation(_reservationSegments: List[Reservation_Segment]) extends TopOrDeclarationLevel
case class Definition_Item(_block:Block) extends TopOrDeclarationLevel {
  //verify assumptions for translation
  def check() = {
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
 * An empty item, the only interesting information is given in the containing item,
 * namely its position
 */
case class Section_Pragma() extends TopLevel
case class Pragma(_notionName: Option[Pragmas]) extends TopLevel
case class Cluster(_registrs:List[Registrations]) extends RegistrationSubitems

case class Identify(_firstPat:Pattern_Shaped_Expression, _sndPat:Pattern_Shaped_Expression, _lociEqns:Loci_Equalities) extends RegistrationSubitems

/**
 * Contains a single block containing one subitem being the scheme head for this scheme and some futher subitems jointly forming the proof of it
 * @param mmlId
 * @param _block
 */
case class Scheme_Block_Item(mmlId: MMLId, _block:Block) extends MMLIdSubitem {
  def scheme_head(): Scheme_Head = {
    assert(_block.kind == "Scheme-Block")
    val scheme_head_item = _block._items.head
    assert(scheme_head_item.kind == "Scheme-Head")
    scheme_head_item._subitem match {
      case scheme_Head: Scheme_Head => scheme_Head
      case _ => throw ImplementationError("Scheme head expected as first item in Scheme-Block-Item. ")
    }
  }
  /**
   * Statement and proof of the scheme
   * @return
   */
  def provenSentence() = {
    val justItems = _block._items.tail
    val startPos = justItems.head.pos.startPosition()
    val endPos = justItems.last.pos.endposition
    ProvedClaim(scheme_head()._form, Some(Block("Proof", Positions(Position(startPos.line.toString+"\\"+startPos.col), endPos), justItems)))
  }
}

sealed trait Heads extends TopLevel
/**
 *
 * @param _sch
 * @param _vars
 * @param _form
 * @param _provForm
 */
case class Scheme_Head(_sch:Scheme, _vars:Schematic_Variables, _form:Formula, _provForm:Option[Provisional_Formulas]) extends Heads
case class Suppose_Head(_ass:Assumptions) extends Heads
case class Case_Head(_ass:Assumptions) extends Heads

sealed trait Nyms extends BlockSubitem {
  def _patNew: Patterns
  def _patRefOld: Pattern_Shaped_Expression
  def antonymic: Boolean
}
sealed trait Synonym extends Nyms {
  override def antonymic = false
}
sealed trait Antonym extends Nyms {
  override def antonymic = true
}
case class Pred_Synonym(_patNew:Predicate_Pattern, _patRefOld: Pattern_Shaped_Expression) extends Synonym
case class Pred_Antonym(_patNew:Predicate_Pattern, _patRefOld: Pattern_Shaped_Expression) extends Antonym
case class Attr_Synonym(_patNew:Attribute_Pattern, _patRefOld:Pattern_Shaped_Expression) extends Synonym
case class Attr_Antonym(_patNew:Attribute_Pattern, _patRefOld:Pattern_Shaped_Expression) extends Antonym
case class Func_Synonym(_patNew:Functor_Patterns, _patRefOld:Pattern_Shaped_Expression) extends Synonym
case class Func_Antonym(_patNew:Functor_Patterns, _patRefOld:Pattern_Shaped_Expression) extends Antonym
case class Mode_Synonym(_patNew:Mode_Pattern, _patRefOld:Pattern_Shaped_Expression) extends Synonym

sealed trait Statement extends ProofLevel {
  def prfClaim: ProvedClaim
}
case class Conclusion(prfClaim:ProvedClaim) extends Statement
/**
 * corresponds to a reconsider
 * followed by a list of assignments
 */
case class Type_Changing_Statement(_eqList:Equalities_List, _tp:Type, _just:Justification) extends Statement with TopOrDeclarationLevel {
  def claim = Type_Changing_Claim(_eqList, _tp)
  override def prfClaim: ProvedClaim = ProvedClaim(claim, Some(_just))
}
case class Regular_Statement(prfClaim:ProvedClaim) extends Statement with TopOrDeclarationLevel
case class Theorem_Item(mmlId:MMLId, _prop: Proposition, _just: Justification) extends Statement with MMLIdSubitem with ProvenFactReference {
  override def prfClaim: ProvedClaim = ProvedClaim(_prop, Some(_just))
  def labelled: Boolean = _prop._label.spelling != ""
  override def referencedLabel: GlobalName = if (labelled) _prop.referencedLabel else globalName
}
case class Choice_Statement(_qual:Qualified_Segments, prfClaim:ProvedClaim) extends Statement with TopOrDeclarationLevel

/**
 * Definitions which have a label and may get redefined
 * @precondition _def.isEmpty => (redefinition <=> _pat.hasOrgiRefs && redefinition => mmlIdO.isEmpty)
 */
sealed trait RedefinableLabeledDefinition extends Definition with MMLIdSubitem {
  def mmlIdO: Option[MMLId]
  def _redef: Redefine
  def _pat: RedefinablePatterns
  def _def: Option[Definiens]
  def redefinition = _redef.occurs
  def check: Unit = {
    if (! (_def.isDefined || (redefinition == _pat.hasOrigRefs && (!redefinition || mmlIdO.isEmpty)))) {
      throw ImplementationError("Wrong parsing assumption. ")
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
    TranslationController.getTheoryPath(aid) ? LocalName(defKind.toString+ln)
  }
}
case class Attribute_Definition(mmlIdO:Option[MMLId], _redef:Redefine, _pat:Attribute_Pattern, _def:Option[Definiens]) extends RedefinableLabeledDefinition
case class Functor_Definition(mmlIdO:Option[MMLId], _redef:Redefine, _pat:RedefinableFunctor_Patterns, _tpSpec:Option[Type_Specification], _def:Option[Definiens]) extends RedefinableLabeledDefinition
case class Predicate_Definition(mmlIdO:Option[MMLId], _redef:Redefine, _pat:Predicate_Pattern, _def:Option[Definiens]) extends RedefinableLabeledDefinition
/**
 * definition of a structure, takes ancestors (a list of structures it inherits from),
 * structure-Pattern contains several loci, corresponds to the universes the structure is over,
 * a list of field segments, which first needs to repeat the fieldsSegments of the inherited structures
 * then the new ones
 * a field segment is a list of selectors of the same type
 * a selector is a field of the structure (or an inherited one)
 * finally there is a Structure_Patterns_Rendering defining the new notations (namely introducing the selectors)
 * @param _ancestors a list of structures (whoose selectors') to import
 * @param _strPat the structure pattern
 * @param _fieldSegms a list of field segments containing the selectors (fields) of the structure
 * @param _rendering contains patterns with notations for the selectors, the restriction
 */
case class Structure_Definition(_ancestors:Ancestors, _strPat:Structure_Pattern, _fieldSegms:Field_Segments, _rendering:Structure_Patterns_Rendering) extends Definition
case class Constant_Definition(_children:List[Equating]) extends Definition
case class Mode_Definition(_redef:Redefine, _pat:Mode_Pattern, _expMode:Modes) extends Definition
/**
 * 1-1 corresponds to a deffunc definition, its uses are private_functor_terms
 * used as shortcut and visible within its block
 * @param _var its variable
 * @param _tpList contains the type of the variable
 * @param _tm
 */
case class Private_Functor_Definition(_var:Variable, _tpList:Type_List, _tm:MizTerm) extends PrivateDefinition
case class Private_Predicate_Definition(_var:Variable, _tpList:Type_List, _form:Formula) extends PrivateDefinition

/**
 * stating that a term tm has some properties
 * @param pos
 * @param _aggrTerm the term
 * @param _adjCl its properties
 * @param _tp (optional) qualifies
 */
case class Functorial_Registration(pos:Position, _aggrTerm:MizTerm, _adjCl:Adjective_Cluster, _tp:Option[Type]) extends Registrations
/**
 * stating that some attributes hold on a type
 * @param pos
 * @param _adjClust
 * @param _tp
 */
case class Existential_Registration(pos:Position, _adjClust:Adjective_Cluster, _tp:Type) extends Registrations
/**
 * stating that some attributes attrs implies another attribute at
 * @param pos
 * @param _attrs the attributes attrs
 * @param _at the implied attribute
 * @param _tp
 */
case class Conditional_Registration(pos:Position, _attrs:Adjective_Cluster, _at: Adjective_Cluster, _tp:Type) extends Registrations
/**
 * registering properties for a mode
 */
case class Property_Registration(_props:Properties, _block:Block) extends Registrations

case class Scheme(idnr: Int, spelling:String, nr:Int) extends TopOrDeclarationLevel

case class Loci_Declaration(_qualSegms:Qualified_Segments, _conds:Option[Conditions]) extends DeclarationLevel
case class Assumption(_ass:Assumptions) extends DeclarationLevel
sealed trait Assumptions extends DeclarationLevel
case class Single_Assumption(pos:Position, _prop:Proposition) extends Assumptions
case class Collective_Assumption(pos:Position, _cond:Conditions) extends Assumptions
case class Existential_Assumption(_qualSegm:Qualified_Segments, _cond:Conditions) extends Assumptions

case class Correctness(_correctnessCond:Correctness_Conditions, _just:Justification) extends DeclarationLevel
case class Correctness_Conditions(_cond:List[CorrectnessConditions]) extends DeclarationLevel
case class Correctness_Condition(_cond:CorrectnessConditions, _just:Option[Justification]) extends DeclarationLevel
/**
 * Well-definedness conditions that need to be proven along with definitions
 */
sealed trait CorrectnessConditions extends DeclarationLevel
/**
 * non-emptyness of non-expandable types (modes) or clustered_types in registrations of attributes
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
/*/**
 * conjunction of all necessary correctness conditions, doesn't appear in esx files
 */
case class correctness() extends CorrectnessConditions*/
//telling Mizar to remember these properties for proofs later
case class Property(_props:Properties, _just:Option[Justification]) extends DeclarationLevel {
  def matchProperty() : Option[MizarProperty] = _props.matchProperty(_just)
}
/**
 * Corresponds to a case distinction in a proof
 * Only occurs within proofs
 * @param _just the remaining Proof, starting with the distinguished cases (as Case_Blocks)
 */
case class Per_Cases(_just:Justification) extends ProofLevel
/**
 * A single case in a case distinction
 * @param _block
 */
case class Case_Block(_block:Block) extends ProofLevel

case class Exemplification(_exams: List[Exemplifications]) extends ProofLevel
/**
 * Fix variables that are universally quantified over in a proof
 * @param _qual variable segments containing the variables
 * @param _conds
 */
case class Generalization(_qual:Qualified_Segments, _conds:Option[Claim]) extends ProofLevel
/**
 * Fix variables in a proof
 * @param _qual variable segments conatining the variables
 * @param _conds
 */
case class Default_Generalization(_qual:Qualified_Segments, _conds:Option[Claim]) extends ProofLevel
case class Reduction(_tm:MizTerm, _tm2:MizTerm) extends ProofLevel