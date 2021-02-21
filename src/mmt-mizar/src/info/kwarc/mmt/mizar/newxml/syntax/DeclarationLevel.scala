package info.kwarc.mmt.mizar.newxml.syntax

import info.kwarc.mmt.api.{GlobalName, ImplementationError}
import info.kwarc.mmt.mizar.newxml.syntax.Claim
import info.kwarc.mmt.mizar.newxml.syntax.Utils._
import info.kwarc.mmt.mizar.newxml.translator.{DeclarationLevelTranslationError, TranslatorUtils}

sealed trait DeclarationLevel
trait Subitem extends DeclarationLevel {
  def kind:String = {
    this.getClass.getName
  }
  def shortKind: String = kind.split('.').lastOption.getOrElse(kind).replace('_','-')
}
sealed trait MMLIdSubitem extends Subitem {
  def MmlId: MMLId
  def mizarGlobalName():MizarGlobalName = {
    val sgn = this.MmlId.mizarSemiGlobalName()
    sgn.makeGlobalName(this.shortKind)
  }
}
case class Reservation(_reservationSegments: List[Reservation_Segment]) extends Subitem
case class Definition_Item(_block:Block) extends Subitem {
  //verify addumptions for translation
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
case class Section_Pragma() extends Subitem
case class Pragma(_notionName: Option[Pragmas]) extends Subitem
case class Loci_Declaration(_qualSegms:Qualified_Segments, _conds:Option[Conditions]) extends Subitem
case class Cluster(_registrs:List[Registrations]) extends RegistrationSubitems
case class Correctness(_correctnessCond:Correctness_Conditions, _just:Justification) extends Subitem
case class Correctness_Condition(_cond:CorrectnessConditions, _just:Option[Justification]) extends Subitem
case class Exemplification(_exams:List[Exemplifications]) extends Subitem
case class Assumption(_ass:Assumptions) extends Subitem
case class Identify(_firstPat:Patterns, _sndPat:Patterns, _lociEqns:Loci_Equalities) extends RegistrationSubitems
case class Generalization(_qual:Qualified_Segments, _conds:Option[Claim]) extends Subitem // let
case class Reduction(_tm:MizTerm, _tm2:MizTerm) extends Subitem
case class Scheme_Block_Item(MmlId: MMLId, _block:Block) extends MMLIdSubitem {
  def scheme_head(): Scheme_Head = {
    assert(_block.kind == "Scheme-Block")
    val scheme_head_item = _block._items.head
    assert(scheme_head_item.kind == "Scheme-Head")
    scheme_head_item._subitem match {
      case scheme_Head: Scheme_Head => scheme_Head
      case _ => throw ImplementationError("Scheme head expected as first item in Scheme-Block-Item. ")
    }
  }
  def provenSentence() = {
    val justItems = _block._items.tail
    val startPos = justItems.head.pos.startPosition()
    val endPos = justItems.last.pos.endposition
    ProvedClaim(scheme_head()._form, Some(Block("Proof", Positions(Position(startPos.line.toString+"\\"+startPos.col), endPos), justItems)))
  }
}
//telling Mizar to remember these properties for proofs later
case class Property(_props:Properties, _just:Option[Justification]) extends Subitem {
  def matchProperty() : MizarProperty = _props.matchProperty(_just)
}
case class Per_Cases(_just:Justification) extends Subitem
case class Case_Block(_block:Block) extends Subitem

sealed trait Heads extends Subitem
case class Scheme_Head(_sch:Scheme, _vars:Schematic_Variables, _form:Formula, _provForm:Option[Provisional_Formulas]) extends Heads
case class Suppose_Head(_ass:Assumptions) extends Heads
case class Case_Head(_ass:Assumptions) extends Heads

sealed trait Nyms extends BlockSubitem {
  def _patOld: Patterns
  def _patNew: Patterns
  def antonymic: Boolean
}
sealed trait Synonym extends Nyms {
  override def antonymic = false
}
sealed trait Antonym extends Nyms {
  override def antonymic = true
}
case class Pred_Synonym(_patOld:Predicate_Pattern, _patNew: Predicate_Pattern) extends Synonym
case class Pred_Antonym(_patOld:Predicate_Pattern, _patNew: Predicate_Pattern) extends Antonym
case class Attr_Synonym(_patOld:Attribute_Pattern, _patNew:Attribute_Pattern) extends Synonym
case class Attr_Antonym(_patOld:Attribute_Pattern, _patNew:Attribute_Pattern) extends Antonym
case class Func_Synonym(_patOld:Functor_Patterns, _patNew:Functor_Patterns) extends Synonym
case class Func_Antonym(_patOld:Functor_Patterns, _patNew:Functor_Patterns) extends Antonym
case class Mode_Synonym(_patOld:Mode_Pattern, _patNew:Mode_Pattern) extends Synonym

sealed trait Statement extends Subitem {
  def prfClaim: ProvedClaim
}
case class Conclusion(prfClaim:ProvedClaim) extends Statement
/**
 * corresponds to a reconsider
 * followed by a list of assignments
 */
case class Type_Changing_Statement(_eqList:Equalities_List, _tp:Type, _just:Justification) extends Statement {
  def claim = Type_Changing_Claim(_eqList, _tp)
  override def prfClaim: ProvedClaim = ProvedClaim(claim, Some(_just))
}
case class Regular_Statement(prfClaim:ProvedClaim) extends Statement
case class Theorem_Item(MmlId:MMLId, _prop: Proposition, _just: Justification) extends Statement with MMLIdSubitem with ProvenFactReference {
  override def prfClaim: ProvedClaim = ProvedClaim(_prop, Some(_just))
  def labelled: Boolean = _prop._label.spelling != ""
  override def referencedLabel(): GlobalName = if (labelled) _prop.referencedLabel() else TranslatorUtils.mMLIdtoGlobalName(mizarGlobalName())
}
case class Choice_Statement(_qual:Qualified_Segments, prfClaim:ProvedClaim) extends Statement

sealed trait BlockSubitem extends Subitem
sealed trait Definition extends BlockSubitem
case class Attribute_Definition(MmlId:MMLId, _redef:Redefine, _attrPat:Attribute_Pattern, _def:Option[Definiens]) extends Definition with MMLIdSubitem
case class Functor_Definition(MmlId:MMLId, _redefine:Redefine, _pat:RedefinableFunctor_Patterns, _tpSpec:Option[Type_Specification], _def:Option[Definiens]) extends Definition with MMLIdSubitem
case class Predicate_Definition(MmlId:MMLId, _redefine:Redefine, _predPat:Predicate_Pattern, _def:Option[Definiens]) extends Definition with MMLIdSubitem
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
case class Private_Functor_Definition(_var:Variable, _tpList:Type_List, _tm:MizTerm) extends Definition
case class Private_Predicate_Definition(_var:Variable, _tpList:Type_List, _form:Formula) extends Definition

sealed trait RegistrationSubitems extends BlockSubitem
sealed trait Registrations extends RegistrationSubitems
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
case class Property_Registration(_props:Properties, _block:Block) extends Registrations with Subitem

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

sealed trait Pragmas extends DeclarationLevel
case class Unknown(pos:Position, inscription:String) extends Pragmas
case class Notion_Name(pos:Position, inscription:String) extends Pragmas
case class Canceled(MmlId:MMLId, amount:Int, kind:String, position:Position) extends Pragmas

case class Scheme(idNr: Int, spelling:String, nr:Int) extends DeclarationLevel
case class Reservation_Segment(pos: Position, _vars:Variables, _varSegm:Variable_Segments, _tp:Type) extends DeclarationLevel

sealed trait Segments extends DeclarationLevel {
  def _vars: Variables
  def _tpList: Type_List
  def _tpSpec: Option[Type_Specification]
}
case class Functor_Segment(pos:Position, _vars:Variables, _tpList:Type_List, _tpSpec:Option[Type_Specification]) extends Segments
case class Predicate_Segment(pos:Position, _vars:Variables, _tpList:Type_List) extends Segments {
  override def _tpSpec: Option[Type_Specification] = None
}