package info.kwarc.mmt.focalize.syntax

import info.kwarc.mmt.api.utils.{XMLToScala,Group}
import XMLToScala.checkString

/** document (called a module in focalize) */
case class focdoc(named: NamedDoc, _declarations: List[ToplevelDeclaration])

/* ************* module level ************* */

sealed trait ToplevelDeclaration

/** modules are species and collections */
sealed abstract class FocModule extends ToplevelDeclaration

/** import a file */
case class load(_name: String) extends FocModule
/** load a file and make accessible without qualifiers */
case class open(_name: String) extends FocModule
/** coq import */
case class coq_require(_name: String) extends FocModule

/** theory declaration */
case class species(named: NamedMod, _parameters: List[parameter], _declarations: List[FocDeclaration]) extends FocModule

/**
 * instance of a concrete species
 * all fields in the implemented species must be defined, i.e., carrier, definition, letprop, or theorem
 * the collection can be used as a type (equal to the transparent carrier)
 *
 * declarations are the same as in the species but with all definitions filtered
 * the carrier does not appear in the interface (currently falsely appears as a constant rep of type carrier)
 */
case class collection(named: NamedMod, _implements: SpeciesExpr, _declarations: List[FocDeclaration]) extends FocModule

/** parameter of a species declaration
 *  kind = collection: type is a species (x is Species)
 *  kind = entity: type is a collection (x in Collection), always atomic
 */
// <type> child comes early, so the corresponding argument comes early too
case class parameter(kind: String, XMLtype: CollectionOrSpeciesExpr, named: NamedDecl) {
  checkString(kind, "entity", "collection")
}
object parameter {
  val collection = "collection"
  val element = "entity"
}

/** inductive type definition */
case class concrete_type(foc_name: String, _params: List[param], _df: List[TypedefPart]) extends ToplevelDeclaration
/** type variable giving the name of a parameter of a concrete type
 */
case class param(XMLtype: tvar)
/** concrete (= defined) types are one of the following:
 *  an alias, a list of constructors, a list of record fields, an abstract type (also used for base types like Int that are bound to different types in different systems
 */
sealed abstract class TypedefPart
/** type in a definition */
case class alias(XMLtype: TypeExpr) extends TypedefPart
/** constructor of an inductive type */
case class constr(foc_name: String, XMLtype: TypeExpr) extends TypedefPart
/** record type definition */
case class record_label_and_type(foc_name: String, XMLtype: TypeExpr) extends TypedefPart
/** a type that is abstract for focalize */
case class external_type() extends TypedefPart

/** toplevel function symbol definition (definiens still to be added) */
case class global_fun(foc_name: String, XMLtype: TypeExpr, _df: Option[Expression]) extends ToplevelDeclaration

/* ************* declaration level ************* */

/** declarations inside a species (also called methods)
 */
sealed abstract class FocDeclaration

sealed abstract class NamedDeclaration extends FocDeclaration {
  def name: NamedDecl
}

/** include */
case class inherits(_from: SpeciesExpr) extends FocDeclaration

/** the definition of Self (name is actually absent, i.e., will be empty) */
case class carrier(name: NamedDecl, XMLtype: TypeExpr) extends NamedDeclaration

/** function symbol declaration */
case class signature(name: NamedDecl, XMLtype: TypeExpr) extends NamedDeclaration

/** function symbol definition (definiens still to be added to FocDoc TODO) */
case class definition(name: NamedDecl, XMLtype: TypeExpr, _df: Option[Expression]) extends NamedDeclaration

/** boolean-valued defined function symbol, always fully applied, proof-irrelevant */
case class letprop(name: NamedDecl, _params: List[param_prop], _proposition: Proposition) extends NamedDeclaration with ToplevelDeclaration
case class param_prop(foc_name: String, XMLtype: TypeExpr)

/** axiom */
case class property(name: NamedDecl, proposition: Proposition) extends NamedDeclaration

/** theorem */
case class theorem(name: NamedDecl, proposition: Proposition, proof_ : ProofExpr) extends NamedDeclaration with ToplevelDeclaration

/* ************* identifiers ************* */

case class foc_name(infile: String, _name: String)

case class NamedDoc(foc_name: String, _md: general_informations) extends Group

case class NamedMod(foc_name: String, _md: Option[informations]) extends Group

case class NamedDecl(foc_name: String, _history: Option[history], _md: Option[informations]) extends Group

/* ************* metadata ************* */

case class general_informations(title: Option[String], author: Option[String], comments: Option[String])

/** @param math annotates MathML tag to a symbol */
case class informations(math: scala.xml.Node, comments: String)

/** information about how a name was imported into a species
 *  @param _initial the declaring species
 *  @param _from the current species
 */
case class history(_initial: initial_apparition, _from: comes_from) {
  /** the species where a method with this history was declared, and a flag indicating whether it was inherited */
  def origin = {
    val declaredIn = _initial.toName
    (declaredIn, declaredIn != _from.toName)
  }
}
/** the species defining the concept */
case class initial_apparition(infile: String, _name: String) {
  def toName = foc_name(infile, _name)
}
/** the species from which this is directly imported (could be the same as initial apparition) */
case class comes_from(infile: String, _name: String) {
  def toName = foc_name(infile, _name)
}
