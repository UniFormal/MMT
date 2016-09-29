package info.kwarc.mmt.focalize.syntax

import info.kwarc.mmt.api.utils.{XMLToScala,Group}
import XMLToScala.checkString

/** document (called a module in focalize) */
case class focdoc(named: NamedDoc, body: List[Module])

/* ************* module level ************* */

trait ToplevelDeclaration

/** modules are species and collections */
abstract class Module extends ToplevelDeclaration

/** import a file */
case class load(name: String) extends Module
/** load a file and make accessible without qualifiers */
case class open(name: String) extends Module
/** coq import */
case class coq_require(name: String) extends Module

/** theory declaration */
case class species(named: NamedMod, _parameters: List[parameter], _declarations: List[Declaration]) extends Module

/**
 * instance of a concrete species
 * all fields in the implemented species must be defined, i.e., carrier, definition, letprop, or theorem
 * the collection can be used as a type (equal to the transparent carrier)
 * 
 * declarations are the same as in the species but with all definitions filtered
 * the carrier does not appear in the interface (currently falsely appears as a constant rep of type carrier)
 */
case class collection(named: NamedMod, _implements: SpeciesExpr, _declarations: List[Declaration]) extends Module

/** parameter of a species declaration
 *  kind = collection: type is a species (x is Species)
 *  kind = entity: type is a collection (x in Collection), always atomic
 */
case class parameter(kind: String, _name: foc_name, __type: CollectionOrSpeciesExpr) {
  checkString(kind, "entity", "collection")
}

/** inductive type definition */
case class concrete_type(name: foc_name, _params: List[param], df: List[TypedefPart]) extends Module
/** type variable giving the name of a parameter of an inductive type */
case class param(__type: tvar)
/** concrete (= defined) types are one of the following:
 *  an alias, a list of constructors, a list of record fields, an abstract type (also used for base types like Int that are bound to different types in different systems
 */
abstract class TypedefPart
/** type in a definition */
case class alias(__type: TypeExpr) extends TypedefPart
/** constructor of an inductive type */
case class constr(name: foc_name, __type: TypeExpr) extends TypedefPart
/** record type definition */
case class record_label_and_type(name: foc_name, __type: TypeExpr) extends TypedefPart
/** a type that is abstract for focalize */
case class external_type() extends TypedefPart

/** toplevel function symbol definition (definiens still to be added) */
case class global_fun(name: NamedDecl, __type: TypeExpr) extends ToplevelDeclaration

/* ************* declaration level ************* */

/** declarations inside a species
 */
abstract class Declaration

/** include */
case class inherits(_from: SpeciesExpr) extends Declaration
/** the definition of Self (name is actually absent, i.e., will be empty) */
case class carrier(name: NamedDecl, __type: TypeExpr) extends Declaration
/** function symbol declaration */
case class signature(name: NamedDecl, __type: TypeExpr) extends Declaration
/** function symbol definition (definiens still to be added to FocDoc XXX) */
case class definition(name: NamedDecl, __type: TypeExpr) extends Declaration

/** boolean-valued defined function symbol, always fully applied, proof-irrelevant */
case class letprop(name: NamedDecl, params: List[param_prop], proposition: Proposition) extends Declaration with ToplevelDeclaration
/** axiom */
case class property(name: NamedDecl, proposition: Proposition) extends Declaration
/** theorem */
case class theorem(name: NamedDecl, proposition: Proposition, proof: ProofExpr) extends Declaration with ToplevelDeclaration

case class param_prop(_name: foc_name, __type: TypeExpr)

/* ************* identifiers ************* */

case class foc_name(infile: String, name: String)

case class NamedDoc(foc_name: String, md: general_informations) extends Group

case class NamedMod(foc_name: String, md: Option[informations]) extends Group

case class NamedDecl(foc_name: String, _history: Option[history], _md: Option[informations]) extends Group

/* ************* metadata ************* */

case class general_informations(title: Option[String], author: Option[String], comments: Option[String])
case class informations(math: String, comments: String)

/** information about how a name was imported into a species */
case class history(_initial: initial_apparition, _from: comes_from)
/** the species defining the concept */
case class initial_apparition(infile: String, _name: String)
/** the species from which this is directly imported (could be the same as initial apparition) */
case class comes_from(infile: String, _name: String)