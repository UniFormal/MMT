package info.kwarc.mmt.focalize.syntax

import info.kwarc.mmt.api.utils.{XMLToScala,Group}
import XMLToScala.checkString

/** There are three kinds of types:
 *  * plain types (TypeExpr) correspond to plain types and sets
 *  * collections (CollectionOrSpeciesExpr) refer to the carrier of a collection, collections are always names (atom)
 *  * species (CollectionOrSpeciesExpr) correspond to theories and model classes; their values are the collections
 *    species expressions are always names (atom) or names applied to arguments (app); all such arguments are names
 */
sealed trait CollectionOrSpeciesExpr

/** theory expression */
sealed trait SpeciesExpr extends CollectionOrSpeciesExpr

/** ground realization expression */
sealed trait CollectionExpr extends CollectionOrSpeciesExpr

/** instantiation of species parameters */ 
case class app(_species: foc_name, _args: List[param]) extends SpeciesExpr

/** param_inst can be
 *  * Self: the collection implementing the current species (Java this),
 *    typically used in inherits-declaration when the required species has already been inherited already
 *  * collection name (collection expression, which is anyway atomic)
 *  * previous (entity or collection) parameter
 *  Note: A previous parameter is the only way to provide an entity parameter
 *  
 *  TODO currently these are confusingly exported as <param [infile=String]>String</param>
 */
case class param(infile: String, _name: String)

/**
 * plain types
 * built-in types for literals are defined as abstract types in the standard library in basics.fcl
 */
sealed trait TypeExpr

/** type symbol (except for self, all type names are global)
 *  order = high: species symbol
 *  order = first: plain type symbol or (the underlying representation type of a) collection
 *  @param infile the file in which the species is declared if different from current file
 */
case class atom(order: String, infile: String, _name: String) extends TypeExpr with SpeciesExpr with CollectionExpr {
  checkString(order, "high", "first")
}
/** the implicit type symbol representing the carrier of models */
case class self() extends TypeExpr
/** type variable */
case class tvar(_name: String) extends TypeExpr
/** type operator applied to arguments */
case class prm(order: String, _arguments: List[TypeExpr], _operator: foc_name) extends TypeExpr {
  checkString(order, "first")
}
/** function type */
case class fct(_domain: TypeExpr, _codomain: TypeExpr) extends TypeExpr
/** product type */
case class prod(_components: List[TypeExpr]) extends TypeExpr
/** only used for arguments of a constructor of an inductive type, can be treated the same way as prod */
case class sum_args(_components: List[TypeExpr]) extends TypeExpr


/** typed expressions
 *  
 *  not every expression is a proposition, but those of type Bool can be used as propositions
 *  
 *  The special expression E_self is exported as a foc_name in the code but that does not actually occur.
 */
sealed abstract class Expression extends Proposition
/** bracketed expression */
case class paren_expr(_expr: Expression) extends Expression

/** symbol (type arguments of identifiers are currently not exported)
 *
 * There are the following cases
 *  - global definition (global function, constructor of inductive type, projection of record type)
 *    no of_species
 *  - method declared in the current species or inherited from a different species
 *    no of_species
 *  - locally bound variable
 *    no of_species
 *  - method of a collection
 *    of_species refers to collection (global collection or collection parameter; could theoretically be Self but that never occurs in export)
 *  - species parameter: impossible because expression variables cannot be species parameters
 */
case class identifier(_name: foc_name, of_species: Option[foc_name], poly_args: List[TypeExpr]) extends Expression

/** like identifier but with additional information about printing */
case class symbol(math: scala.xml.Node, _id: identifier) extends Expression

/** if */
case class if_expr(_if: Expression, _then: Expression, _else: Expression) extends Expression

/** lambda abstraction (curried just for convenience), anonymous functions are rare and variables cannot be ascribed by the user */
case class fun(varnames: List[foc_name], _body: Expression) extends Expression

/** function application (curried just for convenience) */
case class application(_fun: Expression, _args: List[Expression]) extends Expression

/** 2n children: identifier, expression, identifier, ... */ 
case class record_expr(_fields: List[RecordField]) extends Expression
case class RecordField(_name: identifier, _value: Expression) extends Group

/** record projection */
case class record_access_expr(_arg: Expression, _projection: identifier) extends Expression

/** tuple (the elimination form for tuples is a absent and relegated to pattern-matching) */
case class tuple_expr(_args: List[Expression]) extends Expression

/** hypothetical, not exported yet */
//case class let(XMLvar: foc_name, XMLtype, TypeExpr, _df: Expression,_body: Expression) extends Expression

/** constructors of inductive types are represented using application and identifier */

/** integer literals */
case class int(XMLval: BigInt) extends Expression

/** local variable binding */
case class XMLvar(_name: String)


/** pattern matching is currently not exported */


/** propositions
 *  
 *  equality is defined in basics.fcl in the standard library
 *  
 *  application of letprop-defined predicate symbols is special case of application.
 */
sealed abstract class Proposition
/** bracketed formula */
case class paren_logical_expr(_expr: Proposition) extends Proposition
/** conjunction */
case class and(_left: Proposition, _right: Proposition) extends Proposition
/** disjunction */
case class or(_left: Proposition, _right: Proposition) extends Proposition
/** negation */
case class not(_arg: Proposition) extends Proposition
/** implication */
case class implies(_left: Proposition, _right: Proposition) extends Proposition
/** equivalence */
case class equiv(_left: Proposition, _right: Proposition) extends Proposition
/** universal quantification */
case class all(XMLvar: foc_name, XMLtype: TypeExpr, _body: Proposition) extends Proposition
/** existential quantification */
case class ex(XMLvar: foc_name, XMLtype: TypeExpr, _body: Proposition) extends Proposition

/**
 * proofs
 */
sealed abstract class ProofExpr

case class proof() extends ProofExpr