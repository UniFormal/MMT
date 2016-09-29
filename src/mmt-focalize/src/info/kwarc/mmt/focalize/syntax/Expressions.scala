package info.kwarc.mmt.focalize.syntax

import info.kwarc.mmt.api.utils.{XMLToScala,Group}
import XMLToScala.checkString

/** There are three kinds of types:
 *  * plain types (TypeExpr) correspond to plain types and sets
 *  * collections (CollectionOrSpeciesExpr) refer to the carrier of a collection, collections are always names (atom)
 *  * species (CollectionOrSpeciesExpr) correspond to theories and model classes; their values are the collections
 *    species expressions are always names (atom) or names applied to arguments (app); all such arguments are names
 */
trait CollectionOrSpeciesExpr

/** theory expression */
trait SpeciesExpr extends CollectionOrSpeciesExpr

/** instantiation of species parameters */ 
case class app(species: foc_name, args: List[param_inst]) extends SpeciesExpr

/** param_inst can be
 *  * Self: the collection implementing the current species (Java this)
 *  * collection name (collection expression, which is anyway atomic)
 *  * previous (entity or collection) parameter
 *  Note: A previous parameter is the only way to provide an entity parameter
 *  
 *  currently these are confusingly exported as <param [infile=String]>String</param>
 *  XXX
 */
case class param_inst()

/**
 * plain types
 * built-in types for literals are defined as abstract types in the standard library in basics.fcl
 */
trait TypeExpr

/** type symbol
 *  order = high: species symbol
 *  order = first: plain type symbol or (the underlying representaion type of a) collection
 */
case class atom(order: String, infile: String, _name: String) extends TypeExpr with CollectionOrSpeciesExpr {
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
/** can be treated the same way as prod */
case class sum_args(_components: List[TypeExpr]) extends TypeExpr


/** typed expressions
 *  
 *  not every expression is a proposition, but those of type Bool can be used as propositions
 *  
 *  The special expression Self is used is exported as a foc_name. XXX
 */
abstract class Expression extends Proposition
/** bracketed expression */
case class paren_expr(_expr: Expression) extends Expression
/** variable */
case class __var(_name: String) extends Expression
/** symbol (type arguments of identifiers are currently not exported) */
case class identifier(_name: foc_name, of_species: Option[foc_name], poly_args: List[TypeExpr]) extends Expression
/** like identifier but with additional information about printing */
case class symbol(id: identifier) extends Expression

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

/** constructors of inductive types are represented using application and identifier */

/** pattern matching is currently not exported */


/** propositions
 *  
 *  equality is defined in basics.fcl in the standard library
 */
abstract class Proposition
/** bracketed formula */
case class paren_logical_expr(_expr: Proposition) extends Proposition
/** conjunction */
case class and(left: Proposition, right: Proposition) extends Proposition
/** disjunction */
case class or(left: Proposition, right: Proposition) extends Proposition
/** negation */
case class not(arg: Proposition) extends Proposition
/** implication */
case class implies(left: Proposition, right: Proposition) extends Proposition
/** equivalence */
case class equiv(left: Proposition, right: Proposition) extends Proposition
/** universal quantification */
case class all(__var: foc_name, __type: TypeExpr, _body: Proposition) extends Proposition
/** existential quantification */
case class ex(__var: foc_name, __type: TypeExpr, _body: Proposition) extends Proposition

/**
 * proofs
 */
abstract class ProofExpr

case class proof() extends ProofExpr