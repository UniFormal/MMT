package info.kwarc.mmt.pvs.syntax

import info.kwarc.mmt.api.utils.Group

/** PVS objects are theories, types, and expressions */
sealed trait Object

// ****************************************
// ********** types

/** PVS types */
sealed trait Type extends Object with domain
/** a */
case class type_name(place: String, name: name, _res: Option[resolution]) extends Type
/** a(args) */
case class type_application(place: String, _type: type_name, _arguments: List[typearg]) extends Type
/** A -> B or (x:A) -> B */
case class function_type(place: String, _from: domain, _to: Type) extends Type
/** predicate subtype -- A|F */
case class subtype(place: String, _of: Type, _by: Expr) extends Type
/** flexary, possibly dependent product -- e.g., A1 * (x2:A2) * ... * An */
case class tuple_type(place: String, _domains: List[domain]) extends Type
/** A1 + ... + An */
case class cotuple_type(place: String, _arguments: List[Type]) extends Type
/** {f1:A1, ..., fn: An} */
case class record_type(place: String, _fields: List[field_decl]) extends Type
// ??? formals in record field?
case class field_decl(named: NamedDecl, _type: Type)

/** (p): type, for a predicate p: _type = a => bool; _ */
case class expr_as_type(place: String, _expr: Expr, _type: Option[Type]) extends Type

/**
 * A domain is a type, possibly binding a variable
 *  
 * Its two cases unify the simple and dependent cases of product and function types
 *  
 * The only way to create dependent types is via predicate subtypes (including declared ones).
 */
sealed trait domain

/** ??? */
sealed trait typearg

// ****************************************
// ********** expressions (typed)

sealed trait Expr extends Object with assignment_arg with typearg
/** reference to a declared name
 *  @param name the user-provided reference
 *  @param _type the type disambiguating overloading (can be computed from resolution)
 *  @param _res internal, disambiguated reference (internally maintained as a pointer)
 */
case class name_expr(place: String, name: name, _type: Type, _res: resolution) extends Expr
/** */
case class varname_expr(place: String, name: name, _type: Type) extends Expr
/** number literal */
case class number_expr(place: String, _num: Int) extends Expr
/** string literal */
case class string_expr(place: String, _str: String) extends Expr
/** function application */
case class application(_fun: Expr, _arg: Expr, infix: Boolean) extends Expr
/** tuple */
case class tuple_expr(_arguments: List[Expr]) extends Expr
/** list */
case class list_expr(place: String, _arguments: List[Expr]) extends Expr
/** record */
case class record_expr(place: String, assignments: List[assignment]) extends Expr
/** n-th projection (as a polymorphic function, turned into lambda with fresh type variable by type checker) */
case class proj_expr(place: String, _index: Int) extends Expr
/** n-th projection applied to an argument */
case class proj_appl_expr(place: String, _expr: Expr, index: Int) extends Expr
/** named projection from a record */
case class field_appl_expr(place: String, _expr: Expr, id: String) extends Expr
/** (a :: A), coerce a to A, which must be compatible with the type of a */
case class coercion_expr(place: String, _of: Expr, _to: Type) extends Expr

// ********** binders

/** universal quantifier */
case class forall_expr(place: String, bindings: List[binding], _body: Expr) extends Expr
/** existential quantifier */
case class exists_expr(place: String, bindings: List[binding], _body: Expr) extends Expr
/** lambda abstraction */
case class lambda_expr(place: String, binding_list: List[bindings], _body: Expr) extends Expr
/** equal to lambda, convenience for using predicates as (expression-level) sets */
case class set_expr(place: String, bindings: List[binding], _body: Expr) extends Expr
/** let expression  */
case class let_expr(place: String, let_bindings: List[let_binding], _body: Expr) extends Expr
/** list of variable bindings */
case class bindings(bindings: List[binding])
/* a single variable binding */
case class binding(id: String, named: ChainedDecl, _type: Type) extends domain with typearg
/* a single variable binding with definiens */
case class let_binding(named: ChainedDecl, _type: Type, _expr: Expr) // not quite

// ********** case distinctions

/** if-then-else expression */
case class if_expr(place: String, _if: Expr, _then: Expr, _else: Expr) extends Expr
/** condition expression, i.e., if-expression with multiple cases (checked for disjointness and exhaustiveness) */
case class cond_expr(place: String, conditions: List[cond]) extends Expr
/** if -> then (case in a condition expression) */
case class cond(_if: Expr, _then: Expr)

/** one-level pattern matching of an expression against a list of cases */
case class cases_expr(place: String, _expr: Expr, selections: List[selection]) extends Expr
/** case in a case expression -- id(bindings) : _expr */
case class selection(place: String, id: String, bindings: List[binding], _expr: Expr)

// ********** updates

case class assignment(plase: String, assignment_args: List[assignment_arg], _expr: Expr)
/** */
sealed trait assignment_arg
case class field_assign(place: String, id: String) extends assignment_arg
case class proj_assign(place: String, _index: Int) extends assignment_arg

/** change value in a tuple, record, or function */
case class update_expr(place: String, assignments: List[assignment]) extends Expr

// ********** tables

/** table expressions, similar to 2-dimensional arrays */
case class table_expr(place: String) extends Expr // other fields omitted

/** internal reference to a declared name, corresponds to MMT URI */
case class resolution(_theory: theory_name, _decl: declref)
/** attempt to represent pointers a xlinks; changed to integer-index in list of overloaded id in that theory */
case class declref(href: String)

// ****************************************
// ********** Names

/**
 * the only theory expressions are references (names)
 * however, those subsume instantiation of parameters and declarations
 */
sealed trait TheoryExpr extends Object

/**
 * user-provided reference to a module
 * @param id theory name
 * @param library_id library (in particular, as declared by lib_decl)
 * @param _actuals instantiations for the formal parameters (positional, full)
 * @param mappings instantiations/renaming for
 */
case class theory_name(id: String, library_id: String, mappings: List[mapping], target: Option[theory_name], actuals: List[Object]) extends TheoryExpr

/**
 * user-provided reference to a symbol
 * @param id theory name
 * @param library_id library (in particular, as declared by lib_decl)
 * @param _actuals instantiations for the formal parameters (positional, full)
 * @param mappings instantiations/renaming for
 * @param target abbreviation for substituting a set of names at once by the declarations of the same local name in a different theory
 */
case class name(id: String, theory_id: String, library_id: String,
                mappings: List[mapping], target: Option[theory_name], actuals: List[Object]) extends Group


/** superclass of instantiations and renaming when forming theories */
sealed trait mapping
/** c := c' */
case class mapping_subst(mapping_rhs: name, mapping_lhs: Object) extends mapping
/** c ~> c' */
case class mapping_rename(mapping_rhs: name, mapping_lhs: Object) extends mapping
// ??? mapping_def obsolete in rnc


// ****************************************
// ********** commonly used groups of attributes/children

/** common parts of a named declaration in a theory */
case class NamedDecl(id: String, place: String, formals: List[FormalParameter]) extends Group
/**
 * formal parameters only allowed if optional id is given
 */
case class OptNamedDecl(id: Option[String], place: String, formals: List[FormalParameter]) extends Group
/** like NamedDecl but may be chained */
case class ChainedDecl(named: NamedDecl, chain_p: Boolean) extends Group
/** common parts of an unnamed declaration in a theory */
case class UnnamedDecl(place: String, chain_p: Boolean) extends Group

/** */
case class DeclaredType(_declared: Type, _internal: Type) extends Group
/**
 * formula can have free variables
 */
case class Assertion(kind: String, _formula: Expr) extends Group {
   assert(List("assumption", "axiom", "challenge", "claim", "conjecture", "corollary", "fact", "formula", "law",
               "lemma", "obligation", "postulate", "proposition", "sublemma", "theorem") contains kind)
}
case class NonEmptiness(nonempty_p: Boolean, contains: Option[Expr]) extends Group