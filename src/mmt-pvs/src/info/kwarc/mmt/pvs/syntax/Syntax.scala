package info.kwarc.mmt.pvs.syntax

import info.kwarc.mmt.api.utils.Group

case class pvs_file(_modules: List[Module])

trait Module
case class theory(named: NamedDecl, formals: List[FormalParameterDecl],
                  assuming: List[AssumingDecl], exporting_ : Option[exporting], _decls: List[Decl]) extends Module

case class exporting(place: String, exporting_kind: String,
           exporting_names: List[name], exporting_but_names: List[name], exporting_theory_names: List[theory_name]) {
   assert(List("nil", "all", "closure", "default") contains exporting_kind)
}

/**
 * Recursive types can occur at the top level, or as a declaration.
 * The latter are inline-datatypes, and are the same except that formals are not allowed.
 */
// List[importing] outside formals?
case class datatype(named: NamedDecl, formals: List[FormalParameterDecl], _constructors: List[constructor]) extends Module
// case class codatatype extends Module missing in rnc

case class constructor(named: NamedDecl, ordnum: Int, accessors: List[accessor], recognizer: String, subtype_id: String)
case class accessor(named: NamedDecl, _type: Type)

/**
 * a declaration in the list of parameters of a declaration
 * 
 * This is essentially a subtype of Decl, but some Decl counterparts can be more general and are declared separately.
 */
sealed trait FormalParameterDecl

/** subtype declaration ??? */
case class type_def_decl(named: NamedDecl, ne: NonEmptiness, df: DeclaredType, _def: Type) extends FormalParameterDecl
/** c:T */
case class formal_const_decl(named: ChainedDecl, tp: DeclaredType) extends FormalParameterDecl
/** a TYPE FROM A (i.e., a is predicate subtype of A) */
case class formal_subtype_decl(named: ChainedDecl, sup: Type) extends FormalParameterDecl
/** m: T for a theory T */
// ??? named missing in rnc
case class formal_theory_decl(named: ChainedDecl, _name: theory_name) extends FormalParameterDecl

/** 
 *  a declaration in the assumptions of a theory
 *  
 *  This is the same as Decl except for allowing assumptions.
 */
sealed trait AssumingDecl
/** a: F for a formula F */
case class assumption(named: ChainedDecl, assertion: Assertion) extends AssumingDecl

/** a declaration in the body of theory */ 
sealed trait Decl extends AssumingDecl

/** include T, can occur in formal parameters without semantic change in case parameters already need included declarations */
case class importing(unnamed: UnnamedDecl, name: theory_name) extends Decl with FormalParameterDecl
/** a TYPE [= A] (can also occur as formal parameter but only without definiens) */
// list of formal parameters missing in rnc
case class type_decl(named: NamedDecl, ne: NonEmptiness, df: Option[DeclaredType]) extends Decl with FormalParameterDecl
/**
 * c(G) : A [= t]
 * ??? why list of list in formals, why no type-decls in formals
 */
case class const_decl(named: ChainedDecl, decl_formals: List[formals], tp: DeclaredType, _def: Option[Expr]) extends Decl
/** c(G) : A [= t] where c is a recursive function */
case class def_decl(named: ChainedDecl, decl_formals:List[formals], tp: DeclaredType, _def: Expr,
                    _measure: Expr, _order: Option[Expr]) extends Decl
/** x: A (declares type of future variables) ??? */
case class var_decl(named: ChainedDecl, tp: DeclaredType) extends Decl

/** a: F for a formula F ??? no formals */
case class axiom_decl(named: ChainedDecl, assertion: Assertion) extends Decl
/** a: F for a formula F (has proof, which is not exported) */
case class formula_decl(named: ChainedDecl, assertion: Assertion) extends Decl
/** a: F for a formula (for a proof obligation, prepended to a declaration by the type checker) */
case class tcc_decl(named: ChainedDecl, assertion: Assertion) extends Decl

/** named theorem-flavor statement in the meta-logic */
trait Judgement extends Decl
/** p: |- A <: B */
case class subtype_judgement(named: NamedDecl, _sub: Type, _sup: Type) extends Judgement
/** p: |- t: A (some limitations on t) */
// missing in rnc
case class expr_judgement(named: NamedDecl, _expr: Expr, _type: Type) extends Judgement
/** p: |- name: A (special case of expr_judgement) */
case class name_judgement(named: NamedDecl, _name: name_expr, _type: Type) extends Judgement
/** p: |- number: A (special case of expr_judgement) */
case class number_judgement(named: NamedDecl, _number: number_expr, _type: Type) extends Judgement
/** p: |- name(bindings) : A */
// _type occurs first in rnc
case class application_judgement(named: NamedDecl, name: name_expr, bindings: List[binding], _type: Type) extends Judgement
// recursive judgement missing in rnc

/** conversion f (makes f an implicit conversion */
case class conversion_decl(unnamed: UnnamedDecl, expr: Expr) extends Decl

/** name of a formula that is loaded as a conditional rewrite rule */
case class auto_rewrite(unnamed: UnnamedDecl, kind: String, rewrite_name: List[rewrite_name]) extends Decl {
   assert(List("plus", "minus") contains kind)
}

/** ??? */
case class rewrite_name(place: String, kind: String, _name: name, _res: resolution, _spec: rewrite_name_spec) {
   assert(List("lazy", "eager", "macro") contains kind)
}
/** ??? */
case class rewrite_name_spec() // element rewrite-name-spec {type-expr | name | xsd:integer}
/** assigns a short to library (= folder) ??? fields look wrong in rnc */
case class lib_decl(named: ChainedDecl, _type: Type, _type2: Type) extends Decl
/** named import (parameters, instantiations, and renamings are part of the name (fields are wrong in rnc) */
case class theory_decl(named: ChainedDecl, domain: theory_name) extends Decl
case class type_from_decl(unnamed: UnnamedDecl, ne: NonEmptiness, _type: Type) extends Decl
case class macro_decl(named: ChainedDecl, tp: DeclaredType) extends Decl
case class ind_decl(unnamed: UnnamedDecl, tp: DeclaredType) extends Decl
case class corec_decl(unnamed: UnnamedDecl, tp: DeclaredType) extends Decl
case class coind_decl(unnamed: UnnamedDecl, tp: DeclaredType) extends Decl
case class inline_datatype(unnamed: UnnamedDecl, tp: DeclaredType) extends Decl

sealed trait Object

sealed trait Type extends Object with domain
/** a */
case class type_name(place: String, id: String, theory_id: String) extends Type
/** a(args) */
case class type_application(place: String, _type: type_name, _arguments: List[typearg]) extends Type
/** A -> B */
case class function_type(place: String, _from: domain, _to: Type) extends Type
/** A|F (predicate subtype) */
case class subtype(place: String, _of: Type, _by: Expr) extends Type
/** A1 * ... * An (what are domains???) */
case class tuple_type(place: String, _domains: List[domain]) extends Type
/** A1 + ... + An (what are domains???) */
case class cotuple_type(place: String, _arguments: List[Type]) extends Type
/** {f1:A1, ..., fn: An} */
case class record_type(place: String, _fields: List[field_decl]) extends Type
/** ??? */
case class expr_as_type(place: String, _expr: Expr, _type: Type) extends Type

sealed trait domain
sealed trait typearg
case class field_decl(named: NamedDecl, _type: Type)

sealed trait Expr extends Object with assignment_arg with typearg
/** reference to a declared name
 *  @param name the user-provided reference
 *  @param _type the type disambiguating overloading (can be computed from resolution)
 *  @param _res internal, disambiguated reference (internally maintained as a pointer)
 */
case class name_expr(place: String, name: name, _type: Type, _res: resolution) extends Expr
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
/** ??? */
case class field_expr(place: String, id: String) extends Expr
/** ??? */
case class proj_expr(place: String, _index: Int) extends Expr
/** ??? */
case class field_appl_expr(place: String, _expr: Expr, id: String) extends Expr
/** ??? */
case class proj_appl_expr(place: String, _expr: Expr, index: Int) extends Expr
/** ??? */
case class coercion_expr(place: String, _of: Expr, _to: Type) extends Expr
case class if_expr(place: String, _if: Expr, _then: Expr, _else: Expr) extends Expr
case class lambda_expr(place: String, bindings: List[binding], _body: Expr) extends Expr
case class forall_expr(place: String, bindings: List[binding], _body: Expr) extends Expr
case class exists_expr(place: String, bindings: List[binding], _body: Expr) extends Expr
case class set_expr(place: String, bindings: List[binding], _body: Expr) extends Expr
case class let_expr(place: String, let_bindings: List[let_binding], _body: Expr) extends Expr
case class update_expr(place: String, assignments: List[assignment]) extends Expr
case class cases_expr(place: String, _expr: Expr, selections: List[selection]) extends Expr
case class cond_expr(place: String, _expr: Expr, conditions: List[condition]) extends Expr
// case class table_expr(place: String, ) extends Expr

case class binding(named: ChainedDecl, _type: Type) extends domain with typearg // takes attribute and child id
case class let_binding(named: ChainedDecl, _type: Type, _expr: Expr) // not quite
case class condition(_0: Expr, _1: Expr)
case class selection(place: String, _cons: Expr, bindings: List[binding], _expr: Expr)
case class formals(formals: List[binding])

case class assignment(plase: String, assignment_args: List[assignment_arg], _expr: Expr)
sealed trait assignment_arg
case class field_assign(place: String, id: String) extends assignment_arg
case class proj_assign(place: String, _index: Int) extends assignment_arg

/** internal reference to a declared name, corresponds to MMT URI */
case class resolution(_theory: theory_name, _decl: declref)
/** attempt to represent pointers a xlinks */
case class declref(href: String)

/** superclass of instantiations and renaming when forming theories */
sealed trait mapping
/** c := c' */
case class mapping_subst(mapping_rhs: name, mapping_lhs: Object) extends mapping
/** c ~> c' */
case class mapping_rename(mapping_rhs: name, mapping_lhs: Object) extends mapping
// in rnc but obsolete
//case class mapping_def(mapping_rhs: name, mapping_lhs: Object) extends mapping

/**
 * user-provided reference to a module
 * @param id theory name
 * @param library_id library (in particular, as declared by lib_decl)
 * @param _actuals instantiations for the formal parameters (positional, full)
 * @param mappings instantiations/renaming for
 */
// ??? no target
case class theory_name(id: String, library_id: String, mappings: List[mapping], _actuals: List[Object]) extends Object
/**
 * user-provided reference to a symbol
 * @param id theory name
 * @param library_id library (in particular, as declared by lib_decl)
 * @param _actuals instantiations for the formal parameters (positional, full)
 * @param mappings instantiations/renaming for
 * @param target abbreviation for substituting a set of names at once by the declarations of the same local name in a different theory
 */
case class name(id: String, theory_id: String, library_id: String,
                mappings: List[mapping], target: Option[theory_name], _actuals: List[Object])


/* commonly used groups of attributes/children */

case class NamedDecl(id: String, place: String) extends Group
case class ChainedDecl(id: String, place: String, chain_p: Boolean) extends Group
case class UnnamedDecl(place: String, chain_p: Boolean) extends Group

case class DeclaredType(_declared: Type, _internal: Type) extends Group
case class Assertion(kind: String, _formula: Expr) extends Group {
   assert(List("assumption", "axiom", "challenge", "claim", "conjecture", "corollary", "fact", "formula", "law",
               "lemma", "obligation", "postulate", "proposition", "sublemma", "theorem") contains kind)
}
case class NonEmptiness(nonempty_type: Boolean, contains: Option[Expr]) extends Group

/*
## Links
declref = element declref { attlist.declref }
attlist.declref &=
  [ a:defaultValue = "simple" ] attribute xlink:type { "simple" }?,
  attribute xlink:href { text }?,
  attribute xlink:role { text }?,
  attribute xlink:arcrole { text }?,
  attribute xlink:title { text }?,
  attribute xlink:show { "new" | "replace" | "embed" | "other" | "none" }?,
  attribute xlink:actuate { "onLoad" | "onRequest" | "other" | "none" }?

*/