package info.kwarc.mmt.pvs.syntax

import info.kwarc.mmt.api.utils.Group

/* Conventions needed for the XML parser
 *  * class names and field names correspond to the tag and attribute names in the XML syntax
 *  * exception: field names starting with _ have no analogue in the XML syntax, and the parser uses the next available XML object(s) to fill them
 */

// ****************************************
// ********** toplevel structure

/**
 * a pvs file containing some modules
 */
case class pvs_file(_modules: List[Module])

/**
 * toplevel declarations
 */
trait Module
case class theory(named: NamedDecl, theory_formals: List[FormalParameter],
                  assuming: List[AssumingDecl], exporting_ : Option[exporting], _decls: List[Decl]) extends Module

case class exporting(place: String, exporting_kind: String,
           exporting_names: List[name], exporting_but_names: List[name], exporting_theory_names: List[theory_name]) {
   assert(List("nil", "all", "closure", "default") contains exporting_kind)
}

// ********** datatypes

/**
 * Recursive types can occur at the top level, or as a declaration.
 * The latter are inline-datatypes, and are the same except that formals are not allowed.
 */
// ??? List[importing] outside formals?
case class datatype(named: NamedDecl, formals: List[FormalParameter], _constructors: List[constructor]) extends Module
// case class codatatype extends Module missing in rnc

/** constructor of an inductive data type
 *  @param ordnum constructors are numbered consecutively for convenience
 */
case class constructor(named: NamedDecl, ordnum: Int, accessors: List[accessor], recognizer: String, subtype_id: Option[String])
case class accessor(named: NamedDecl, _type: Type)

// ********** formal parameters

/**
 * a declaration in the list of parameters of a theory declaration
 * 
 * This is essentially a subtype of Decl, but the Decl counterparts are general.
 */
sealed trait FormalParameter

/** type -- a TYPE [= A] */
// not in rnc
case class formal_type_decl(named: ChainedDecl, ne: NonEmptiness) extends FormalParameter
/** type with given supertype -- a TYPE FROM A for a type A */
case class formal_subtype_decl(named: ChainedDecl, ne: NonEmptiness, _sup: Type) extends FormalParameter
/** typed constant -- c:A for a type A */
case class formal_const_decl(named: ChainedDecl, tp: DeclaredType) extends FormalParameter
/** model of a theory -- m: T for a theory T */
case class formal_theory_decl(named: ChainedDecl, _name: theory_name) extends FormalParameter


// ****************************************
// ********** declarations in a theory

/** 
 *  a declaration in the assumptions of a theory
 *  
 *  This is the same as [[Decl]] except for also allowing assumptions (= axioms).
 */
sealed trait AssumingDecl
/** a: F for a formula F */
case class assumption(named: ChainedDecl, assertion: Assertion) extends AssumingDecl

/** a declaration in the body of theory */ 
sealed trait Decl extends AssumingDecl

// ********** basic declarations

/** a TYPE [= A] (??? formal parameter in rnc) */
//??? chained?
case class type_decl(named: NamedDecl, ne: NonEmptiness, df: Option[DeclaredType]) extends Decl
/** a TYPE FROM A (a is subtype of A) ??? unnamed in rnc? */
case class type_from_decl(named: ChainedDecl, ne: NonEmptiness, _type: Type) extends Decl
/** a TYPE = A ??? relation to type-decl? */
case class type_def_decl(named: NamedDecl, ne: NonEmptiness, df: DeclaredType, _def: Type) extends Decl
/**
 * c(G) : A [= t]
 */
case class const_decl(named: ChainedDecl, arg_formals: List[bindings], tp: DeclaredType, _def: Option[Expr]) extends Decl with Group
/** defined constant that is always defined ??? def missing in rnc */
case class macro_decl(decl: const_decl) extends Decl

/* can all have formals */
/** named import (parameters, instantiations, and renamings are part of the name); rnc to be revisited */
case class theory_decl(named: ChainedDecl, domain: theory_name) extends Decl
/** c(G) : A [= t] where c is a recursive function */
case class def_decl(named: ChainedDecl, tp: DeclaredType, _def: Expr,
                    _measure: Expr, _order: Option[Expr]) extends Decl

// ********** complex declarations

/** inductive definition */
case class ind_decl(unnamed: NamedDecl, tp: DeclaredType, _body: Expr) extends Decl
/** coinductive definition */
case class coind_decl(unnamed: NamedDecl, tp: DeclaredType, _body: Expr) extends Decl
/** like adt but with decl_formals instead of theory_formals */
case class inline_datatype(unnamed: NamedDecl, tp: DeclaredType) extends Decl

// ********** assertions

/** a: F for a formula F ??? different to assumption? */
case class axiom_decl(named: ChainedDecl, assertion: Assertion) extends Decl
/** a: F for a formula F (has proof, which is not exported) */
case class formula_decl(named: ChainedDecl, assertion: Assertion) extends Decl
/** a: F for a formula (for a proof obligation, prepended to a declaration by the type checker) */
case class tcc_decl(named: ChainedDecl, assertion: Assertion) extends Decl

// ********** judgements

/** named theorem-flavor statement in the meta-logic */
// ??? Type or DeclaredType
trait Judgement extends Decl
/** p: |- A <: B */
case class subtype_judgement(named: OptNamedDecl, _sub: Type, _sup: Type) extends Judgement
/** p: |- t: A (some limitations on t) */
// ??? missing in rnc
case class expr_judgement(named: OptNamedDecl, _expr: Expr, _type: Type) extends Judgement
/** p: |- name: A (special case of expr_judgement) */
case class name_judgement(named: OptNamedDecl, _name: name_expr, _type: Type) extends Judgement
/** p: |- number: A (special case of expr_judgement) */
case class number_judgement(named: OptNamedDecl, _number: number_expr, _type: Type) extends Judgement
/** p: |- name(bindings) : A */
// ??? _type occurs first in rnc
case class application_judgement(named: OptNamedDecl, _name: name_expr, bindings: List[binding], _type: Type) extends Judgement
// ??? recursive judgment missing in rnc

// ********** non-denoting declarations

/** x: A (declares type of future variables) */
case class var_decl(id: String, unnamed: UnnamedDecl, tp: DeclaredType) extends Decl

/** conversion f (makes f an implicit conversion */
case class conversion_decl(unnamed: UnnamedDecl, _expr: Expr) extends Decl

/** name of a formula that is loaded as a conditional rewrite rule */
case class auto_rewrite(unnamed: UnnamedDecl, kind: String, rewrite_name: List[rewrite_name]) extends Decl {
   assert(List("plus", "minus") contains kind)
}

/** reference to a formula that is to be used in rewriting */
case class rewrite_name(place: String, kind: String, _name: name, _res: resolution, _spec: rewrite_name_spec) {
   assert(List("lazy", "eager", "macro") contains kind)
}
/** user-provided information for disambiguation in a rewrite_name */
case class rewrite_name_spec() // element rewrite-name-spec {type-expr | formula_name}

/** assigns a short name to a library (= folder) */
case class lib_decl(id: String, unnamed: UnnamedDecl, library: String) extends Decl

/** include T, can occur in formal parameters without semantic change in case parameters already need included declarations */
case class importing(unnamed: UnnamedDecl, name: theory_name) extends Decl with FormalParameter