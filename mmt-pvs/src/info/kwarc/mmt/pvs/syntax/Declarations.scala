package info.kwarc.mmt.pvs.syntax

import info.kwarc.mmt.api.utils.{XMLToScala,Group}
import XMLToScala.checkString

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
//parameters taken twice
case class theory(named: NamedDecl, theory_formals: List[FormalParameter],
                  assuming: List[AssumingDecl], exporting_ : Option[exporting], _decls: List[Decl]) extends Module

case class exporting(place: String, exporting_kind: String,
           exporting_names_ : exporting_names, exporting_but_names_ : exporting_names, exporting_theory_names: List[theory_name]) {
   checkString(exporting_kind, "nil", "all", "closure", "default")
}
case class exporting_names(name:List[name]) extends Group

// ********** datatypes
/**
 * an ADT-like datatype declared at toplevel, see also [[InlineDatatypeBody]]
 *
 * for historical reasons, imports can occur separately in addition to within formals although that is redundant  
 */
case class TopDatatypeBody(named: NamedDecl, theory_formals: List[FormalParameter], importings: List[importing],
                            _constructors: List[constructor]) extends Group
/** a top-level datatype with least fixed point semantics */
case class datatype(body: TopDatatypeBody) extends Module

/** a top-level datatype with greatest fixed point semantics */
case class codatatype(body: TopDatatypeBody) extends Module

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

/** type -- a TYPE */
// not in rnc
case class formal_type_decl(named: ChainedDecl, ne: NonEmptiness) extends FormalParameter
/** type with given supertype -- a TYPE FROM A for a type A */
case class formal_subtype_decl(named: ChainedDecl, ne: NonEmptiness, sup: DeclaredType) extends FormalParameter
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
/** a: F for a formula F (semantically, an axiom in the list of formal parameters, becomes tcc when instantiating the theory) */
case class assumption(named: ChainedDecl, assertion: Assertion) extends AssumingDecl

/** a declaration in the body of theory */ 
sealed trait Decl extends AssumingDecl

// ********** basic declarations

/** a TYPE (may be asserted to be non-empty */
case class type_decl(named: ChainedDecl, nonempty_p: Boolean) extends Decl
/** a TYPE FROM A (a is subtype of A) */
case class type_from_decl(named: ChainedDecl, nonempty_p: Boolean, tp: DeclaredType) extends Decl
/** a(arg_formals) TYPE = A (defined types may be dependent) */
case class type_def_decl(named: NamedDecl, ne: NonEmptiness, arg_formals: List[bindings], df: DeclaredType) extends Decl
/**
 * c(G) : A [= t]
 */
case class const_decl(named: ChainedDecl, arg_formals: List[bindings], tp: DeclaredType, _def: Option[Expr]) extends Decl with Group {
  override def toString() = named.named.id + "[" + arg_formals.map(_.toString).mkString(", ") + "]: " + tp + " = " + _def.map(_.toString)
}
/** defined constant that is always expanded */
case class macro_decl(decl: const_decl) extends Decl

/* can all have formals */
/** named import (parameters, instantiations, and renamings are part of the name); rnc to be revisited */
case class theory_decl(named: ChainedDecl, _domain: theory_name) extends Decl
/** c(G) : A [= t] where c is a recursive function */
case class def_decl(named: ChainedDecl, arg_formals: List[bindings], tp: DeclaredType, _def: Expr,
                    _measure: Option[Expr], _order: Option[Expr]) extends Decl

// ********** complex declarations

/** inductive definition */
case class ind_decl(named: NamedDecl, arg_formals: List[bindings], tp: DeclaredType, _body: Expr) extends Decl
/** coinductive definition */
case class coind_decl(named: NamedDecl, arg_formals: List[bindings], tp: DeclaredType, _body: Expr) extends Decl

/** an ADT-like datatype declared inside a theory, see also [[TopDatatypeBody]] */
case class InlineDatatypeBody(named: NamedDecl, arg_formals: List[bindings], _constructors: List[constructor]) extends Group
/** an inline ADT with least fixed point semantics */
case class inline_datatype(body: InlineDatatypeBody) extends Decl
/** an inline ADT with greatest fixed point semantics */
case class inline_codatatype(body: InlineDatatypeBody) extends Decl

// ********** enum stuff
case class enumtype_decl(named:NamedDecl, enum_elts :List[id]) extends Decl
case class id(_id: String) //extends Group

// ********** assertions

/** a: F for a formula F */
case class axiom_decl(named: ChainedDecl, assertion: Assertion) extends Decl
/** a: F for a formula F (has proof, which is not exported) */
case class formula_decl(named: ChainedDecl, assertion: Assertion) extends Decl
/** a: F for a formula (for a proof obligation, prepended to a declaration by the type checker) */
case class tcc_decl(named: ChainedDecl, assertion: Assertion) extends Decl

// ********** judgements

/** named theorem-flavor statement in the meta-logic */
trait Judgement extends Decl
/** p: |- A <: B */
case class subtype_judgement(named: OptNamedDecl, sub: DeclaredType, sup: DeclaredType) extends Judgement
/** p: |- t: A (some limitations on t) */
case class expr_judgement(named: OptNamedDecl, _expr: Expr, tp: DeclaredType) extends Judgement
/** p: |- name: A (special case of expr_judgement) */
case class name_judgement(named: OptNamedDecl, _name: name_expr, tp: DeclaredType) extends Judgement
/** p: |- number: A (special case of expr_judgement) */
case class number_judgement(named: OptNamedDecl, _number: number_expr, tp: DeclaredType) extends Judgement
/** p: |- name(arg_formals) : A */
case class application_judgement(named: OptNamedDecl, _name: name_expr, arg_formals: List[bindings], tp: DeclaredType) extends Judgement
// ??? recursive judgment missing in rnc

// ********** non-denoting declarations

/** x: A (declares type of future variables) */
case class var_decl(id: String, unnamed: UnnamedDecl, tp: DeclaredType) extends Decl

/** conversion f (makes f an implicit conversion) */
case class conversion_decl(unnamed: UnnamedDecl, kind: String, _expr: Expr) extends Decl {
   checkString(kind, "add", "remove")
}

/** name of a formula that is loaded as a conditional rewrite rule */
case class auto_rewrite(unnamed: UnnamedDecl, key: String, kind: String, _rewrite_name : List[rewrite_name]) extends Decl {
   checkString(kind, "add", "remove")
}

/** reference to a formula that is to be used in rewriting */
case class rewrite_name(place: String, kind: Option[String], name: name, _res: Option[resolution], _spec: Option[rewrite_name_spec]) {
   if (kind.isDefined) checkString(kind.get, "lazy", "eager", "macro")
}
/** user-provided information for disambiguation in a rewrite_name */
case class rewrite_name_spec() // element rewrite-name-spec {type-expr | formula_name}

/** assigns a short name to a library (= folder) */
case class lib_decl(id: String, unnamed: UnnamedDecl, library: String) extends Decl

/** include T, can occur in formal parameters without semantic change in case parameters already need included declarations */
case class importing(unnamed: UnnamedDecl, _name: theory_name) extends Decl with FormalParameter

// ****************************************
// ********** commonly used groups of attributes/children

/** common parts of a named declaration in a theory */
case class NamedDecl(id: String, place: String, decl_formals: List[FormalParameter]) extends Group
/**
 * formal parameters only allowed if optional id is given
 */
case class OptNamedDecl(id: Option[String], place: String, formals: List[FormalParameter], semi_colon_p: Boolean) extends Group
/** like NamedDecl but may be chained */
case class ChainedDecl(named: NamedDecl, chain_p: Boolean, semi_colon_p: Boolean) extends Group
/** common parts of an unnamed declaration in a theory */
case class UnnamedDecl(place: String, chain_p: Boolean, semi_colon_p: Boolean) extends Group

//case class DeclParameters(decl_formals: List[FormalParameter], arg_formals: List[bindings]) extends Group

/** */
case class DeclaredType(_declared: Type, _internal: Type) extends Group {
  override def toString() = "{" + _declared.toString + " / " + _internal.toString + "}"
}
/**
 * formula can have free variables
 */
case class Assertion(kind: String, _formula: Expr) extends Group {
   assert(List("assumption", "axiom", "challenge", "claim", "conjecture", "corollary", "fact", "formula", "law",
               "lemma", "obligation", "postulate", "proposition", "sublemma", "theorem") contains kind)
}
case class NonEmptiness(nonempty_p: Boolean, contains: Option[Expr]) extends Group