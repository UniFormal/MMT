package info.kwarc.mmt.pvs.syntax

import info.kwarc.mmt.api.utils.Group

case class pvs_file(_modules: List[Module])

trait Module
case class theory(named: NamedDecl, formals: List[formal],
                  assuming: List[AssumingDecl], exporting_ : Option[exporting], _decls: List[Decl]) extends Module

case class exporting(place: String, exporting_kind: String,
           exporting_names: List[name], exporting_but_names: List[name], exporting_theory_names: List[theory_name]) {
   assert(List("nil", "all", "closure", "default") contains exporting_kind)
}

sealed trait formal
case class formal_subtype_decl(_decl: type_def_decl) extends formal
case class formal_const_decl(named: ChainedDecl, tp: DeclaredType) extends formal
case class formal_theory_decl(_name: theory_name) extends formal

/**
 * Recursive types can occur at the top level, or as a declaration.
 * The latter are inline-datatypes, and are the same except that formals are not allowed.
 */
// List[importing] outside formals?
case class datatype(named: NamedDecl, formals: List[formal], _constructors: List[constructor]) extends Module

case class constructor(named: NamedDecl, ordnum: Int, accessors: List[accessor], recognizer: String, subtype_id: String)
case class accessor(named: NamedDecl, _type: Type)

sealed trait AssumingDecl
case class assumption(named: ChainedDecl, assertion: Assertion) extends AssumingDecl

sealed trait Decl extends AssumingDecl

case class type_decl(named: NamedDecl, ne: NonEmptiness, tp: DeclaredType) extends Decl with formal
case class type_def_decl(named: NamedDecl, ne: NonEmptiness, tp: DeclaredType, _def: Type) extends formal // no Decl?

case class const_decl(named: ChainedDecl,
                    decl_formals:List[formals], tp: DeclaredType, _def: Option[Expr]) extends Decl
case class def_decl(named: ChainedDecl,
                    decl_formals:List[formals], tp: DeclaredType, _def: Expr,
                    _measure: Expr, _order: Option[Expr]) extends Decl
case class var_decl(named: ChainedDecl, tp: DeclaredType) extends Decl

case class axiom_decl(named: ChainedDecl, assertion: Assertion) extends Decl
case class formula_decl(named: ChainedDecl, assertion: Assertion) extends Decl
case class tcc_decl(named: ChainedDecl, assertion: Assertion) extends Decl

trait Judgement extends Decl
case class subtype_judgement(named: NamedDecl, _sub: Type, _sup: Type) extends Judgement
case class number_judgement(named: NamedDecl, _number: number_expr, _type: Type) extends Judgement
case class name_judgement(named: NamedDecl, _name: name_expr, _type: Type) extends Judgement
case class application_judgement(named: NamedDecl, _type: Type, name: name_expr, bindings: List[binding]) extends Judgement

case class conversion_decl(unnamed: UnnamedDecl, expr: Expr) extends Decl

case class auto_rewrite(unnamed: UnnamedDecl, kind: String, rewrite_name: List[rewrite_name]) extends Decl {
   assert(List("plus", "minus") contains kind)
}
case class rewrite_name(place: String, kind: String, _name: name, _res: resolution, _spec: rewrite_name_spec) {
   assert(List("lazy", "eager", "macro") contains kind)
}
case class rewrite_name_spec() // element rewrite-name-spec {type-expr | name | xsd:integer}

case class lib_decl(named: ChainedDecl, _type: Type, _type2: Type) extends Decl
case class theory_decl(named: ChainedDecl, _type: Type, _type2: Type) extends Decl
case class type_from_decl(unnamed: UnnamedDecl, ne: NonEmptiness, _type: Type) extends Decl
case class macro_decl(named: ChainedDecl, tp: DeclaredType) extends Decl
case class ind_decl(unnamed: UnnamedDecl, tp: DeclaredType) extends Decl
case class corec_decl(unnamed: UnnamedDecl, tp: DeclaredType) extends Decl
case class coind_decl(unnamed: UnnamedDecl, tp: DeclaredType) extends Decl
case class inline_datatype(unnamed: UnnamedDecl, tp: DeclaredType) extends Decl
case class importing(unnamed: UnnamedDecl, name: theory_name) extends Decl with formal

sealed trait Object

sealed trait Type extends Object with domain
case class type_name(place: String, id: String, theory_id: String) extends Type
case class function_type(place: String, _from: domain, _to: Type) extends Type
case class subtype(place: String, _of: Type, _by: Expr) extends Type
case class tuple_type(place: String, _domains: List[domain]) extends Type
case class cotuple_type(place: String, _arguments: List[Type]) extends Type
case class record_type(place: String, _fields: List[field_decl]) extends Type
case class expr_as_type(place: String, _expr: Expr, _type: Type) extends Type
case class type_application(place: String, _type: type_name, _arguments: List[typearg]) extends Type

sealed trait domain
sealed trait typearg
case class field_decl(named: NamedDecl, _type: Type)

sealed trait Expr extends Object with assignment_arg with typearg
case class name_expr(place: String, id: String, _type: Type, _res: resolution) extends Expr
case class tuple_expr(_arguments: List[Expr]) extends Expr
case class number_expr(place: String, _num: Int) extends Expr
case class string_expr(place: String, _str: String) extends Expr
case class list_expr(place: String, _arguments: List[Expr]) extends Expr

case class record_expr(place: String, assignments: List[assignment]) extends Expr
case class field_expr(place: String, id: String) extends Expr
case class proj_expr(place: String, _index: Int) extends Expr
case class field_appl_expr(place: String, _expr: Expr, id: String) extends Expr
case class proj_appl_expr(place: String, _expr: Expr, index: Int) extends Expr

case class application(_fun: Expr, _arg: Expr, infix: Boolean) extends Expr
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

case class resolution(_theory: theory_name, _decl: declref)
case class declref(href: String)

sealed trait mapping
case class mapping_def(mapping_rhs: name, mapping_lhs: Object) extends mapping
case class mapping_subst(mapping_rhs: name, mapping_lhs: Object) extends mapping
case class mapping_rename(mapping_rhs: name, mapping_lhs: Object) extends mapping

case class target(_theory: theory_name)

case class theory_name(id: String, library_id: String, mappings: List[mapping], _actuals: List[Object]) extends Object
case class name(id: String, theory_id: String, library_id: String)

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
names = name+
name = id, theory-id?, library-id?, actuals-content?, mappings-content?, target?

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