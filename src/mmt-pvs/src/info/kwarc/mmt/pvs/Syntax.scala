package info.kwarc.mmt.pvs

import info.kwarc.mmt.api._
import utils._
import archives._ 

trait Module
case class theory(place: String, id: String, formals: List[formal],
                  assuming: List[AssumingDecl], exporting_ : Option[exporting], children: List[Decl]) extends Module

case class exporting(place: String, exporting_kind: String,
           exporting_names: List[name], exporting_but_names: List[name], exporting_theory_names: List[theory_name]) {
   assert(List("nil", "all", "closure", "default") contains exporting_kind)
}

sealed trait formal
case class formal_subtype_decl() extends formal
case class formal_const_decl() extends formal
case class formal_theory_decl() extends formal

/*
formal-subtype-decl-content =
  element formal-subtype-decl {type-def-decl}
formal-const-decl-content =
  element formal-const-decl {id, typed-declaration}
formal-theory-decl-content =
  element formal-theory-decl {theory-name}
*/

case class datatype() extends Module
case class codatatype() extends Module

/*
# Recursive types can occur at the top level, or as a declaration.  The
# latter are inline-datatypes, and are the same except that formals are not
# allowed.

recursive-type-content =
  element datatype
    {commonattrs,
     id,
     formals-content?,
     importings,
     constructors}

constructors = constructor-content+

constructor-content =
  element constructor
    {commonattrs,
     ordnumattr,
     id,
     accessors-content?,
     recognizer-content,
     subtype-id-content?}

ordnumattr =
  attribute ordnum {xsd:integer}?
recognizer-content =
  element recognizer {identifier-token}
accessors-content =
  element accessors {accessor-content+}
subtype-id-content =
  element subtype-id {id}
accessor-content =
  element accessor
    {commonattrs,
     id,
     type-expr}
*/

sealed trait AssumingDecl
case class assumption(place: String, chain_p: Boolean, id: String, kind: String, _formula: Expr) extends AssumingDecl with FormulaKind

sealed trait Decl extends AssumingDecl

case class type_decl(place: String, nonempty_type: Boolean, id: String, contains: Option[Expr], children: List[Type]) extends Decl with formal
case class const_decl(place: String, chain_p: Boolean, id: String,
                    decl_formals:List[formals], _declared: Type, _type: Type, _def: Option[Expr]) extends Decl
case class def_decl(place: String, chain_p: Boolean, id: String,
                    decl_formals:List[formals], _declared: Type, _type: Type, _def: Expr,
                    _measure: Expr, _order: Option[Expr]) extends Decl
case class formals(formals: List[binding])

case class var_decl(place: String, chain_p: Boolean, id: String, _declared: Type, _type: Type) extends Decl
case class axiom_decl(place: String, chain_p: Boolean, kind: String, id: String, _formula: Expr) extends Decl with FormulaKind
case class formula_decl(place: String, chain_p: Boolean, kind: String, id: String, _formula: Expr) extends Decl with FormulaKind

trait Judgement extends Decl
case class subtype_judgement(place: String, id: String, _sub: Type, _sup: Type) extends Judgement
case class number_judgement(place: String, id: String, _number: number_expr, _type: Type) extends Judgement
case class name_judgement(place: String, id: String, _name: name_expr, _type: Type) extends Judgement
case class application_judgement(place: String, id: String, _type: Type, name: name_expr, bindings: List[binding]) extends Judgement

case class conversion_decl(place: String, chain_p: Boolean, expr: Expr) extends Decl

case class auto_rewrite(kind: String, place: String, chain_p: Boolean, rewrite_name: List[rewrite_name]) extends Decl {
   assert(List("plus", "minus") contains kind)
}
case class rewrite_name(place: String, kind: String, _name: name, _res: resolution, _spec: rewrite_name_spec) {
   assert(List("lazy", "eager", "macro") contains kind)
}
case class rewrite_name_spec() // element rewrite-name-spec {type-expr | name | xsd:integer}

case class lib_decl(place: String, chain_p: Boolean, id: String, _type: Type, _type2: Type) extends Decl
case class theory_decl(place: String, chain_p: Boolean, id: String, _type: Type, _type2: Type) extends Decl
case class type_from_decl(place: String, chain_p: Boolean, non_empty: Boolean, contains: Option[Expr], _type: Type) extends Decl
case class macro_decl(place: String, chain_p: String, id: String, _declared: Type, _type: Type) extends Decl
case class ind_decl(place: String, chain_p: String, _declared: Type, _type: Type) extends Decl
case class corec_decl(place: String, chain_p: String, _declared: Type, _type: Type) extends Decl
case class coind_decl(place: String, chain_p: String, _declared: Type, _type: Type) extends Decl
case class inline_datatype(place: String, chain_p: String, _declared: Type, _type: Type) extends Decl
case class tcc_decl(place: String, chain_p: Boolean, id: String, kind: String, _expr: Expr) extends Decl with FormulaKind
case class importing(place: String, chain_p: Boolean, name: theory_name) extends Decl with formal

trait FormulaKind {
   def kind: String
   def check {
      assert(List("assumption", "axiom", "challenge", "claim", "conjecture", "corollary", "fact", "formula", "law",
                   "lemma", "obligation", "postulate", "proposition", "sublemma", "theorem") contains kind)
   }
}

sealed trait Object

sealed trait Type extends Object with domain
case class type_name(place: String, id: String, theory_id: String) extends Type
case class function_type(place: String, _from: domain, _to: Type) extends Type
case class subtype(place: String, _of: Type, _by: Expr) extends Type
case class tuple_type(place: String, children: List[domain]) extends Type
case class cotuple_type(place: String, children: List[Type]) extends Type
case class record_type(place: String, children: List[field_decl]) extends Type
case class expr_as_type(place: String, _expr: Expr, _type: Type) extends Type
case class type_application(place: String, _type: type_name, children: List[typearg]) extends Type

sealed trait domain
sealed trait typearg
case class field_decl(place: String, id: String, _type: Type)

sealed trait Expr extends Object with assignment_arg with typearg
case class name_expr(place: String, id: String, _type: Type, _res: resolution) extends Expr
case class tuple_expr(children: List[Expr]) extends Expr
case class number_expr(place: String, _num: Int) extends Expr
case class string_expr(place: String, _str: String) extends Expr
case class list_expr(place: String, children: List[Expr]) extends Expr

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

case class binding(place: String, chain_p: Boolean, id: String, _type: Type) extends domain with typearg // takes attribute and child id
case class let_binding(place: String, chain_p: Boolean, id: String, _type: Type, _expr: Expr) // not quite
case class condition(_0: Expr, _1: Expr)
case class selection(place: String, _cons: Expr, bindings: List[binding], _expr: Expr)

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

case class theory_name(id: String, library_id: String, mappings: List[mapping], children: List[Object]) extends Object
case class name(id: String, theory_id: String, library_id: String)

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