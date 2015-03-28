package info.kwarc.mmt.pvs

import info.kwarc.mmt.api._
import utils._
import archives._ 

trait Module
case class theory(place: String, id: String, children: List[Decl]) extends Module
case class datatype() extends Module
case class codatatype() extends Module

sealed trait Decl
case class type_decl(place: String, nonempty_type: Boolean, id: String, children: List[type_name]) extends Decl
case class const_decl(place: String, chain_p: Boolean, id: String, children: List[Type]) extends Decl
case class var_decl(place: String, chain_p: Boolean, id: String, children: List[Type]) extends Decl
case class axiom_decl(place: String, kind: String, id: String, _formula: Expr) extends Decl
case class formula_decl(place: String, kind: String, id: String, _formula: Expr) extends Decl

case class judgement_decl(place: String) extends Decl
case class tcc_decl(place: String) extends Decl
case class lib_decl(place: String) extends Decl
case class macro_decl(place: String) extends Decl
case class auto_rewrite(place: String) extends Decl
case class def_decl(place: String) extends Decl
case class ind_decl(place: String) extends Decl
case class corec_decl(place: String) extends Decl
case class coind_decl(place: String) extends Decl
case class type_from_decl(place: String) extends Decl
case class conversion(place: String) extends Decl
case class theory_decl(place: String) extends Decl
case class inline_datatype(place: String) extends Decl
case class importing() extends Decl
case class exporting(exporting_kind: String) extends Decl

/*
judgement-decl = subtype-judgement-content
               | number-judgement-content
               | name-judgement-content
               | application-judgement-content
subtype-judgement-content =
  element subtype-judgement {commonattrs, id?, type-expr, type-expr}
number-judgement-content =
  element number-judgement {commonattrs, id?, number-expr-content, type-expr}
name-judgement-content =
  element name-judgement {commonattrs, id?, name-expr-content, type-expr}
application-judgement-content =
  element application-judgement {commonattrs, id?, type-expr, name-expr-content, bindings-content}

conversion-content =
  element conversion-decl {declaration, expr}

auto-rewrite-content =
  element auto-rewrite
   {auto-rewrite-attr?, declaration, rewrite-name-content+}
rewrite-name-content =
   element rewrite-name
   {commonattrs, rewrite-name-attrs?, name, resolution?, rewrite-name-spec?}
rewrite-name-spec =
   element rewrite-name-spec {type-expr | name | xsd:integer}
rewrite-name-attrs =
   attribute kind {string "lazy" | string "eager" | string "macro"}

lib-decl-content =
  element lib-decl {id, typed-declaration}

theory-decl-content =
  element theory-decl {id, typed-declaration}

type-decl-content =
  element type-decl
    # The type-value is the given type, the type-expr is its canonical form
    #                              type-value  type-expr
    {type-decl-attrs, id, declaration, type-expr?, type-expr?, contains-content?}

type-from-decl-content =
  element type-from-decl
    {type-decl-attrs, id, declaration, type-expr, contains-content?}

var-decl-content =
  element var-decl {id, typed-declaration}

const-decl-content =
  element const-decl {id, decl-formals-content?, typed-declaration, expr?}

decl-formals-content =
   element decl-formals {decl-formals1-content+}

decl-formals1-content =
   element formals {decl-formals2-content+}

decl-formals2-content =
   element formals {binding-content+}

macro-decl-content =
  element macro-decl {id, typed-declaration}

def-decl-content =
   element def-decl
   {id, decl-formals-content?, typed-declaration,
      def-expr, measure-expr, order-expr?}

def-expr = expr

measure-expr = expr

order-expr = expr

ind-decl-content =
  element ind-decl {id, typed-declaration}

corec-decl-content =
  element corec-decl {id, typed-declaration}

coind-decl-content =
  element coind-decl {id, typed-declaration}

formula-decl-content =
  element formula-decl {formula-decl-attr, id, declaration, expr}

axiom-decl-content =
  element axiom-decl {formula-decl-attr, id, declaration, expr}

tcc-decl-content =
  element tcc-decl {formula-decl-attr, id, declaration, expr}

formula-decl-attr =
  attribute kind { "assumption" | "axiom" | "challenge" | "claim" |
                   "conjecture" | "corollary" | "fact" | "formula" | "law" |
                   "lemma" | "obligation" | "postulate" | "proposition" |
                   "sublemma" | "theorem" }

inline-datatype-content =
  element inline-datatype {id, typed-declaration}

declaration =
  commonattrs, chainattr?

type-decl =
  id, declaration

type-def-decl =
  type-decl-attrs, type-decl, type-expr, contains-content?

type-decl-attrs =
  attribute nonempty-type {boolean}?

contains-content =
  element contains
    {expr}
  
typed-declaration =
  declaration, declared-type, type-expr

declared-type = type-expr  

 */

sealed trait Type extends domain
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

sealed trait Expr extends assignment_arg with typearg
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
case class theory_name(id: String)
case class declref(href: String)

case class target(_theory: theory_name)

/*
theory-content =
  element theory
    {commonattrs,
     id,
     formals-content?,
     assuming-part?,
     theory-part?,
     exporting-content?}

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

formals-content =
  element formals {formals}

formals = (importings | formal)*

formal = importing-content
       | type-decl-content
       | formal-subtype-decl-content
       | formal-const-decl-content
       | formal-theory-decl-content

formal-subtype-decl-content =
  element formal-subtype-decl {type-def-decl}

formal-const-decl-content =
  element formal-const-decl {id, typed-declaration}

formal-theory-decl-content =
  element formal-theory-decl {theory-name}

theory-name-content =
  element theory-name {theory-name}

theory-name = id,
              actuals-content?,
              library-id-content?,
              mappings-content?

actuals-content =
  element actuals {(expr | type-expr | theory-name)+}

actual-content =
  element actual {expr | type-expr | theory-name}

library-id-content =
  element library-id {id}

mappings-content =
  element mappings {mapping+}

mapping = mapping-def-content | mapping-subst-content | mapping-rename-content

mapping-def-content =
  element mapping-def {mapping-lhs-content, mapping-rhs-content}

mapping-subst-content =
  element mapping-subst {mapping-lhs-content, mapping-rhs-content}

mapping-rename-content =
  element mapping-rename {mapping-lhs-content, mapping-rhs-content}

mapping-lhs-content =
  element mapping-lhs {name}

mapping-rhs-content =
  element mapping-rhs {expr-content | type-content | theory-name-content}

importings = importing-content*

importing-content =
  element importing
    {commonattrs,
     chainattr,
     theory-name}

exporting-content =
  element exporting
    {commonattrs,
     exporting-names?,
     exporting-but-names?,
     exporting-kind,
     exporting-theory-names?}

exporting-names =
  element exporting-names {names}

exporting-but-names =
  element exporting-but-names {names}

exporting-kind =
  element exporting-kind {"nil" | "all" | "closure" | "default"}

exporting-theory-names =
  element exporting-theory-names {theory-names}

theory-names = theory-name-content+

assuming-part =
  element assuming {assuming-decl+}

assuming-decl = theory-decl | assumption-content

assumption-content =
  element assumption {formula-decl-attr, id, declaration, expr}

theory-part = theory-decl+


### Names
names = name+
name = id, theory-id?, library-id?, actuals-content?, mappings-content?, target?
theory-id : FR String
library-id  FR: String

### Tokens

identifiers-content =
  element identifiers {commonattrs, identifier-content+}

identifier-content =
  element identifier {commonattrs, FR: String}

### Common Attributes
auto-rewrite-attr =
   attribute kind {string "plus" | string "minus"}

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