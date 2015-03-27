package info.kwarc.mmt.pvs

import info.kwarc.mmt.api._
import utils._
import archives._ 

case class theory(place: String, id: String, children: List[Decl])

sealed trait Decl
case class type_decl(place: String, nonempty_type: Boolean, id: String, children: List[type_name]) extends Decl
case class const_decl(place: String, chain_p: Boolean, id: String, children: List[Type]) extends Decl
case class var_decl(place: String, chain_p: Boolean, id: String, children: List[Type]) extends Decl
case class axiom_decl(place: String, kind: String, id: String, _0: Expr) extends Decl
case class formula_decl(place: String, kind: String, id: String, _0: Expr) extends Decl
case class exporting(exporting_kind: String) extends Decl

sealed trait Type
case class type_name(place: String, id: String, theory_id: String) extends Type
case class function_type(_0: Type, _1: Type) extends Type
case class tuple_type(children: List[Type]) extends Type

sealed trait Expr
case class name_expr(place: String, id: String, _0: Type, _1: resolution) extends Expr
case class application(_0: Expr, _1: Expr) extends Expr
case class tuple_expr(children: List[Expr]) extends Expr

case class resolution(_0: theory_name, _1: declref)
case class theory_name(id: String)
case class declref(href: String)