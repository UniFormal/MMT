package info.kwarc.mmt.pvs

import info.kwarc.mmt.api._
import utils._
import archives._ 

case class theory(place: String, id: String, children: List[Decl])

trait Decl
case class type_decl(place: String, nonempty_type: Boolean, id: String, children: List[type_name]) extends Decl
case class const_decl(place: String, chain_p: Boolean, id: String, children: List[Type]) extends Decl
case class exporting(exporting_kind: String) extends Decl

trait Type
case class type_name(place: String, id: String, theory_id: String) extends Type
case class function_type(_0: Type, _1: Type) extends Type
case class tuple_type(children: List[Type]) extends Type