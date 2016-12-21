package info.kwarc.mmt.focalize

import info.kwarc.mmt.api._
import utils._
import uom._
import objects._


object Focalize {
  val _base = DPath(URI("http", "focal.inria.fr"))
  val _path = _base ? "Focalize"


  val tp = _path ? "tp"
  val proof = _path ? "proof"
  val expr = _path ? "expr"
  
  val self = _path ? "Self"
  val prop = _path ? "prop"
  val fct = _path ? "fct"
  val prod = _path ? "prod"
  
  val and = _path ? "and"
  val or = _path ? "or"
  val not = _path ? "not"
  val implies = _path ? "implies"
  val equiv = _path ? "equiv"
  val all = _path ? "all"
  val ex = _path ? "ex"
  
  val if_expr = _path ? "if_expr"
  val fun = _path ? "fun"
  val app = _path ? "app"
  val tuple = _path ? "tuple"
  
  val int = _path ? "int"
  object intLiterals extends RepresentedRealizedType(OMS(int), StandardInt)
  
  val omittedProof = _path ? "omittedProof"
}