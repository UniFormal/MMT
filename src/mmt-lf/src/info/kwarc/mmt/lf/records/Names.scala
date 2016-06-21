package info.kwarc.mmt.lf.records

import info.kwarc.mmt.lf._
import info.kwarc.mmt.api._
import objects._

object Names {
   val _base = LF._base
   val _path = _base ? "Records"
   def formation = _path ? "type"
}

object Formation {
   def path = Names._path ? "Record"
   def apply(tp: Term) = OMA(OMS(path), List(tp))
}

object Intro {
   def path = Names._path ? "record"
   def apply(tp: Term, fields: List[Term]) = OMA(OMS(path), tp :: fields)
}

object Elim {
   def path = Names._path ? "projection"
   def apply(tp: Term, field: LocalName) = OMA(OMS(path), List(tp, OML(VarDecl(field,None,None,None))))
}
 

