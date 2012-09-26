package info.kwarc.mmt.mizar.mmt.objects

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.lf._


object MMTNot {
	def apply(form : Term) = Mizar.apply(Mizar.constant("not"), form)
}

object MMTAnd {
	def apply(forms : List[Term]) = Mizar.apply(Mizar.constant("and"), forms :_ *) 
}

object MMTFor {
	def apply(vname : String, tp : Term, form : Term) = Mizar.forall(vname, tp, form)
}

//only for queries, so exists is allowed
object MMTExists {
	def apply(vname : String, tp : Term, form : Term) = OMBIND(Mizar.apply(Mizar.constant("ex"), tp), Context(VarDecl(LocalName(vname), Some(Mizar.any), None)), form)
}

object MMTPred {
	def apply(aid : String, absnr : Int, kind : String, vars : List[Term]) =  vars.length match {
	  case 0 => MMTResolve(aid, kind, absnr)
	  case _ => Mizar.apply(MMTResolve(aid, kind, absnr), vars : _*)
	}
}
/*
object MMTPrivPred {
	def apply(vars : List[Term], form : Term) = 
}
*/
object MMTIs {
	def apply(tm : Term, tp : Term) = Mizar.apply(Mizar.constant("is"), tm, tp)
}

object MMTVerum {
	def apply() = Mizar.constant("true")
}
object MMTErrorFrm {
	def apply() = Mizar.constant("err") //TODO
}