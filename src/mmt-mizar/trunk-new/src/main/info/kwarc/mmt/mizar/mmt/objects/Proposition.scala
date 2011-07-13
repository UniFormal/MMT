package info.kwarc.mmt.mizar.mmt.objects

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._


object MMTNot {
	def apply(form : Term) = OMA(Mizar.constant("not"), form :: Nil)
}

object MMTAnd {
	def apply(forms : List[Term]) = OMA(Mizar.constant("and"), forms) 
}

object MMTFor {
	def apply(vname : String, tp : Term, form : Term) = OMBIND(Mizar.constant("for"), Context(TermVarDecl(vname, Some(tp), None)), form)
}

object MMTPred {
	def apply(name : Option[String], aid : String, absnr : Int, kind : String, vars : List[Term]) =  {
		name match {
			case None => OMA(OMID(MMTUtils.getPath(aid, kind, absnr)), vars)
			case Some(n) => OMA(OMID(MMTUtils.getPath(aid, n)), vars)
		}
	}
}
/*
object MMTPrivPred {
	def apply(vars : List[Term], form : Term) = 
}
*/
object MMTIs {
	def apply(tm : Term, tp : Term) = OMA(Mizar.constant("is"), tm :: tp :: Nil)
}

object MMTVerum {
	def apply() = Mizar.constant("true")
}
object MMTErrorFrm {
	def apply() = Mizar.constant("err") //TODO
}