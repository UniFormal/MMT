package mmt.objects
import jomdoc._
import jomdoc.documents._
import jomdoc.utils._
import jomdoc.frontend._
import jomdoc.symbols._
import jomdoc.libraries._
import jomdoc.modules._
import jomdoc.objects._



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
	def apply(aid : String, absnr : Int, kind : String, vars : List[Term]) =  OMA(OMS(MMTUtils.getPath(aid, kind, absnr)), vars)
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