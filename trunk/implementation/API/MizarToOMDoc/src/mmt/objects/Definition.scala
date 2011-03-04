package mmt.objects
import jomdoc._
import jomdoc.documents._
import jomdoc.utils._
import jomdoc.frontend._
import jomdoc.symbols._
import jomdoc.libraries._
import jomdoc.modules._
import jomdoc.objects._
import jomdoc.lf._

object MMTDefArgs {
	def apply(nr : Int, kind : String) : Term = {
		if (nr == 0) {
			kind match {
				case "R" => Mizar.constant("prop")
				case "M" => Mizar.constant("tp")
				case "K" | "L" | "O" => Mizar.constant("set")
				case _ => OMV("Error in mmt.objects -> MMTDefArgs")
			}
		} else if (nr == 1) {
			kind match {
				case "R" => OMA(LF.constant("arrow"), Mizar.constant("set") :: Mizar.constant("prop") :: Nil)
				case "M" => OMA(LF.constant("arrow"), Mizar.constant("set") :: Mizar.constant("tp") :: Nil)
				case "K" | "L" | "O" => OMA(LF.constant("arrow"), Mizar.constant("set") :: Mizar.constant("set") :: Nil)
				case _ => OMV("Error in mmt.objects -> MMTDefArgs")
			}
		} else {
			OMA(LF.constant("arrow"), Mizar.constant("set") :: apply(nr - 1, kind) :: Nil)
			
		}
	}
}

object MMTDefTypes {
	def apply(argTypes : List[Term], retType : Term, funcPath : SPath) : Term = {
		genTypeDef(argTypes, retType, funcPath, Nil)
	}
	
	private def genTypeDef(argTypes : List[Term], retType : Term, funcPath : SPath, vars : List[Term]) : Term = {
		argTypes match {
			case hd :: tl => {
				val name = "x" + argTypes.length.toString
				Pi(name, Mizar.constant("set"), OMA(LF.constant("arrow"), OMA(Mizar.constant("is"), OMV(name) :: hd :: Nil) :: genTypeDef(tl, retType, funcPath, OMV(name) :: vars) :: Nil))
			}
			case Nil => {
				OMA(Mizar.constant("is"), OMA(OMS(funcPath), vars.reverse) :: retType :: Nil)
			}
		}
	}
}

object MMTDefMeaning {
	def apply( argTypes : List[Term], form : Term) : Term = {
		genPropDef(argTypes, form, Nil)
	}
	private def genPropDef(argTypes : List[Term], form : Term, vars : List[Term]) : Term = {
		argTypes match {
			case hd :: tl => {
				val name = "x" + argTypes.length.toString
				Pi(name, Mizar.constant("set"), OMA(LF.constant("arrow"), OMA(Mizar.constant("is"), OMV(name) :: hd :: Nil) :: genPropDef(tl, form, OMV(name) :: vars) :: Nil))
			}
			case Nil => {
				OMA(Mizar.constant("proof"), form :: Nil)
			}
		}
	}
}