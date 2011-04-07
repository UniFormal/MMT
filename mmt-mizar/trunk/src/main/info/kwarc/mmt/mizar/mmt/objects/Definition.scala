package mmt.objects
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.lf._

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
	def apply(args : List[(Option[String],Term)], retType : Term, funcPath : GlobalName) : Term = {
		genTypeDef(args, retType, funcPath, Nil)
	}
	
	private def genTypeDef(args : List[(Option[String],Term)], retType : Term, funcPath : GlobalName, vars : List[Term]) : Term = {
		args match {
			case hd :: tl => {
				val name = hd._1 match {
					case Some(nm) => nm 
					case None => "x" + args.length.toString
				}
				Pi(name, Mizar.constant("set"), OMA(LF.constant("arrow"), OMA(Mizar.constant("is"), OMV(name) :: hd._2 :: Nil) :: genTypeDef(tl, retType, funcPath, OMV(name) :: vars) :: Nil))
			}
			case Nil => {
				OMA(Mizar.constant("is"), OMA(OMID(funcPath), vars.reverse) :: retType :: Nil)
			}
		}
	}
}

object MMTDefMeans {
	def apply( args : List[(Option[String], Term)], form : Term) : Term = {
		genPropDef(args, form, Nil)
	}
	private def genPropDef(args : List[(Option[String],Term)], form : Term, vars : List[Term]) : Term = {
		args match {
			case hd :: tl => {
				val name = hd._1 match {
					case None => "x" +args.length
					case Some(nm) => nm
				}
				val tm = hd._2
				Pi(name, Mizar.constant("set"), OMA(LF.constant("arrow"), OMA(Mizar.constant("is"), OMV(name) :: tm :: Nil) :: genPropDef(tl, form, OMV(name) :: vars) :: Nil))
			}
			case Nil => {
				OMA(Mizar.constant("proof"), form :: Nil)
			}
		}
	}
}