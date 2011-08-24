package info.kwarc.mmt.mizar.mmt.objects

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.lf._

import info.kwarc.mmt.mizar.mizar.translator.TranslationController

import info.kwarc.mmt.mizar.mizar.objects.ParsingController

object MMTResolve {
	def apply(aid : String, kind : String, absnr : Int) : Term = {
	 
			kind match {
			  case "R" | "K" | "M" => 
			    val lname =  aid+kind+absnr match {
			    	case "HIDDENM1" => "set" :: Nil	
			    	case "HIDDENR1" => "==" :: Nil
			    	case "HIDDENR2" => "in" :: Nil
			    	case _ => (aid + "_" + kind + "_" + absnr) :: "_args" :: Nil
			    }
			    OMID(MMTUtils.getPath(aid, lname))
			  case "V" => 
			    val lname = ParsingController.attributes(aid).get(absnr) match {
			      case Some(mstr) => (aid + "_" + "L" + "_" + mstr) :: "attr" :: Nil
			      case None => (aid + "_" + kind + "_" + absnr) :: "attr" :: Nil
			    }
			    OMID(MMTUtils.getPath(aid, lname))

			  case "L" => 
			    val lname = (aid + "_" + kind + "_" + absnr) :: "smode" :: Nil
			    OMID(MMTUtils.getPath(aid, lname))
			  case "U" =>
			    val mstrnr = ParsingController.selectors(aid)(absnr)
			    val lname = (aid + "_" + "L" + "_" + mstrnr) :: "sel" :: Nil
			    OMID(MMTUtils.getPath(aid, lname))
			  case "G" => 
			    val lname = (aid + "_" + "L" + "_" + absnr) :: "saggr" :: Nil
			    OMID(MMTUtils.getPath(aid, lname))
			  
			}
	}
}

object MMTTerm {
	def apply(name : String) : Term = OMV(name) //TODO check 
}

object MMTAttribute {
	def apply(aid : String, kind : String, absnr : Int, value : Boolean) : Term = {
		value match {
		  case false => OMA(Mizar.constant("not"), List(MMTResolve(aid, kind, absnr)))//TODO
		  case true => MMTResolve(aid, kind, absnr)
		}
	}
}

object MMTFunc {
	def apply(f : Term, args : List[Term]) : Term = args.length match {
		case 0 => f
		case _ => OMA(f,args)
	}
}

object MMTCluster {
	def apply(adjs : List[Term]) : Option[Term] = {
	  adjs.length match {
	    case 0 => None
	    case _ =>  Some(clusterAttrs(adjs))		
	  }
	}

	private def clusterAttrs(adjs : List[Term]) : Term = {
		adjs match {
			case hd :: (hd2 :: tl) => OMA(Mizar.constant("and"), hd :: clusterAttrs(hd2 :: tl) :: Nil)
			case hd :: Nil => hd
			case Nil => OMV("error in cluster attrs")
		}
	}

}
