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

import info.kwarc.mmt.mizar.mizar.translator.TranslationController

import info.kwarc.mmt.mizar.mizar.objects.ParsingController

object MMTResolve {
	def apply(aid : String, kind : String, absnr : Int) : Term = {
	 
			kind match {
			  case "R" => 
			    val lname =  aid+kind+absnr match {
			    	case "HIDDENR1" => "R1" :: Nil
			    	case "HIDDENR2" => "R2" :: Nil
			    	case _ => (kind + absnr) :: "pred" :: Nil
			    }
			    OMID(MMTUtils.getPath(aid, lname))
			  case "M" => 
			    val lname =  aid+kind+absnr match {
			    	case "HIDDENM1" => "M1" :: Nil	
			    	case _ => (kind + absnr) :: "mode" :: Nil
			    }
			    OMID(MMTUtils.getPath(aid, lname))
			  case "K" => 
			    val lname = (kind + absnr) :: "func" :: Nil
			    OMID(MMTUtils.getPath(aid, lname))
			  case "V" => 
			    
			    val lname = try {ParsingController.attributes(aid).get(absnr) match {
			      case Some(mstr) =>  ("L" + mstr) :: "strict" :: Nil
			      case None => (kind + absnr) :: "attr" :: Nil
			    }} catch {
			      case _ : Throwable =>  (kind + absnr) :: "attr" :: Nil
			    }
			    OMID(MMTUtils.getPath(aid, lname))

			  case "L" => 
			    val lname = (kind + absnr) :: "struct" :: Nil
			    OMID(MMTUtils.getPath(aid, lname))
			  case "U" =>
			    val lname = ("U" + absnr) :: ("sel") :: Nil
			    OMID(MMTUtils.getPath(aid, lname))
			  case "G" => 
			    val lname = ("L" + absnr) :: "aggr" :: Nil
			    OMID(MMTUtils.getPath(aid, lname))
			  
			}
	}
}

object MMTTerm {
	def apply(name : String) : Term = OMV(name) 
}

object MMTAttribute {
	def apply(aid : String, kind : String, absnr : Int, value : Boolean) : Term = {
		value match {
		  case false => Mizar.apply(Mizar.constant("neg-attr"), MMTResolve(aid, kind, absnr))
		  case true => MMTResolve(aid, kind, absnr)
		}
	}
}

object MMTFunc {
	def apply(f : Term, args : List[Term]) : Term = args.length match {
		case 0 => f
		case _ => Mizar.apply(f, args : _*)
	}
}

object MMTCluster {
	def apply(adjs : List[Term]) :Term = {
	  adjs.length match {
	    case 0 => MMTUtils.nullAttribute
	    case _ =>  clusterAttrs(adjs)		
	  }
	}

	private def clusterAttrs(adjs : List[Term]) : Term = {
		adjs match {
			case hd :: (hd2 :: tl) => Mizar.cluster(hd, clusterAttrs(hd2 :: tl))
			case hd :: Nil => hd
			case Nil => OMV("error in cluster attrs")
		}
	}

}
