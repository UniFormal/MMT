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

object MMTType {
	def apply(name : Option[String], aid : String, kind : String, absnr : Int) : Term = {
			name match {
			case Some(n) => OMID(MMTUtils.getPath(aid, n))
			case None => OMID(MMTUtils.getPath(aid, kind, absnr))
			}
	}
}

object MMTTerm {
	def apply(name : String) : Term = OMV(name) //TODO check 
}

object MMTAttribute {
	def apply(name : String, value : Boolean) : Term = {
			OMID(MMTUtils.getPath(TranslationController.currentAid, name))
	}
}


object MMTCluster {
	def apply(adjs : List[Term]) : Term = {
		//attr = any -> bool
		//adj : tp -> attr ->tp
		//cluster = adj tp attr1 cluster attr2 cluster attr3 ....) 
		clusterAttrs(adjs)		
	}

	private def clusterAttrs(adjs : List[Term]) : Term = {
		adjs match {
			case hd :: (hd2 :: tl) => OMA(Mizar.constant("cluster"), hd :: clusterAttrs(hd2 :: tl) :: Nil)
			case hd :: Nil => hd
			case Nil => OMSTR("Error in MMTCluster -> clusterAttrs -> nr of attrs is 0")
		}
	}
}