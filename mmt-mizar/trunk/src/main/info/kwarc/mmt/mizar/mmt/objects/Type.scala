package mmt.objects

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._

object MMTType {
	def apply(name : Option[String], aid : String, kind : String, absnr : Int) = {
		name match {
			case Some(n) => OMID(MMTUtils.getPath(aid, n))
			case None => OMID(MMTUtils.getPath(aid, kind, absnr))
		}
	}
}

object MMTTerm {
	def apply(name : String) = OMV(name) //TODO check 
}