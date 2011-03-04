package mmt.objects

import jomdoc._
import jomdoc.documents._
import jomdoc.utils._
import jomdoc.frontend._
import jomdoc.symbols._
import jomdoc.libraries._
import jomdoc.modules._
import jomdoc.objects._

object MMTType {
	def apply(aid : String, kind : String, absnr : Int) = OMS(MMTUtils.getPath(aid, kind, absnr))
}

object MMTTerm {
	def apply(name : String) = OMV(name) //TODO check 
}