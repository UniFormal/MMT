package mmt.objects

import jomdoc._
import jomdoc.documents._
import jomdoc.utils._
import jomdoc.frontend._
import jomdoc.symbols._
import jomdoc.libraries._
import jomdoc.modules._
import jomdoc.objects._


object Mizar {
	val MizarTh = DPath(utils.mmt.baseURI.resolve("set_theories/mizar/mizar.omdoc")) ? "Mizar"
	val TarskiTh = DPath(utils.mmt.baseURI.resolve("set_theories/mizar/tarski.omdoc")) ? "Tarski"
	
	def constant(name : String) : Term = OMS(MizarTh ? name)   
	
	def compact(t : Term) : Term = {
		t match {
			//iff
			case OMA(OMS(SPath(MizarTh,LocalPath(List("and")))),OMA(OMS(SPath(MizarTh,LocalPath(List("not")))), OMA(OMS(SPath(MizarTh,LocalPath(List("and")))),t1 :: OMA(OMS(SPath(MizarTh,LocalPath(List("not")))), t2 :: Nil) :: Nil) :: Nil) :: OMA(OMS(SPath(MizarTh,LocalPath(List("not")))), OMA(OMS(SPath(MizarTh,LocalPath(List("and")))),t3 :: OMA(OMS(SPath(MizarTh,LocalPath(List("not")))), t4 :: Nil) :: Nil) :: Nil) :: Nil) => {
				if (t1 == t4 && t2 == t3) {
					OMA(constant("iff"), compact(t1) :: compact(t2) :: Nil)
				} else {
					OMA(constant("and") ,OMA(constant("imp"), compact(t1) :: compact(t2) :: Nil) :: OMA(constant("imp"), compact(t3) :: compact(t4) :: Nil) :: Nil)
				}
			}
			//imp
			case OMA(OMS(SPath(MizarTh,LocalPath(List("not")))), OMA(OMS(SPath(MizarTh,LocalPath(List("and")))),t1 :: OMA(OMS(SPath(MizarTh,LocalPath(List("not")))), t2 :: Nil) :: Nil) :: Nil) => { 
				  OMA(constant("imp"), compact(t1) :: compact(t2) :: Nil)
			}
			case OMA(f,args) => OMA(f, args.map(compact))
			case OMBIND(cons, tp, body) => OMBIND(cons, tp, compact(body))
			case _ => t
		}
	}
}

object MMTUtils {
	
	def getFilePath(aid : String) : DPath = DPath(utils.mmt.baseURI.resolve("set_theories/mizar/" + aid + ".omdoc"))
	def getTheoryPath(aid : String) : MPath = getFilePath(aid : String) ? aid
	def getPath(aid : String, kind : String, absnr : Int) : SPath = getTheoryPath(aid) ? (aid + "_" + kind+ "_" + absnr.toString)
}
