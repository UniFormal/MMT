package info.kwarc.mmt.mizar.mmt.objects

import info.kwarc.mmt.mizar.mizar.translator._

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.lf._



object  MMTRCluster {
	def apply(nr :Int, typ : Term, cluster : Term) {
		val cName = TranslationController.currentAid + "_RReg_" + nr
		val df = OMA(Mizar.constant("ded"), OMA(Mizar.constant("ex"),OMA(Mizar.constant("adjective"), typ :: cluster :: Nil) :: (Lambda("x", Mizar.constant("any"), Mizar.constant("true"))) :: Nil) :: Nil) 
		val c = new Constant(OMMOD(TranslationController.currentTheory), LocalName(cName),Some(df), None, null)
		TranslationController.add(c)
	}
}

object  MMTFCluster {
	def apply(nr :Int, functor : Term, args : List[Term], cluster : Term) {
		val cName = TranslationController.currentAid + "_FReg_" + nr 
		val df = OMA(Mizar.constant("ded"), OMA(Mizar.constant("ex"),OMA(Mizar.constant("adjective"), OMA(functor, args) :: cluster :: Nil) :: (Lambda("x", Mizar.constant("any"), Mizar.constant("true"))) :: Nil) :: Nil) 

		val c = new Constant(OMMOD(TranslationController.currentTheory), LocalName(cName),Some(df), None, null)
		TranslationController.add(c)
	}
}

object  MMTCCluster {
	def apply(nr :Int, typ : Term, first : Term, second : Term) {
		val cName = TranslationController.currentAid + "_CReg_" + nr 
		val df = OMA(Mizar.constant("ded"), OMA(Mizar.constant("for"),OMA(Mizar.constant("adjective"), typ :: first :: Nil) :: (Lambda("x", Mizar.constant("any"), OMA(Mizar.constant("is"), OMV("x") :: OMA(Mizar.constant("adjective"), typ :: second :: Nil) :: Nil ))) :: Nil) :: Nil) 

		val c = new Constant(OMMOD(TranslationController.currentTheory), LocalName(cName),Some(df), None, null)
		TranslationController.add(c)
	}
}
