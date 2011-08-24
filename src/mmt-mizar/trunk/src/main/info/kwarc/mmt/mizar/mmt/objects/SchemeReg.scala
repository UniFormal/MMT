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
import info.kwarc.mmt.api.patterns._
import info.kwarc.mmt.api.lf._



object  MMTRCluster {
	def apply(aid : String, nr :Int, typ : Term, cluster : Option[Term]) {
		val cName = aid + "_RReg_" + nr
		val ftyp = cluster match {
		  case Some(cl) => OMA(Mizar.constant("adjective"), typ :: cl :: Nil)
		  case None => typ
		}
		val tp = OMA(Mizar.constant("proof"), OMA(Mizar.constant("ex"), ftyp:: (Lambda("x", Mizar.constant("any"), Mizar.constant("true"))) :: Nil) :: Nil) 
		val c = new Constant(OMMOD(TranslationController.currentTheory), LocalName(cName),Some(tp), None, Individual(None))
		TranslationController.add(c)
	}
}

object  MMTFCluster {
	
	def apply(aid : String, nr :Int, functor : Term, argNames : List[String], argTypes : List[Term], cluster : Option[Term]) {
		val cName = aid + "_FReg_" + nr 
		val args = argNames.zip(argTypes)
		val ftyp = cluster match {
		  case Some(cl) => OMA(Mizar.constant("adjective"), MMTFunc(functor, argNames.map(OMV)) :: cl :: Nil)
		  case None => MMTFunc(functor, argNames.map(OMV))
		}
		val tp = args.foldRight[Term](OMA(Mizar.constant("proof"), OMA(Mizar.constant("ex"), ftyp :: (Lambda("x", Mizar.constant("any"), Mizar.constant("true"))) :: Nil) :: Nil) 
)((x,r) => Pi(x._1,x._2,r)) 

		val c = new Constant(OMMOD(TranslationController.currentTheory), LocalName(cName),Some(tp), None, Individual(None))
		TranslationController.add(c)
	}
}

object  MMTCCluster {
	def apply(aid : String, nr :Int, typ : Term, first : Option[Term], second : Option[Term]) {
		val cName = aid + "_CReg_" + nr 
		
		val ftyp1 = first match {
		  case Some(cl) => OMA(Mizar.constant("adjective"), typ :: cl :: Nil)
		  case None => typ
		}
		
		val ftyp2 = second match {
		  case Some(cl) => OMA(Mizar.constant("adjective"), typ :: cl :: Nil)
		  case None => typ
		}
		
		val tp = OMA(Mizar.constant("proof"), OMA(Mizar.constant("for"), ftyp1 :: (Lambda("x", Mizar.constant("any"), OMA(Mizar.constant("is"), OMV("x") :: ftyp2 :: Nil ))) :: Nil) :: Nil) 

		val c = new Constant(OMMOD(TranslationController.currentTheory), LocalName(cName),Some(tp), None, Individual(None))
		TranslationController.add(c)
	}
}


object SchemePatterns {
  val MizSchemeDef : Pattern = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizSchemeDef"),
						Context(TermVarDecl("nr_a", None, None),
					                 TermVarDecl("args", Some(OMA(OMID(mmt.repetition), Mizar.constant("tp") :: OMV("nr_a") :: Nil)), None)) ++ 
					                 Context(TermVarDecl("nr_p", None, None),
					                 TermVarDecl("premises", Some(OMA(OMID(mmt.repetition), Mizar.constant("prop") :: OMV("nr_p") :: Nil)), None)) ++
						             Context(TermVarDecl("prop", Some(Mizar.constant("prop")), None)),
						Context(TermVarDecl("scheme", Some(OMA(LF.arrow, OMV("args") :: OMV("premises") :: OMV("prop") :: Nil)), None))		//TODO
		)
}