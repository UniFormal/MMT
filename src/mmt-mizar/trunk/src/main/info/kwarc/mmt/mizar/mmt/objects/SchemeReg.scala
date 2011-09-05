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




object RegPatterns  {
  val MizExistentialReg = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizExistentialRegistration"),
		  Context(TermVarDecl("n", None, None),
				  SeqVarDecl("argTypes", Some(SeqSubst(Mizar.tp,"i", SeqUpTo(OMV("n")))), None),
				  TermVarDecl("typ", Some(Mizar.tp), None),
				  TermVarDecl("cluster", Some(Mizar.attr(OMV("typ"))), None)),
		  Context(TermVarDecl("reg", Some(MMTUtils.args("x", "n", MMTUtils.argTypes("x", "argTypes", "n",
		      Mizar.proof(Mizar.exists("q", 
		          Mizar.adjective(OMV("cluster"), OMV("typ")),
		          Mizar.constant("true")))))),
		      None)))
				  
  
  val MizFunctionalReg = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizFunctionalRegistration"),
		  Context(TermVarDecl("n", None, None),
				  SeqVarDecl("argTypes", Some(SeqSubst(Mizar.tp,"i", SeqUpTo(OMV("n")))), None),
				  TermVarDecl("functor", Some(Mizar.tp), None),
				  TermVarDecl("cluster", Some(MMTUtils.args("x", "n", MMTUtils.argTypes("x", "argTypes", "n",
				      Mizar.attr(OMA(OMV("functor"), List(SeqVar("x"))))))), None)),
		  Context(TermVarDecl("reg", Some(MMTUtils.args("x", "n", MMTUtils.argTypes("x", "argTypes", "n",
		      Mizar.proof(Mizar.exists("q", 
		          Mizar.adjective(OMV("cluster"), OMA(OMV("functor"), List(SeqVar("x")))),
		          Mizar.constant("true")))))),
		      None)))
	  
    
  val MizConditionalReg = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizConditionalRegistration"),
		  Context(TermVarDecl("n", None, None),
				  SeqVarDecl("argTypes", Some(SeqSubst(Mizar.tp,"i", SeqUpTo(OMV("n")))), None),
				  TermVarDecl("typ", Some(Mizar.tp), None),
				  TermVarDecl("first", Some(Mizar.attr(OMV("typ"))), None),
				  TermVarDecl("second", Some(Mizar.attr(OMV("typ"))), None)),
		  Context(TermVarDecl("reg", Some(MMTUtils.args("x", "n", MMTUtils.argTypes("x", "argTypes", "n",
		      Mizar.proof(Mizar.implies(
		          Mizar.exists("q", Mizar.adjective(OMV("first"), OMV("typ")),Mizar.constant("true")),
		          Mizar.exists("q", Mizar.adjective(OMV("second"), OMV("typ")),Mizar.constant("true"))))))),          
		      None)))
}


/*
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
		println(c)
		
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
*/

object SchemePatterns {
  val MizSchemeDef : Pattern = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizSchemeDef"),
						Context(TermVarDecl("n", None, None),
					                 SeqVarDecl("args", Some(SeqSubst(Mizar.tp,"i", SeqUpTo(OMV("n")))), None)) ++ 
					                 Context(TermVarDecl("m", None, None),
					                 SeqVarDecl("premises", Some(SeqSubst(Mizar.prop,"i", SeqUpTo(OMV("m")))), None)) ++
						             Context(TermVarDecl("prop", Some(Mizar.prop), None)),
						Context(TermVarDecl("scheme", Some( MMTUtils.args("x", "n", MMTUtils.argTypes("x", "args", "n",
								            Mizar.proof(Mizar.implies(Mizar.and(SeqVar("premises"), Mizar.constant("true")),OMV("prop")))))),
						                    None)))
}