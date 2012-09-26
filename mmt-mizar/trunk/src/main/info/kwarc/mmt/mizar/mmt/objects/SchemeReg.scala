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
import info.kwarc.mmt.lf._
import info.kwarc.mmt.lfs._




object RegPatterns  {
  val MizExistentialReg = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizExistentialRegistration"),
		  Context(VarDecl(LocalName("n"), None, None),
				  VarDecl(LocalName("argTypes"), Some(SeqMap(Mizar.tp, LocalName("i"), OMV("n"))), None),
				  VarDecl(LocalName("typ"), Some(Mizar.tp), None),
				  VarDecl(LocalName("cluster"), Some(Mizar.attr(OMV("typ"))), None)),
		  Context(VarDecl(LocalName("reg"), Some(MMTUtils.args("x", "n", MMTUtils.argTypes("x", "argTypes", "n",
		      Mizar.proof(Mizar.exists("q", 
		          Mizar.adjective(OMV("cluster"), OMV("typ")),
		          Mizar.constant("true")))))),
		      None)))
				  
  
  val MizFunctionalReg = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizFunctionalRegistration"),
		  Context(VarDecl(LocalName("n"), None, None),
				  VarDecl(LocalName("argTypes"), Some(SeqMap(Mizar.tp, LocalName("i"), OMV("n"))), None),
				  VarDecl(LocalName("functor"), Some(Mizar.tp), None),
				  VarDecl(LocalName("cluster"), Some(MMTUtils.args("x", "n", MMTUtils.argTypes("x", "argTypes", "n",
				      Mizar.attr(Mizar.apply(OMV("functor"), OMV("x")))))), None)),
		  Context(VarDecl(LocalName("reg"), Some(MMTUtils.args("x", "n", MMTUtils.argTypes("x", "argTypes", "n",
		      Mizar.proof(Mizar.exists("q", 
		          Mizar.adjective(OMV("cluster"), Mizar.apply(OMV("functor"), OMV("x"))),
		          Mizar.constant("true")))))),
		      None)))
	  
    
  val MizConditionalReg = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizConditionalRegistration"),
		  Context(VarDecl(LocalName("n"), None, None),
				  VarDecl(LocalName("argTypes"), Some(SeqMap(Mizar.tp, LocalName("i"), OMV("n"))), None),
				  VarDecl(LocalName("typ"), Some(Mizar.tp), None),
				  VarDecl(LocalName("first"), Some(Mizar.attr(OMV("typ"))), None),
				  VarDecl(LocalName("second"), Some(Mizar.attr(OMV("typ"))), None)),
		  Context(VarDecl(LocalName("reg"), Some(MMTUtils.args("x", "n", MMTUtils.argTypes("x", "argTypes", "n",
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
						Context(VarDecl(LocalName("n"), None, None),
					                 VarDecl(LocalName("args"), Some(SeqMap(Mizar.tp, LocalName("i"), OMV("n"))), None)) ++ 
					                 Context(VarDecl(LocalName("m"), None, None),
					                 VarDecl(LocalName("premises"), Some(SeqMap(Mizar.prop, LocalName("i"), OMV("m"))), None)) ++
						             Context(VarDecl(LocalName("prop"), Some(Mizar.prop), None)),
						Context(VarDecl(LocalName("scheme"), Some( MMTUtils.args("x", "n", MMTUtils.argTypes("x", "args", "n",
								            Mizar.proof(Mizar.implies(Mizar.and(OMV("premises"), Mizar.constant("true")),OMV("prop")))))),
						                    None)))
}