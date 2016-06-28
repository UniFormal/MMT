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
import info.kwarc.mmt.lf._
import info.kwarc.mmt.lfs._

import info.kwarc.mmt.morphisms._



object RegPatterns  {
  val MizExistentialReg = Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizExistentialRegistration"),
		  Context(VarDecl(LocalName("n"), None, None, None),
				  VarDecl(LocalName("argTypes"), Some(SeqMap(Mizar.tp, LocalName("i"), OMV("n"))), None, None),
				  VarDecl(LocalName("typ"), Some(Mizar.tp), None, None),
				  VarDecl(LocalName("cluster"), Some(Mizar.attr(OMV("typ"))), None, None)),
		  Context(VarDecl(LocalName("reg"), Some(MMTUtils.args("x", "n", MMTUtils.argTypes("x", "argTypes", "n",
		      Mizar.proof(Mizar.exists("q", 
		          Mizar.adjective(OMV("cluster"), OMV("typ")),
		          Mizar.constant("true")))))),
		      None, None)))
				  
  
  val MizFunctionalReg = Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizFunctionalRegistration"),
		  Context(VarDecl(LocalName("n"), None, None, None),
				  VarDecl(LocalName("argTypes"), Some(SeqMap(Mizar.tp, LocalName("i"), OMV("n"))), None, None),
				  VarDecl(LocalName("functor"), Some(Mizar.tp), None, None),
				  VarDecl(LocalName("cluster"), Some(MMTUtils.args("x", "n", MMTUtils.argTypes("x", "argTypes", "n",
				      Mizar.attr(Mizar.apply(OMV("functor"), OMV("x")))))), None, None)),
		  Context(VarDecl(LocalName("reg"), Some(MMTUtils.args("x", "n", MMTUtils.argTypes("x", "argTypes", "n",
		      Mizar.proof(Mizar.exists("q", 
		          Mizar.adjective(OMV("cluster"), Mizar.apply(OMV("functor"), OMV("x"))),
		          Mizar.constant("true")))))),
		      None, None)))
	  
    
  val MizConditionalReg = Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizConditionalRegistration"),
		  Context(VarDecl(LocalName("n"), None, None, None),
				  VarDecl(LocalName("argTypes"), Some(SeqMap(Mizar.tp, LocalName("i"), OMV("n"))), None, None),
				  VarDecl(LocalName("typ"), Some(Mizar.tp), None, None),
				  VarDecl(LocalName("first"), Some(Mizar.attr(OMV("typ"))), None, None),
				  VarDecl(LocalName("second"), Some(Mizar.attr(OMV("typ"))), None, None)),
		  Context(VarDecl(LocalName("reg"), Some(MMTUtils.args("x", "n", MMTUtils.argTypes("x", "argTypes", "n",
		      Mizar.proof(Mizar.implies(
		          Mizar.exists("q", Mizar.adjective(OMV("first"), OMV("typ")),Mizar.constant("true")),
		          Mizar.exists("q", Mizar.adjective(OMV("second"), OMV("typ")),Mizar.constant("true"))))))),          
		      None, None)))
}

object SchemePatterns {
  val MizSchemeDef = Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizSchemeDef"),
						Context(VarDecl(LocalName("n"), None, None, None),
					                 VarDecl(LocalName("args"), Some(SeqMap(Mizar.tp, LocalName("i"), OMV("n"))), None, None)) ++ 
					                 Context(VarDecl(LocalName("m"), None, None, None),
					                 VarDecl(LocalName("premises"), Some(SeqMap(Mizar.prop, LocalName("i"), OMV("m"))), None, None)) ++
						             Context(VarDecl(LocalName("prop"), Some(Mizar.prop), None, None)),
						Context(VarDecl(LocalName("scheme"), Some( MMTUtils.args("x", "n", MMTUtils.argTypes("x", "args", "n",
								            Mizar.proof(Mizar.implies(Mizar.and(OMV("premises"), Mizar.constant("true")),OMV("prop")))))),
						                    None, None)))
}