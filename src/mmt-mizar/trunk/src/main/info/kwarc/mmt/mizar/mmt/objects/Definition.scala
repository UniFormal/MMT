package info.kwarc.mmt.mizar.mmt.objects

import info.kwarc.mmt.mizar.mizar.objects._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.lf._
import info.kwarc.mmt.api.patterns._
import objects.Conversions._

import info.kwarc.mmt.mizar.mizar.translator._


/* Params Utils */

object MMTArgs {
	def apply(nr : String, args : String, retType : Option[String]) : Context = {
		retType match {
			case Some(s) => Context(TermVarDecl(nr, None, None),
					                                   	SeqVarDecl(args, Some(Rep(Mizar.constant("tp"),OMV(nr))), None), 
					                                   	TermVarDecl(s, Some(Mizar.constant("tp")), None))
			case None => Context(TermVarDecl(nr, None, None),
					                               SeqVarDecl(args, Some(Rep(Mizar.constant("tp"),OMV(nr))), None))
		}
	}
}

object MMTCases {
	def apply(nr : String, cases : String, results : String, default : Option[String]) : Context  = {
		default match {
			case Some(s) => Context(
					   TermVarDecl(nr, None, None),
					   SeqVarDecl(cases, Some(Rep(Mizar.constant("prop"), OMV(nr))), None) ,
					   SeqVarDecl(results,Some(Rep(Mizar.constant("set"), OMV(nr))), None),
					   TermVarDecl(s, Some(Mizar.constant("set")), None))
			case None => Context(
					   TermVarDecl(nr, None, None),
					   SeqVarDecl(cases, Some(Rep(Mizar.constant("prop"), OMV(nr))), None) ,
					   SeqVarDecl(results,Some(Rep(Mizar.constant("set"),OMV(nr))), None))
		}
	}
}

/* Elaboration Utils */
/*For Func/Mode/Pred definitions */

object MMTArgsElab {
	def apply(argsNr : String) : Context = {
			Context(TermVarDecl("_args", Some(OMA(LF.arrow, List(SeqSubst(Mizar.constant("set"), "i", SeqUpTo(OMV("args"))),  Mizar.constant("set")))), None))
	}
}

object MMTArgTypesElab {
	def apply(argsNr : String, argTypes : String, retType : Term)  :  Context  = {
		Context(TermVarDecl("_types", Some(OMA(LF.arrow, List(
								SeqSubst(Pi("x", 
											Mizar.constant("set"),
											OMA(Mizar.constant("is"), List(OMV("x"),Index(OMV(argTypes),OMV("i"))))
											),
											"i",
										SeqUpTo(OMV(argsNr))
										),
								Pi("r", Mizar.constant("set"), OMA(Mizar.constant("is"), List(OMV("r"), retType)))
										
									))), None))
	}
}

object MMTMeansElab {
	def apply(argNr : String, argTypes : String, caseNr : String, cases : String, results : String, defResult : Option[String]) : Context = {
		defResult match {
			case Some(s) => 
		Context(TermVarDecl("_means", Some(OMA(LF.arrow, List(
				SeqSubst(Pi("x", Mizar.constant("set"), OMA(Mizar.constant("is"), List(OMV("x"), Index(OMV(argTypes), OMV("i"))))), "i", SeqUpTo(OMV(argNr))),
				OMA(Mizar.constant("proof"),List(OMA(Mizar.constant("And"),List(
						SeqSubst(OMA(Mizar.constant("implies"), List(
								Index(OMV(cases), OMV("i")),
								Index(OMV(results), OMV("i"))
								)), "i", SeqUpTo(OMV(caseNr))), 
						OMA(Mizar.constant("Implies"), List(
								OMA(Mizar.constant("And"), List(SeqSubst(OMA(Mizar.constant("Not"), List(Index(OMV(cases), OMV("i")))), "i", SeqUpTo(OMV(caseNr)))  
								    )),
								OMV(s)
								))
						))))
				))), None))
				case None => Context(
						TermVarDecl("_means", Some(OMA(LF.arrow, List(
								SeqSubst(Pi("x", Mizar.constant("set"), OMA(Mizar.constant("is"), List(OMV("x"), Index(OMV(argTypes), OMV("i"))))), "i", SeqUpTo(OMV(argNr))),
								OMA(Mizar.constant("proof"),List(OMA(Mizar.constant("And"),List(
										SeqSubst(OMA(Mizar.constant("implies"), List(
												Index(OMV(cases), OMV("i")),
												Index(OMV(results), OMV("i"))
										)), "i", SeqUpTo(OMV(caseNr)))
								))))
						))), None),
						TermVarDecl("_cases", Some(OMA(LF.arrow, List(
								SeqSubst(Pi("x", Mizar.constant("set"), OMA(Mizar.constant("is"), List(OMV("x"), Index(OMV(argTypes), OMV("i"))))), "i", SeqUpTo(OMV(argNr))),
								OMA(Mizar.constant("proof"), List(OMA(Mizar.constant("or"),List(
											SeqSubst(Index(OMV(cases), OMV("i")),"i", SeqUpTo(OMV(caseNr)))
										))))
								))), None)
				)
		}
	}
}

object MMTAttrElab {
	def apply(arg : String, ret : String): Term = {
			val varName = ParsingController.dictionary.getFreeVar()	
			OMA(Mizar.constant("proof"), OMBIND(Mizar.constant("for"), Context(TermVarDecl(varName, Some(Mizar.constant("set")), None)), OMA(LF.constant("arrow"), OMA(Mizar.constant("adjective"),OMV(arg) :: OMV("_args") :: Nil) :: OMA(Mizar.constant("is"), OMV(varName) :: OMV(ret) :: Nil) :: Nil)) :: Nil) //TODO referencing in patterns   			  
			//OMA(Mizar.constant("proof"), Nil)
			//OMV("arg")
	}
}

/* Patterns */
object DefPatterns {	
		
	val MizPredMeansPartialDef : Pattern = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizPredMeansPartialDef"), 
		   			    MMTArgs("n", "args", None) ++ MMTCases("m", "cases", "results", Some("default")), 
						MMTArgsElab("n") ++ MMTArgTypesElab("n", "args", Mizar.constant("prop")) ++ MMTMeansElab("n", "args", "m", "cases", "results", Some("default"))
		  )
	val MizPredMeansCompleteDef : Pattern = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizPredMeansCompleteDef"), 
		   			    MMTArgs("n", "args", None) ++ MMTCases("m", "cases", "results", None), 
						MMTArgsElab("n") ++ MMTArgTypesElab("n", "args", Mizar.constant("prop")) ++ MMTMeansElab("n", "args", "m", "cases", "results", None)
		  )
	
	val MizPredIsPartialDef : Pattern = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizPredIsPartialDef"), 
		   			    MMTArgs("n", "args", None) ++ MMTCases("m", "cases", "results", Some("default")), 
						MMTArgsElab("n") ++ MMTArgTypesElab("n", "args", Mizar.constant("prop")) ++ MMTMeansElab("n", "args", "m", "cases", "results", Some("default"))
		  )
	val MizPredIsCompleteDef : Pattern = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizPredIsCompleteDef"), 
		   			    MMTArgs("n", "args", None) ++ MMTCases("m", "cases", "results", None), 
						MMTArgsElab("n") ++ MMTArgTypesElab("n", "args", Mizar.constant("prop")) ++ MMTMeansElab("n", "args", "m", "cases", "results", None)
		  )
	
	
	val MizFuncMeansPartialDef : Pattern = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizFuncMeansPartialDef"), 
		   			    MMTArgs("n", "args", Some("retType")) ++ MMTCases("m", "cases", "results", Some("default")), 
						MMTArgsElab("n") ++ MMTArgTypesElab("n", "args", "retType") ++ MMTMeansElab("n", "args", "m", "cases", "results", Some("default"))
		  )
	val MizFuncMeansCompleteDef : Pattern = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizFuncMeansCompleteDef"), 
		   			    MMTArgs("n", "args", Some("retType")) ++ MMTCases("m", "cases", "results", None), 
						MMTArgsElab("n") ++ MMTArgTypesElab("n", "args", "retType") ++ MMTMeansElab("n", "args", "m", "cases", "results", None)
		  )
	
	val MizFuncIsPartialDef : Pattern = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizFuncIsPartialDef"), 
		   			    MMTArgs("n", "args", Some("retType")) ++ MMTCases("m", "cases", "results", Some("default")), 
						MMTArgsElab("n") ++ MMTArgTypesElab("n", "args", "retType") ++ MMTMeansElab("n", "args", "m", "cases", "results", Some("default"))
		  )
	val MizFuncIsCompleteDef : Pattern = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizFuncIsCompleteDef"), 
		   			    MMTArgs("n", "args", Some("retType")) ++ MMTCases("m", "cases", "results", None), 
						MMTArgsElab("n") ++ MMTArgTypesElab("n", "args", "retType") ++ MMTMeansElab("n", "args", "m", "cases", "results", None)
		  )
	val MizModeMeansPartialDef : Pattern = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizModeMeansPartialDef"), 
		   			    MMTArgs("n", "args", None) ++ MMTCases("m", "cases", "results", Some("default")), 
						MMTArgsElab("n") ++ MMTArgTypesElab("n", "args", Mizar.constant("tp")) ++ MMTMeansElab("n", "args", "m", "cases", "results", Some("default"))
		  )
	val MizModeMeansCompleteDef : Pattern = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizModeMeansCompleteDef"), 
		   			    MMTArgs("n", "args", None) ++ MMTCases("m", "cases", "results", None), 
						MMTArgsElab("n") ++ MMTArgTypesElab("n", "args", Mizar.constant("tp")) ++ MMTMeansElab("n", "args", "m", "cases", "results", None)
		  )
	
	val MizModeIsPartialDef : Pattern = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizModeIsPartialDef"), 
		   			    MMTArgs("n", "args", None) ++ MMTCases("m", "cases", "results", Some("default")), 
						MMTArgsElab("n") ++ MMTArgTypesElab("n", "args", Mizar.constant("tp")) ++ MMTMeansElab("n", "args", "m", "cases", "results", Some("default"))
		  )
	val MizModeIsCompleteDef : Pattern = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizModeIsCompleteDef"), 
		   			    MMTArgs("n", "args", None) ++ MMTCases("m", "cases", "results", None), 
						MMTArgsElab("n") ++ MMTArgTypesElab("n", "args", Mizar.constant("tp")) ++ MMTMeansElab("n", "args", "m", "cases", "results", None)
		  )
	
	val MizAttrMeansPartialDef : Pattern = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizAttrMeansPartialDef"), 
		   			    Context(TermVarDecl("arg", Some(Mizar.constant("tp")), None), TermVarDecl("ret", Some(Mizar.constant("tp")), None)) ++ 
		   			    	MMTCases("m", "cases", "results", Some("default")), 
		   			    Context(TermVarDecl("_args", Some(OMA(Mizar.constant("attr"), "arg" :: Nil)), None)) ++ 
		   			    Context(TermVarDecl("_types", Some(MMTAttrElab("arg", "ret")), None)) ++ 
							MMTMeansElab("n", "args", "m", "cases", "results", Some("default"))
		  )
	
	val MizAttrMeansCompleteDef : Pattern = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizAttrMeansCompleteDef"), 
		   			    Context(TermVarDecl("arg", Some(Mizar.constant("tp")), None), TermVarDecl("ret", Some(Mizar.constant("tp")), None)) ++ 
		   			    	MMTCases("m", "cases", "results", Some("default")), 
		   			    Context(TermVarDecl("_args", Some(OMA(Mizar.constant("attr"), "arg" :: Nil)), None)) ++ 
		   			    Context(TermVarDecl("_types", Some( { val varName = ParsingController.dictionary.getFreeVar()	
		OMA(Mizar.constant("proof"), OMBIND(Mizar.constant("for"), Context(TermVarDecl(varName, Some(Mizar.constant("set")), None)), OMA(LF.constant("arrow"), OMA(Mizar.constant("adjective"),OMV("arg") :: OMV("_args") :: Nil) :: OMA(Mizar.constant("is"), OMV(varName) :: OMV("ret") :: Nil) :: Nil)) :: Nil) //TODO referencing in patterns
		   			    }), None)) ++ 
							MMTMeansElab("n", "args", "m", "cases", "results", Some("default"))
		  )
	
	val MizAttrIsPartialDef : Pattern = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizAttrIsPartialDef"), 
		   			    Context(TermVarDecl("arg", Some(Mizar.constant("tp")), None), TermVarDecl("ret", Some(Mizar.constant("tp")), None)) ++ 
		   			    	MMTCases("m", "cases", "results", Some("default")), 
		   			    Context(TermVarDecl("_args", Some(OMA(Mizar.constant("attr"), "arg" :: Nil)), None)) ++ 
		   			    Context(TermVarDecl("_types", Some( { val varName = ParsingController.dictionary.getFreeVar()	
		OMA(Mizar.constant("proof"), OMBIND(Mizar.constant("for"), Context(TermVarDecl(varName, Some(Mizar.constant("set")), None)), OMA(LF.constant("arrow"), OMA(Mizar.constant("adjective"),OMV("arg") :: OMV("_args") :: Nil) :: OMA(Mizar.constant("is"), OMV(varName) :: OMV("ret") :: Nil) :: Nil)) :: Nil) //TODO referencing in patterns
		   			    }), None)) ++ 
							MMTMeansElab("n", "args", "m", "cases", "results", Some("default"))
		  )
	
	val MizAttrIsCompleteDef : Pattern = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizAttrIsCompleteDef"), 
		   			    Context(TermVarDecl("arg", Some(Mizar.constant("tp")), None), TermVarDecl("ret", Some(Mizar.constant("tp")), None)) ++ 
		   			    	MMTCases("m", "cases", "results", Some("default")), 
		   			    Context(TermVarDecl("_args", Some(OMA(Mizar.constant("attr"), "arg" :: Nil)), None)) ++ 
		   			    Context(TermVarDecl("_types", Some( { val varName = ParsingController.dictionary.getFreeVar()	
		OMA(Mizar.constant("proof"), OMBIND(Mizar.constant("for"), Context(TermVarDecl(varName, Some(Mizar.constant("set")), None)), OMA(LF.constant("arrow"), OMA(Mizar.constant("adjective"),OMV("arg") :: OMV("_args") :: Nil) :: OMA(Mizar.constant("is"), OMV(varName) :: OMV("ret") :: Nil) :: Nil)) :: Nil) //TODO referencing in patterns
		   			    }), None)) ++ 
							MMTMeansElab("n", "arg", "m", "cases", "results", Some("default"))
		  )
	
	val MizStructDef : Pattern = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("MizStructDef"),
						MMTArgs("nr_a","args",None) ++ 
						     Context(TermVarDecl("nr_str",None,None),
						    		 TermVarDecl("strs",None, None)) ++
						     Context(TermVarDecl("nr_sel", None, None),
						         TermVarDecl("sels", Some(Mizar.constant("tp")), None)),
						Context(TermVarDecl("str_theo", None, None))		
		)

}