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
					                                   	TermVarDecl(args, Some(OMA(OMID(mmt.repetition), Mizar.constant("tp") :: OMV(nr) :: Nil)), None), 
					                                   	TermVarDecl(s, Some(Mizar.constant("tp")), None))
			case None => Context(TermVarDecl(nr, None, None),
					                               TermVarDecl(args, Some(OMA(OMID(mmt.repetition), Mizar.constant("tp") :: OMV(nr) :: Nil)), None))
		}
	}
}

object MMTCases {
	def apply(nr : String, cases : String, results : String, default : Option[String]) : Context  = {
		default match {
			case Some(s) => Context(
					   TermVarDecl(nr, None, None),
					   TermVarDecl(cases, Some(Rep(Mizar.constant("prop"), nr)), None) ,
					   TermVarDecl(results,Some(Rep(Mizar.constant("set"), nr)), None),
					   TermVarDecl(s, Some(Mizar.constant("set")), None))
			case None => Context(
					   TermVarDecl(nr, None, None),
					   TermVarDecl(cases, Some(Rep(Mizar.constant("prop"), nr)), None) ,
					   TermVarDecl(results,Some(Rep(Mizar.constant("set"),nr)), None))
		}
	}
}

/* Elaboration Utils */
/*For Func/Mode/Pred definitions */

object MMTArgsElab {
	def apply(argsNr : String) : Context = {
			Context(TermVarDecl("_args", Some(Arrow(Ellipsis(Mizar.constant("set"), "i", OMI(1), OMV(argsNr)),  Mizar.constant("set"))), None))
	}
}

object MMTArgTypesElab {
	def apply(argsNr : String, argTypes : String, retType : Term)  :  Context  = {
		Context(TermVarDecl("_types", Some(OMA(LF.arrow, List(
								Ellipsis(Pi("x", 
											Mizar.constant("set"),
											OMA(Mizar.constant("is"), List(OMV("x"),Index(OMV(argTypes),OMV("i"))))
											),
											"i", 
										OMI(1), 
										OMV(argsNr)
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
				Ellipsis(Pi("x", Mizar.constant("set"), OMA(Mizar.constant("is"), List(OMV("x"), Index(OMV(argTypes), OMV("i"))))), "i", OMI(1), OMV(argNr)),
				OMA(Mizar.constant("proof"),List(OMA(Mizar.constant("And"),List(
						Ellipsis(OMA(Mizar.constant("implies"), List(
								Index(OMV(cases), OMV("i")),
								Index(OMV(results), OMV("i"))
								)), "i", OMI(1), OMV(caseNr)), 
						OMA(Mizar.constant("Implies"), List(
								OMA(Mizar.constant("And"), List(Ellipsis(OMA(Mizar.constant("Not"), List(Index(OMV(cases), OMV("i")))), "i", OMI(1), OMV(caseNr))  )),
								OMV(s)
								))
						))))
				))), None))
				case None => Context(
						TermVarDecl("_means", Some(OMA(LF.arrow, List(
								Ellipsis(Pi("x", Mizar.constant("set"), OMA(Mizar.constant("is"), List(OMV("x"), Index(OMV(argTypes), OMV("i"))))), "i", OMI(1), OMV(argNr)),
								OMA(Mizar.constant("proof"),List(OMA(Mizar.constant("And"),List(
										Ellipsis(OMA(Mizar.constant("implies"), List(
												Index(OMV(cases), OMV("i")),
												Index(OMV(results), OMV("i"))
										)), "i", OMI(1), OMV(caseNr))
								))))
						))), None),
						TermVarDecl("_cases", Some(OMA(LF.arrow, List(
								Ellipsis(Pi("x", Mizar.constant("set"), OMA(Mizar.constant("is"), List(OMV("x"), Index(OMV(argTypes), OMV("i"))))), "i", OMI(1), OMV(argNr)),
								OMA(Mizar.constant("proof"), List(OMA(Mizar.constant("or"),List(
											Ellipsis(Index(OMV(cases), OMV("i")),"i", OMI(1), OMV(caseNr))
										))))
								))), None)
				)
		}
	}
}

object MMTAttrElab {
	def apply(arg : String, ret : String): Term = {
			val varName = ParsingController.dictionary.getFreeVar()	
			//OMA(Mizar.constant("proof"), OMBIND(Mizar.constant("for"), Context(TermVarDecl(varName, Some(Mizar.constant("set")), None)), OMA(LF.constant("arrow"), OMA(Mizar.constant("adjective"),OMV(arg) :: OMID(GlobalName(OMMOD(Mizar.MizarTh),LocalName("_args"))) :: Nil) :: OMA(Mizar.constant("is"), OMV(varName) :: OMV(ret) :: Nil) :: Nil)) :: Nil) //TODO referencing in patterns   			  
			OMA(Mizar.constant("proof"), Nil)
			//OMV("arg")
	}
}

/* Patterns */
object DefPatterns {	
		
	val MizPredMeansPartialDef : Pattern = new Pattern(OMMOD(Mizar.MizarTh), LocalName("MizPredMeansPartialDef"), 
		   			    Some( MMTArgs("n", "args", None) ++ MMTCases("m", "cases", "results", Some("default"))), 
						MMTArgsElab("n") ++ MMTArgTypesElab("n", "args", Mizar.constant("prop")) ++ MMTMeansElab("n", "args", "m", "cases", "results", Some("default"))
		  )
	val MizPredMeansCompleteDef : Pattern = new Pattern(OMMOD(Mizar.MizarTh), LocalName("MizPredMeansCompleteDef"), 
		   			    Some( MMTArgs("n", "args", None) ++ MMTCases("m", "cases", "results", None)), 
						MMTArgsElab("n") ++ MMTArgTypesElab("n", "args", Mizar.constant("prop")) ++ MMTMeansElab("n", "args", "m", "cases", "results", None)
		  )
	
	val MizPredIsPartialDef : Pattern = new Pattern(OMMOD(Mizar.MizarTh), LocalName("MizPredIsPartialDef"), 
		   			    Some( MMTArgs("n", "args", None) ++ MMTCases("m", "cases", "results", Some("default"))), 
						MMTArgsElab("n") ++ MMTArgTypesElab("n", "args", Mizar.constant("prop")) ++ MMTMeansElab("n", "args", "m", "cases", "results", Some("default"))
		  )
	val MizPredIsCompleteDef : Pattern = new Pattern(OMMOD(Mizar.MizarTh), LocalName("MizPredIsCompleteDef"), 
		   			    Some( MMTArgs("n", "args", None) ++ MMTCases("m", "cases", "results", None)), 
						MMTArgsElab("n") ++ MMTArgTypesElab("n", "args", Mizar.constant("prop")) ++ MMTMeansElab("n", "args", "m", "cases", "results", None)
		  )
	
	
	val MizFuncMeansPartialDef : Pattern = new Pattern(OMMOD(Mizar.MizarTh), LocalName("MizFuncMeansPartialDef"), 
		   			    Some( MMTArgs("n", "args", Some("retType")) ++ MMTCases("m", "cases", "results", Some("default"))), 
						MMTArgsElab("n") ++ MMTArgTypesElab("n", "args", "retType") ++ MMTMeansElab("n", "args", "m", "cases", "results", Some("default"))
		  )
	val MizFuncMeansCompleteDef : Pattern = new Pattern(OMMOD(Mizar.MizarTh), LocalName("MizFuncMeansCompleteDef"), 
		   			    Some( MMTArgs("n", "args", Some("retType")) ++ MMTCases("m", "cases", "results", None)), 
						MMTArgsElab("n") ++ MMTArgTypesElab("n", "args", "retType") ++ MMTMeansElab("n", "args", "m", "cases", "results", None)
		  )
	
	val MizFuncIsPartialDef : Pattern = new Pattern(OMMOD(Mizar.MizarTh), LocalName("MizFuncIsPartialDef"), 
		   			    Some( MMTArgs("n", "args", Some("retType")) ++ MMTCases("m", "cases", "results", Some("default"))), 
						MMTArgsElab("n") ++ MMTArgTypesElab("n", "args", "retType") ++ MMTMeansElab("n", "args", "m", "cases", "results", Some("default"))
		  )
	val MizFuncIsCompleteDef : Pattern = new Pattern(OMMOD(Mizar.MizarTh), LocalName("MizFuncIsCompleteDef"), 
		   			    Some( MMTArgs("n", "args", Some("retType")) ++ MMTCases("m", "cases", "results", None)), 
						MMTArgsElab("n") ++ MMTArgTypesElab("n", "args", "retType") ++ MMTMeansElab("n", "args", "m", "cases", "results", None)
		  )
	val MizModeMeansPartialDef : Pattern = new Pattern(OMMOD(Mizar.MizarTh), LocalName("MizModeMeansPartialDef"), 
		   			    Some( MMTArgs("n", "args", None) ++ MMTCases("m", "cases", "results", Some("default"))), 
						MMTArgsElab("n") ++ MMTArgTypesElab("n", "args", Mizar.constant("tp")) ++ MMTMeansElab("n", "args", "m", "cases", "results", Some("default"))
		  )
	val MizModeMeansCompleteDef : Pattern = new Pattern(OMMOD(Mizar.MizarTh), LocalName("MizModeMeansCompleteDef"), 
		   			    Some( MMTArgs("n", "args", None) ++ MMTCases("m", "cases", "results", None)), 
						MMTArgsElab("n") ++ MMTArgTypesElab("n", "args", Mizar.constant("tp")) ++ MMTMeansElab("n", "args", "m", "cases", "results", None)
		  )
	
	val MizModeIsPartialDef : Pattern = new Pattern(OMMOD(Mizar.MizarTh), LocalName("MizModeIsPartialDef"), 
		   			    Some( MMTArgs("n", "args", None) ++ MMTCases("m", "cases", "results", Some("default"))), 
						MMTArgsElab("n") ++ MMTArgTypesElab("n", "args", Mizar.constant("tp")) ++ MMTMeansElab("n", "args", "m", "cases", "results", Some("default"))
		  )
	val MizModeIsCompleteDef : Pattern = new Pattern(OMMOD(Mizar.MizarTh), LocalName("MizModeIsCompleteDef"), 
		   			    Some( MMTArgs("n", "args", None) ++ MMTCases("m", "cases", "results", None)), 
						MMTArgsElab("n") ++ MMTArgTypesElab("n", "args", Mizar.constant("tp")) ++ MMTMeansElab("n", "args", "m", "cases", "results", None)
		  )
	
	val MizAttrMeansPartialDef : Pattern = new Pattern(OMMOD(Mizar.MizarTh), LocalName("MizAttrMeansPartialDef"), 
		   			    Some( Context(TermVarDecl("arg", Some(Mizar.constant("tp")), None), TermVarDecl("ret", Some(Mizar.constant("tp")), None)) ++ 
		   			    	MMTCases("m", "cases", "results", Some("default"))), 
		   			    Context(TermVarDecl("_args", Some(OMA(Mizar.constant("attr"), "arg" :: Nil)), None)) ++ 
		   			    Context(TermVarDecl("_types", Some(MMTAttrElab("arg", "ret")), None)) ++ 
							MMTMeansElab("n", "args", "m", "cases", "results", Some("default"))
		  )
	
	val MizAttrMeansCompleteDef : Pattern = new Pattern(OMMOD(Mizar.MizarTh), LocalName("MizAttrMeansCompleteDef"), 
		   			    Some( Context(TermVarDecl("arg", Some(Mizar.constant("tp")), None), TermVarDecl("ret", Some(Mizar.constant("tp")), None)) ++ 
		   			    	MMTCases("m", "cases", "results", Some("default"))), 
		   			    Context(TermVarDecl("_args", Some(OMA(Mizar.constant("attr"), "arg" :: Nil)), None)) ++ 
		   			    Context(TermVarDecl("_types", Some( { val varName = ParsingController.dictionary.getFreeVar()	
		OMA(Mizar.constant("proof"), OMBIND(Mizar.constant("for"), Context(TermVarDecl(varName, Some(Mizar.constant("set")), None)), OMA(LF.constant("arrow"), OMA(Mizar.constant("adjective"),OMV("arg") :: OMID(GlobalName(OMMOD(Mizar.MizarTh),LocalName("_args"))) :: Nil) :: OMA(Mizar.constant("is"), OMV(varName) :: OMV("ret") :: Nil) :: Nil)) :: Nil) //TODO referencing in patterns
		   			    }), None)) ++ 
							MMTMeansElab("n", "args", "m", "cases", "results", Some("default"))
		  )
	
	val MizAttrIsPartialDef : Pattern = new Pattern(OMMOD(Mizar.MizarTh), LocalName("MizAttrMeansPartialDef"), 
		   			    Some( Context(TermVarDecl("arg", Some(Mizar.constant("tp")), None), TermVarDecl("ret", Some(Mizar.constant("tp")), None)) ++ 
		   			    	MMTCases("m", "cases", "results", Some("default"))), 
		   			    Context(TermVarDecl("_args", Some(OMA(Mizar.constant("attr"), "arg" :: Nil)), None)) ++ 
		   			    Context(TermVarDecl("_types", Some( { val varName = ParsingController.dictionary.getFreeVar()	
		OMA(Mizar.constant("proof"), OMBIND(Mizar.constant("for"), Context(TermVarDecl(varName, Some(Mizar.constant("set")), None)), OMA(LF.constant("arrow"), OMA(Mizar.constant("adjective"),OMV("arg") :: OMID(GlobalName(OMMOD(Mizar.MizarTh),LocalName("_args"))) :: Nil) :: OMA(Mizar.constant("is"), OMV(varName) :: OMV("ret") :: Nil) :: Nil)) :: Nil) //TODO referencing in patterns
		   			    }), None)) ++ 
							MMTMeansElab("n", "args", "m", "cases", "results", Some("default"))
		  )
	
	val MizAttrIsCompleteDef : Pattern = new Pattern(OMMOD(Mizar.MizarTh), LocalName("MizAttrMeansPartialDef"), 
		   			    Some( Context(TermVarDecl("arg", Some(Mizar.constant("tp")), None), TermVarDecl("ret", Some(Mizar.constant("tp")), None)) ++ 
		   			    	MMTCases("m", "cases", "results", Some("default"))), 
		   			    Context(TermVarDecl("_args", Some(OMA(Mizar.constant("attr"), "arg" :: Nil)), None)) ++ 
		   			    Context(TermVarDecl("_types", Some( { val varName = ParsingController.dictionary.getFreeVar()	
		OMA(Mizar.constant("proof"), OMBIND(Mizar.constant("for"), Context(TermVarDecl(varName, Some(Mizar.constant("set")), None)), OMA(LF.constant("arrow"), OMA(Mizar.constant("adjective"),OMV("arg") :: OMID(GlobalName(OMMOD(Mizar.MizarTh),LocalName("_args"))) :: Nil) :: OMA(Mizar.constant("is"), OMV(varName) :: OMV("ret") :: Nil) :: Nil)) :: Nil) //TODO referencing in patterns
		   			    }), None)) ++ 
							MMTMeansElab("n", "args", "m", "cases", "results", Some("default"))
		  )
	

}



// This should be moved
/*
object MMTDirectFuncDef {

	
	def apply(f: MizFuncIsDef, home : TheoryObj) : Instance = {
		val name = f.name match {
			case None => f.aid + "_K_" + f.absnr
			case Some(s) => s
		}
		val args = f.args.map(x => TypeTranslator.translateTyp(x._2))
		val retType = TypeTranslator.translateTyp(f.retType)
		val cases = f.cases.map(x => TypeTranslator.translateTerm(x._1))
		val results = f.cases.map(x => PropositionTranslator.translateFormula(x._2))
		val default = TypeTranslator.translateTerm(f.term)
		val matches = "n" / OMI(args.length) ++ ("args" / Seq(args : _*)) ++ "retType" / retType ++ 
								"m" / OMI(cases.length) ++  "cases" / Seq(cases : _*) ++ "results" / Seq(results : _*) ++ "defRes"  / default
		new Instance(home, LocalName(name), DefPatterns.pattern.home.toMPath ? DefPatterns.pattern.name, matches)
	}
}

object MMTDefArgs {
	def apply(nr : Int, kind : String) : Term = {
		if (nr == 0) {
			kind match {
				case "R" => Mizar.constant("prop")
				case "M" => Mizar.constant("tp")
				case "K" | "L" | "O" => Mizar.constant("set")
				case _ => OMV("Error in mmt.objects -> MMTDefArgs")
			}
		} else if (nr == 1) {
			kind match {
				case "R" => OMA(LF.constant("arrow"), Mizar.constant("set") :: Mizar.constant("prop") :: Nil)
				case "M" => OMA(LF.constant("arrow"), Mizar.constant("set") :: Mizar.constant("tp") :: Nil)
				case "K" | "L" | "O" => OMA(LF.constant("arrow"), Mizar.constant("set") :: Mizar.constant("set") :: Nil)
				case _ => OMV("Error in mmt.objects -> MMTDefArgs")
			}
		} else {
			OMA(LF.constant("arrow"), Mizar.constant("set") :: apply(nr - 1, kind) :: Nil)
			
		}
	}
}

object MMTDefAttrArgs {
	def apply(arg : Term) : Term = {
		OMA(Mizar.constant("attr"), arg :: Nil)
	}
}

object MMTDefAttrTypes {
	def apply(argType : Term, motherType : Term, path : GlobalName) : Term = {
		val varName = ParsingController.dictionary.getFreeVar()	
		OMA(Mizar.constant("proof"), OMBIND(Mizar.constant("for"), Context(TermVarDecl(varName, Some(Mizar.constant("set")), None)), OMA(LF.constant("arrow"), OMA(Mizar.constant("adjective"),argType :: OMID(path) :: Nil) :: OMA(Mizar.constant("is"), OMV(varName) :: motherType :: Nil) :: Nil)) :: Nil)
	}
}


object MMTDefAttrMeans {
	def apply(arg : (Option[String], Term), path : GlobalName, form : Term) : Term = {
		val name = arg._1 match {
					case Some(nm) => nm 
					case None => ParsingController.dictionary.getFreeVar()
				}
		Pi(name, OMA(Mizar.constant("adjective"), arg._2 :: OMID(path) :: Nil), OMA(Mizar.constant("proof"), form :: Nil))
	}
}

object MMTDefTypes {
	def apply(args : List[(Option[String],Term)], retType : Term, funcPath : GlobalName) : Term = {
		genTypeDef(args, retType, funcPath, Nil)
	}
	
	private def genTypeDef(args : List[(Option[String],Term)], retType : Term, funcPath : GlobalName, vars : List[Term]) : Term = {
		args match {
			case hd :: tl => {
				val name = hd._1 match {
					case Some(nm) => nm 
					case None => "x" + args.length.toString
				}
				Pi(name, Mizar.constant("set"), OMA(LF.constant("arrow"), OMA(Mizar.constant("is"), OMV(name) :: hd._2 :: Nil) :: genTypeDef(tl, retType, funcPath, OMV(name) :: vars) :: Nil))
			}
			case Nil => {
				OMA(Mizar.constant("is"), OMA(OMID(funcPath), vars.reverse) :: retType :: Nil)
			}
		}
	}
}

object MMTDefMeans {
	def apply( args : List[(Option[String], Term)], cases : List[(Term,Term)], form : Option[Term]) : Term = {
		genPropDef(args, form, Nil)
	}
	private def genPropDef(args : List[(Option[String],Term)], form : Term, vars : List[Term]) : Term = {
		args match {
			case hd :: tl => {
				val name = hd._1 match {
					case None => "x" +args.length
					case Some(nm) => nm
				}
				val tm = hd._2
				Pi(name, Mizar.constant("set"), OMA(LF.constant("arrow"), OMA(Mizar.constant("is"), OMV(name) :: tm :: Nil) :: genPropDef(tl, form, OMV(name) :: vars) :: Nil))
			}
			case Nil => {
				OMA(Mizar.constant("proof"), form :: Nil)
			}
		}
	}
}
*/