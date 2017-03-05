package info.kwarc.mmt.mizar.mmtwrappers

import info.kwarc.mmt.mizar.objects._
import MizSeq._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.lf._
import objects.Conversions._
import info.kwarc.mmt.mizar.translator._

/* Params Utils */
object MMTArgs {
	def apply(nr : String, args : String, retType : Option[String]) : Context = {
		retType match {
			case Some(s) => Context(VarDecl(nr), VarDecl(args, Rep(Mizar.tp, OMV(nr))), VarDecl(s, Mizar.tp))
			case None => Context(VarDecl(nr), VarDecl(args, Rep(Mizar.tp,OMV(nr))))
		}
	}
}

object MMTCases {
	def apply(argsnr : String, casesnr : String, cases : String, results : String, default : Option[String], retType : Term, isMeans : Boolean) : Context  = {
	  
	  val resultsType = isMeans match {
	    case false => retType
	    case true => Mizar.prop
	  }
	  
	  def wrapItVar(t : Term): Term = isMeans match {
	    case false => t
	    case true => Arrow(retType, t)
	  }
	  
	  val con = Context(
       VarDecl(casesnr),
       VarDecl(cases, Rep(Arrow(Rep(Mizar.any, OMV(argsnr)), Mizar.prop), OMV(casesnr))),
       VarDecl(results,Rep(Arrow(Rep(Mizar.any, OMV(argsnr)), wrapItVar(resultsType)), OMV(casesnr)))
		)
	  
		default match {
			case Some(s) => 
			  con ++ VarDecl(s, Arrow(Rep(Mizar.any, OMV(argsnr)), wrapItVar(resultsType)))
			case None => con 
		}
	}
}

/* Elaboration Utils */
/*For Func/Mode/Pred definitions */

object MMTDefElab {
	def apply(argNr : String, ret : Term) : Context = {
	  Context(VarDecl(MMTUtils.mainPatternName, MMTUtils.PiArgs(argNr, ret)))
	}
}


object MMTArgTypesElab {
	def apply(argNr : String, argTypes : String, retType : Term)  :  Context  = {
		Context(VarDecl("typing",
							MMTUtils.PiArgs("x", argNr,
									MMTUtils.PiArgTypes("x", argTypes, argNr,
										Mizar.be(Mizar.apply(OMV(MMTUtils.mainPatternName), OMV("x")), retType)
									    )
							    )
							))
	}
}

object MMTIsElab {
	def apply(argNr : String, argTypes : String, caseNr : String, cases : String, results : String, defResult : Option[String]) : Context = {
		defResult match {
			case Some(defRes) =>
				Context(VarDecl("meaning",
							MMTUtils.PiArgs("x", argNr,
								MMTUtils.PiArgTypes("x", argTypes, argNr,
								 		 Mizar.proof(Mizar.and(List(
								 		     Mizar.seqConn("and", OMV(caseNr), Ellipsis(OMV(caseNr), "i",
								 		         Mizar.implies(Mizar.apply(Index(cases,"i"), OMV("x")),
								 		                       Mizar.eq(Mizar.apply(OMV(MMTUtils.mainPatternName), OMV("x")),
								 		                                Mizar.apply(Index(results, "i"), OMV("x"))
								 		                               )
								 		                       )
								 		       )),
								 				   Mizar.implies(Mizar.seqConn("and", OMV(caseNr), Ellipsis(OMV(caseNr),"i", Mizar.not(Mizar.apply(Index(cases,"i"), OMV("x"))))),
								 				                 Mizar.eq(Mizar.apply(OMV(MMTUtils.mainPatternName), OMV("x")), Mizar.apply(OMV(defRes), OMV("x")))
								 				   )
								 		 )))
								)
							)
			  ))
			case None =>
				Context(VarDecl("meaning",
							MMTUtils.PiArgs("x", argNr,
								MMTUtils.PiArgTypes("x", argTypes, argNr,
								 		 Mizar.proof(Mizar.seqConn("and", OMV(caseNr), Ellipsis(OMV(caseNr), "i", Mizar.implies(Mizar.apply(Index(cases,"i"),OMV("x")),Mizar.eq(Mizar.apply(OMV(MMTUtils.mainPatternName), OMV("x")), Mizar.apply(Index(results, "i"), OMV("x")))))))))
							),
						VarDecl("completeness",
						    MMTUtils.PiArgs("x", argNr,
								MMTUtils.PiArgTypes("x", argTypes, argNr,
								    Mizar.proof(Mizar.seqConn("or", OMV(caseNr), Ellipsis(OMV(caseNr),"i", Mizar.apply(Index(cases,"i"), OMV("x")))))))
							))
		}
	}
}

object MMTMeansElab {
	def apply(defName : String, argNr : String, argTypes : String, caseNr : String, cases : String, results : String, defResult : Option[String]) : Context = {
		defResult match {
			case Some(defRes) =>
				Context(VarDecl("meaning",
							MMTUtils.PiArgs("x", argNr,
								MMTUtils.PiArgTypes("x", argTypes, argNr,
								 		 Mizar.proof(Mizar.and(List(Mizar.seqConn("and", OMV(caseNr), Ellipsis(OMV(caseNr),"i", 
								 		     Mizar.implies(Mizar.apply(Index(cases,"i"),OMV("x")),Mizar.apply(Index(results, "i"),OMV("x"), Mizar.apply(MMTUtils.mainPatternName, OMV("x")))))),
								 				   Mizar.implies(Mizar.seqConn("and", OMV(caseNr), Ellipsis(OMV(caseNr), "i", Mizar.not(Mizar.apply(Index(cases,"i"),OMV("x"))))),
								 				       Mizar.apply(OMV(defRes), OMV("x"), Mizar.apply(OMV(MMTUtils.mainPatternName), OMV("x")))))))))
							))
			case None =>
				Context(VarDecl("meaning",
							MMTUtils.PiArgs("x", argNr,
								MMTUtils.PiArgTypes("x", argTypes, argNr,
								 		 Mizar.proof(Mizar.seqConn("and", OMV(caseNr), Ellipsis(OMV(caseNr),"i", Mizar.implies(
								 		     Mizar.apply(Index(cases,"i"),OMV("x")),Mizar.apply(Index(results, "i"),OMV("x"), Mizar.apply(MMTUtils.mainPatternName, OMV("x")))))))))
							),
						VarDecl("completeness",
						    MMTUtils.PiArgs("x", argNr,
								MMTUtils.PiArgTypes("x", argTypes, argNr,
								    Mizar.proof(Mizar.seqConn("or", OMV(caseNr), Ellipsis(OMV(caseNr), "i", Mizar.apply(Index(cases,"i"),OMV("x")))))))
							))
		}
	}
}


object MMTAttrTypingElab {
  def apply(argNr : String, argTypes : String,  mType : String) = {
	  val varName = ParsingController.dictionary.getFreeVar()
	  Context(VarDecl("typing",
			  MMTUtils.PiArgs("x", argNr,
			    MMTUtils.PiArgTypes("x", argTypes, argNr,
			         Pi("mType", Mizar.tp,
			             Mizar.forall(varName, Mizar.adjective(Mizar.apply(OMV(MMTUtils.mainPatternName), OMV("x"),OMV(mType)), OMV(mType)), Mizar.is(OMV(varName),OMV(mType)))
			    )))))
  }
}

object MMTAttrIsElab {
  	def apply(argNr : String, argTypes : String, mType : String, caseNr : String, cases : String, results : String, defResult : Option[String]) : Context = {
	  val v = TranslationController.getFreeVar()
  	  defResult match {
			case Some(defRes) =>
				Context(VarDecl("meaning",
							MMTUtils.PiArgs("x", argNr,
								MMTUtils.PiArgTypes("x", argTypes, argNr,
								 		 Mizar.proof(Mizar.and(List(
								 		     Mizar.seqConn("and", OMV(caseNr), Ellipsis(OMV(caseNr), "i", Mizar.implies(Mizar.apply(Index(cases,"i"),OMV("x")),Mizar.forall(v, OMV(mType), Mizar.eq(Mizar.apply(OMV(MMTUtils.mainPatternName),
								 		     OMV("x"),OMV(mType)), Mizar.apply(Index(results, "i"),OMV("x"))))))),
								 				   Mizar.implies(Mizar.seqConn("and", OMV(caseNr), Ellipsis(OMV(caseNr), "i", Mizar.not(Mizar.apply(Index(cases,"i"),OMV("x"))))),
								 				       Mizar.forall(v, OMV(mType), Mizar.eq(Mizar.apply(OMV(MMTUtils.mainPatternName), OMV("x"), OMV(mType)), Mizar.apply(OMV(defRes), OMV("x"))))))))))
							))
			case None =>
				Context(VarDecl("meaning",
							MMTUtils.PiArgs("x", argNr,
								MMTUtils.PiArgTypes("x", argTypes, argNr,
								 		 Mizar.proof(Mizar.seqConn("and", OMV(caseNr), Ellipsis(OMV(caseNr), "i", Mizar.implies(Mizar.apply(Index(cases,"i"),OMV("x")),Mizar.forall(v, OMV(mType), Mizar.eq(Mizar.apply(OMV(MMTUtils.mainPatternName),
								 		     OMV("x"), OMV(mType)), Mizar.apply(Index(results, "i"),OMV("x"))))))))))
						),
						VarDecl("completeness",
						    MMTUtils.PiArgs("x", argNr,
								MMTUtils.PiArgTypes("x", argTypes, argNr,
								    Mizar.proof(Mizar.seqConn("or", OMV(caseNr), Ellipsis(OMV(caseNr), "i", Mizar.apply(Index(cases,"i"),OMV("x")))))))
								    ))
		}
	}
}

object MMTAttrMeansElab {
  	def apply(argNr : String, argTypes : String, mType : String, caseNr : String, cases : String, results : String, defResult : Option[String]) : Context = {
	  val v = TranslationController.getFreeVar()
  	  defResult match {
			case Some(defRes) =>
				Context(VarDecl("meaning",
				  MMTUtils.PiArgs("x", argNr,
						MMTUtils.PiArgTypes("x", argTypes, argNr,
						    Mizar.proof(Mizar.and(List(
						        Mizar.seqConn("and", OMV(caseNr), Ellipsis(OMV(caseNr), "i", Mizar.implies(
						            Mizar.apply(Index(cases,"i"),OMV("x")),
						            Mizar.apply(Index(results, "i"), OMV("x"), Mizar.apply(OMV(MMTUtils.mainPatternName), OMV("x")))
						        ))),
						        Mizar.implies(Mizar.seqConn("and", OMV(caseNr), Ellipsis(OMV(caseNr), "i", Mizar.not(Mizar.apply(Index(cases, "i"), OMV("x"))))),
						                     Mizar.implies(Mizar.apply(MMTUtils.mainPatternName, OMV("x")),Mizar.apply(OMV(defRes), OMV("x"), Mizar.apply(OMV(MMTUtils.mainPatternName), OMV("x")))))
						    )))
						))))
			case None =>
				Context(VarDecl("meaning",
				  MMTUtils.PiArgs("x", argNr,
						MMTUtils.PiArgTypes("x", argTypes, argNr,
						    Mizar.proof(
						        Mizar.seqConn("and", OMV(caseNr), Ellipsis(OMV(caseNr), "i", Mizar.implies(
						            Mizar.apply(Index(cases,"i"),OMV("x")),
						            Mizar.apply(Index(results, "i"), OMV("x"), Mizar.apply(OMV(MMTUtils.mainPatternName), OMV("x")))
						        ))))))
						        ),
						VarDecl("completeness",
						    MMTUtils.PiArgs("x", argNr,
								MMTUtils.PiArgTypes("x", argTypes, argNr,
								    Mizar.proof(Mizar.seqConn("or", OMV(caseNr), Ellipsis(OMV(caseNr),"i", Mizar.apply(Index(cases,"i"),OMV("x"))))))))
								    )
		}
	}
}


/* Patterns */
object DefPatterns {

	val MizPredMeansPartialDef = MizPattern(LocalName("MizPredMeansPartialDef"),
		   			    MMTArgs("n", "args", None) ++ MMTCases("n", "m", "cases", "results", Some("default"), Mizar.prop, true),
						MMTDefElab("n",Mizar.prop) ++ MMTArgTypesElab("n", "args", Mizar.prop) ++ MMTMeansElab("pred","n", "args", "m", "cases", "results", Some("default"))
		  )
	val MizPredMeansCompleteDef = MizPattern(LocalName("MizPredMeansCompleteDef"),
		   			    MMTArgs("n", "args", None) ++ MMTCases("n", "m", "cases", "results", None, Mizar.prop, true),
						MMTDefElab("n",Mizar.prop) ++ MMTArgTypesElab("n", "args", Mizar.prop) ++ MMTMeansElab("pred","n", "args", "m", "cases", "results", None)
		  )

	val MizPredIsPartialDef = MizPattern(LocalName("MizPredIsPartialDef"),
		   			    MMTArgs("n", "args", None) ++ MMTCases("n", "m", "cases", "results", Some("default"), Mizar.prop, false),
						MMTDefElab("n",Mizar.prop) ++ MMTArgTypesElab("n", "args", Mizar.prop) ++ MMTIsElab("n", "args", "m", "cases", "results", Some("default"))
		  )
	val MizPredIsCompleteDef = MizPattern(LocalName("MizPredIsCompleteDef"),
		   			    MMTArgs("n", "args", None) ++ MMTCases("n", "m", "cases", "results", None, Mizar.prop, false),
						MMTDefElab("n",Mizar.prop) ++ MMTArgTypesElab("n", "args", Mizar.prop) ++ MMTIsElab("n", "args", "m", "cases", "results", None)
		  )

	val MizFuncMeansPartialDef = MizPattern(LocalName("MizFuncMeansPartialDef"),
		   			    MMTArgs("n", "args", Some("retType")) ++ MMTCases("n", "m", "cases", "results", Some("default"), Mizar.any, true),
						MMTDefElab("n",Mizar.any) ++ MMTArgTypesElab("n", "args", "retType") ++ MMTMeansElab("func","n", "args", "m", "cases", "results", Some("default"))
		  )
	val MizFuncMeansCompleteDef = MizPattern(LocalName("MizFuncMeansCompleteDef"),
		   			    MMTArgs("n", "args", Some("retType")) ++ MMTCases("n", "m", "cases", "results", None, Mizar.any, true),
						MMTDefElab("n",Mizar.any) ++ MMTArgTypesElab("n", "args", "retType") ++ MMTMeansElab("func","n", "args", "m", "cases", "results", None)
		  )

	val MizFuncIsPartialDef = MizPattern(LocalName("MizFuncIsPartialDef"),
		   			    MMTArgs("n", "args", Some("retType")) ++ MMTCases("n", "m", "cases", "results", Some("default"), Mizar.any, false),
						MMTDefElab("n",Mizar.any) ++ MMTArgTypesElab("n", "args", "retType") ++ MMTIsElab("n", "args", "m", "cases", "results", Some("default"))
		  )
	val MizFuncIsCompleteDef = MizPattern(LocalName("MizFuncIsCompleteDef"),
		   			    MMTArgs("n", "args", Some("retType")) ++ MMTCases("n", "m", "cases", "results", None, Mizar.any, false),
						MMTDefElab("n",Mizar.any) ++ MMTArgTypesElab("n", "args", "retType") ++ MMTIsElab("n", "args", "m", "cases", "results", None)
		  )
	val MizModeMeansPartialDef = MizPattern(LocalName("MizModeMeansPartialDef"),
		   			    MMTArgs("n", "args", None) ++ MMTCases("n", "m", "cases", "results", Some("default"), Mizar.tp, true),
						MMTDefElab("n",Mizar.tp) ++ MMTArgTypesElab("n", "args", Mizar.tp) ++ MMTMeansElab("mode","n", "args", "m", "cases", "results", Some("default"))
		  )
	val MizModeMeansCompleteDef = MizPattern(LocalName("MizModeMeansCompleteDef"),
		   			    MMTArgs("n", "args", None) ++ MMTCases("n", "m", "cases", "results", None, Mizar.tp, true),
						MMTDefElab("n",Mizar.tp) ++ MMTArgTypesElab("n", "args", Mizar.tp) ++ MMTMeansElab("mode","n", "args", "m", "cases", "results", None)
		  )

	val MizModeIsPartialDef = MizPattern(LocalName("MizModeIsPartialDef"),
		   			    MMTArgs("n", "args", None) ++ MMTCases("n", "m", "cases", "results", Some("default"), Mizar.tp, false),
						MMTDefElab("n",Mizar.tp) ++ MMTArgTypesElab("n", "args", Mizar.tp) ++ MMTIsElab("n", "args", "m", "cases", "results", Some("default"))
		  )
	val MizModeIsCompleteDef = MizPattern(LocalName("MizModeIsCompleteDef"),
		   			    MMTArgs("n", "args", None) ++ MMTCases("n", "m", "cases", "results", None, Mizar.tp, false),
						MMTDefElab("n",Mizar.tp) ++ MMTArgTypesElab("n", "args", Mizar.tp) ++ MMTIsElab("n", "args", "m", "cases", "results", None)
		  )

	val MizAttrMeansPartialDef = MizPattern(LocalName("MizAttrMeansPartialDef"),
		   			    MMTArgs("n", "args", None) ++ Context(VarDecl("mType", Mizar.tp)) ++MMTCases("n", "m", "cases", "results", Some("default"), OMV("mType"), true),
						MMTDefElab("n",Mizar.attr(OMV("mType"))) ++ MMTAttrTypingElab("n", "args", "mType") ++ MMTAttrMeansElab("n", "args", "mType", "m", "cases", "results", Some("default"))
		  )
	val MizAttrMeansCompleteDef = MizPattern(LocalName("MizAttrMeansCompleteDef"),
		   			    MMTArgs("n", "args", None) ++ Context(VarDecl("mType", Mizar.tp)) ++MMTCases("n", "m", "cases", "results", None, OMV("mType"), true),
						MMTDefElab("n",Mizar.attr(OMV("mType"))) ++ MMTAttrTypingElab("n", "args", "mType") ++ MMTAttrMeansElab("n", "args", "mType", "m", "cases", "results", None)
		  )
	val MizAttrIsPartialDef = MizPattern(LocalName("MizAttrIsPartialDef"),
		   			    MMTArgs("n", "args", None) ++ Context(VarDecl("mType", Mizar.tp)) ++MMTCases("n", "m", "cases", "results", Some("default"), OMV("mType"), false),
						MMTDefElab("n",Mizar.attr(OMV("mType"))) ++ MMTAttrTypingElab("n", "args", "mType") ++ MMTAttrIsElab("n", "args", "mType", "m", "cases", "results", Some("default"))
		  )
	val MizAttrIsCompleteDef = MizPattern(LocalName("MizAttrIsCompleteDef"),
		   			    MMTArgs("n", "args", None) ++ Context(VarDecl("mType", Mizar.tp)) ++MMTCases("n", "m", "cases", "results", None, OMV("mType"), true),
						MMTDefElab("n",Mizar.attr(OMV("mType"))) ++ MMTAttrTypingElab("n", "args", "mType") ++ MMTAttrIsElab("n", "args", "mType", "m", "cases", "results", None)
		  )

	private def genMSDecl(nrMS : Int) : Context = {
	  nrMS match {
	    case 1 => Context(VarDecl("MS1", Mizar.constant("tp")))
	    case _ => genMSDecl(nrMS - 1) ++ Context(VarDecl("MS" + nrMS, Mizar.constant("tp")))
	  }
	}

	private def genMSElab(nrMS : Int) : Context = {
	  nrMS match {
	    case 1 => Context(VarDecl("MS1_prop", Mizar.proof(Mizar.forall("t",OMV(MMTUtils.mainPatternName),Mizar.is(OMV("t"),OMV("MS1"))))))
	    case _ => genMSElab(nrMS - 1) ++ Context(VarDecl("MS"+ nrMS + "_prop", Mizar.proof(Mizar.forall("t",OMV(MMTUtils.mainPatternName),Mizar.is(OMV("t"),OMV("MS" + nrMS))))))
	  }
	}


	def MizStructDef(nrMS : Int) = {
	  MizPattern(LocalName("MizStructDef" + nrMS),
						MMTArgs("n","args",None) ++ genMSDecl(nrMS),
						Context(VarDecl(MMTUtils.mainPatternName,MMTUtils.PiArgs("x", "n", MMTUtils.PiArgTypes("x", "args", "n",Mizar.tp))),
						  VarDecl("aggr", MMTUtils.PiArgs("x", "n", MMTUtils.PiArgTypes("p","x", "args", "n",Mizar.set))),
						  VarDecl("aggr_prop",MMTUtils.PiArgs("x", "n", MMTUtils.PiArgTypes("p","x", "args", "n",
						      Mizar.be(Mizar.apply(OMV("aggr"), OMV("x"), OMV("p")), Mizar.apply(OMV(MMTUtils.mainPatternName), OMV("x"), OMV("p")))))),
						  VarDecl("strict",Arrow(Mizar.any, Mizar.prop)))  ++ genMSElab(nrMS))
	}


	val MizSelDef =  MizPattern(LocalName("MizSelector"),
			Context(VarDecl("mType", Mizar.tp),
			    	VarDecl("retType", Mizar.tp)),
			Context(VarDecl(MMTUtils.mainPatternName, Arrow(Mizar.any, Mizar.any)),
					VarDecl("sel_prop", Mizar.proof(Mizar.forall("t",OMV("mType"),Mizar.is(Mizar.apply(OMV(MMTUtils.mainPatternName),OMV("t")),OMV("retType")))))))
}
