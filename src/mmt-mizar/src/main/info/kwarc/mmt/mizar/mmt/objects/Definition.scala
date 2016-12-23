package info.kwarc.mmt.mizar.mmt.objects

import info.kwarc.mmt.mizar.mizar.objects._

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

import info.kwarc.mmt.mizar.mizar.translator._

/* Params Utils */
object MMTArgs {
	def apply(nr : String, args : String, retType : Option[String]) : Context = {
		retType match {
			case Some(s) => Context(VarDecl(nr, None, None, None),
					                                   	VarDecl(args, Some(Rep(Mizar.tp, OMV(nr))), None, None),
					                                   	VarDecl(s, Some(Mizar.tp), None, None))
			case None => Context(VarDecl(nr, None, None, None),
					                               VarDecl(args, Some(Rep(Mizar.tp,OMV(nr))), None, None))
		}
	}
}

object MMTCases {
	def apply(nr : String, cases : String, results : String, default : Option[String]) : Context  = {
		default match {
			case Some(s) => Context(
					   VarDecl(nr, None, None, None),
					   VarDecl(cases, Some(Rep(Mizar.prop, OMV(nr))), None, None) ,
					   VarDecl(results,Some(Rep(Mizar.any, OMV(nr))), None, None),
					   VarDecl(s, Some(Mizar.any), None, None))
			case None => Context(
					   VarDecl(nr, None, None, None),
					   VarDecl(cases, Some(Rep(Mizar.prop, OMV(nr))), None, None) ,
					   VarDecl(results,Some(Rep(Mizar.any,OMV(nr))), None, None))
		}
	}
}

/* Elaboration Utils */
/*For Func/Mode/Pred definitions */

object MMTDefElab {
	def apply(argNr : String, ret : Term) : Context = {
	  Context(VarDecl(MMTUtils.mainPatternName, Some(MMTUtils.args(argNr, ret)), None, None))
	}
}


object MMTArgTypesElab {
	def apply(argNr : String, argTypes : String, retType : Term)  :  Context  = {
		Context(VarDecl("typing",
							Some(MMTUtils.args("x", argNr,
									MMTUtils.argTypes("x", argTypes, argNr,
										Mizar.be(Mizar.apply(OMV(MMTUtils.mainPatternName), OMV("x")), retType)
									    )
							    )),
							None, None))
	}
}



object MMTIsElab {
	def apply(argNr : String, argTypes : String, caseNr : String, cases : String, results : String, defResult : Option[String]) : Context = {
		defResult match {
			case Some(defRes) =>
				Context(VarDecl("meaning",
							Some(MMTUtils.args("x", argNr,
								MMTUtils.argTypes("x", argTypes, argNr,
								 		 Mizar.proof(Mizar.and(SeqMap(Mizar.implies(Index(cases,"i"),Mizar.eq(Mizar.apply(OMV(MMTUtils.mainPatternName), OMV("x")), Index(results, "i"))),"i", OMV(caseNr)),
								 				   Mizar.implies(Mizar.and(SeqMap(Mizar.not(Index(cases,"i")),"i", OMV(caseNr))),
								 				       Mizar.eq(Mizar.apply(OMV(MMTUtils.mainPatternName), OMV("x")), OMV(defRes)))))))),
							None, None))
			case None =>
				Context(VarDecl("meaning",
							Some(MMTUtils.args("x", argNr,
								MMTUtils.argTypes("x", argTypes, argNr,
								 		 Mizar.proof(Mizar.and(SeqMap(Mizar.implies(Index(cases,"i"),Mizar.eq(Mizar.apply(OMV(MMTUtils.mainPatternName), OMV("x")), Index(results, "i"))),"i", OMV(caseNr))))))),
							None, None),
						VarDecl("completeness",
						    Some(MMTUtils.args("x", argNr,
								MMTUtils.argTypes("x", argTypes, argNr,
								    Mizar.proof(Mizar.or(SeqMap(Index(cases,"i"),"i", OMV(caseNr))))))),
							None, None))
		}
	}
}

object MMTMeansElab {
	def apply(defName : String, argNr : String, argTypes : String, caseNr : String, cases : String, results : String, defResult : Option[String]) : Context = {
		defResult match {
			case Some(defRes) =>
				Context(VarDecl("meaning",
							Some(MMTUtils.args("x", argNr,
								MMTUtils.argTypes("x", argTypes, argNr,
								 		 Mizar.proof(Mizar.and(SeqMap(Mizar.implies(Index(cases,"i"),Index(results, "i")),"i", OMV(caseNr)),
								 				   Mizar.implies(Mizar.and(SeqMap(Mizar.not(Index(cases,"i")),"i", OMV(caseNr))),
								 				       OMV(defRes))))))),
							None, None))
			case None =>
				Context(VarDecl("meaning",
							Some(MMTUtils.args("x", argNr,
								MMTUtils.argTypes("x", argTypes, argNr,
								 		 Mizar.proof(Mizar.and(SeqMap(Mizar.implies(Index(cases,"i"),Index(results, "i")),"i", OMV(caseNr))))))),
							None, None),
						VarDecl("completeness",
						    Some(MMTUtils.args("x", argNr,
								MMTUtils.argTypes("x", argTypes, argNr,
								    Mizar.proof(Mizar.or(SeqMap(Index(cases,"i"),"i", OMV(caseNr))))))),
							None, None))
		}
	}
}


object MMTAttrTypingElab {
  def apply(argNr : String, argTypes : String,  mType : String) = {
	  val varName = ParsingController.dictionary.getFreeVar()
	  Context(VarDecl("typing",
			  Some(MMTUtils.args("x", argNr,
			    MMTUtils.argTypes("x", argTypes, argNr,
			         Pi("mType", Mizar.tp,
			             Mizar.forall(varName, Mizar.adjective(Mizar.apply(OMV(MMTUtils.mainPatternName), OMV("x"),OMV(mType)), OMV(mType)), Mizar.is(OMV(varName),OMV(mType)))
			    )))),None, None))
  }
}

object MMTAttrIsElab {
  	def apply(argNr : String, argTypes : String, mType : String, caseNr : String, cases : String, results : String, defResult : Option[String]) : Context = {
	  val v = TranslationController.getFreeVar()
  	  defResult match {
			case Some(defRes) =>
				Context(VarDecl("meaning",
							Some(MMTUtils.args("x", argNr,
								MMTUtils.argTypes("x", argTypes, argNr,
								 		 Mizar.proof(Mizar.and(SeqMap(Mizar.implies(Index(cases,"i"),Mizar.forall(v, OMV(mType), Mizar.eq(Mizar.apply(OMV(MMTUtils.mainPatternName),
								 		     OMV("x"),OMV(mType)), Index(results, "i")))),"i", OMV(caseNr)),
								 				   Mizar.implies(Mizar.and(SeqMap(Mizar.not(Index(cases,"i")),"i", OMV(caseNr))),
								 				       Mizar.forall(v, OMV(mType), Mizar.eq(Mizar.apply(OMV(MMTUtils.mainPatternName), OMV("x"), OMV(mType)), OMV(defRes))))))))),
							None, None))
			case None =>
				Context(VarDecl("meaning",
							Some(MMTUtils.args("x", argNr,
								MMTUtils.argTypes("x", argTypes, argNr,
								 		 Mizar.proof(Mizar.and(SeqMap(Mizar.implies(Index(cases,"i"),Mizar.forall(v, OMV(mType), Mizar.eq(Mizar.apply(OMV(MMTUtils.mainPatternName),
								 		     OMV("x"), OMV(mType)), Index(results, "i")))),"i", OMV(caseNr))))))),
						None, None),
						VarDecl("completeness",
						    Some(MMTUtils.args("x", argNr,
								MMTUtils.argTypes("x", argTypes, argNr,
								    Mizar.proof(Mizar.or(SeqMap(Index(cases,"i"),"i", OMV(caseNr))))))),
							None, None))

		}
	}
}

object MMTAttrMeansElab {
  	def apply(argNr : String, argTypes : String, mType : String, caseNr : String, cases : String, results : String, defResult : Option[String]) : Context = {
	  val v = TranslationController.getFreeVar()
  	  defResult match {
			case Some(defRes) =>
				Context(VarDecl("meaning",
							Some(MMTUtils.args("x", argNr,
								MMTUtils.argTypes("x", argTypes, argNr,
								 		 Mizar.proof(Mizar.and(SeqMap(Mizar.implies(Index(cases,"i"),Mizar.forall(v, OMV(mType),
								 		     Mizar.implies(Mizar.is(OMV(v), Mizar.adjective(Mizar.apply(OMV(MMTUtils.mainPatternName),
								 		         OMV("x"),OMV(mType)),OMV(mType))), Index(results, "i")))),"i", OMV(caseNr)),
								 				   Mizar.implies(Mizar.and(SeqMap(Mizar.not(Index(cases,"i")),"i", OMV(caseNr))),
								 				       Mizar.forall(v, OMV(mType), Mizar.implies(Mizar.is(OMV(v), Mizar.adjective(Mizar.apply(OMV(MMTUtils.mainPatternName),
								 				           OMV("x"),OMV(mType)),OMV(mType))), OMV(defRes))))))))),
							None, None))
			case None =>
				Context(VarDecl("meaning",
							Some(MMTUtils.args("x", argNr,
								MMTUtils.argTypes("x", argTypes, argNr,
								 		 Mizar.proof(Mizar.and(SeqMap(Mizar.implies(Index(cases,"i"), Mizar.forall(v, OMV(mType),
								 		     Mizar.implies(Mizar.is(OMV(v), Mizar.adjective(Mizar.apply(OMV(MMTUtils.mainPatternName), OMV("x"),OMV(mType)),OMV(mType))),
								 		         Index(results, "i")))),"i", OMV(caseNr))))))),
						None, None),
						VarDecl("completeness",
						    Some(MMTUtils.args("x", argNr,
								MMTUtils.argTypes("x", argTypes, argNr,
								    Mizar.proof(Mizar.or(SeqMap(Index(cases,"i"),"i", OMV(caseNr))))))),
							None, None))

		}
	}
}


/* Patterns */
object DefPatterns {

	val MizPredMeansPartialDef = MizPattern(LocalName("MizPredMeansPartialDef"),
		   			    MMTArgs("n", "args", None) ++ MMTCases("m", "cases", "results", Some("default")),
						MMTDefElab("n",Mizar.prop) ++ MMTArgTypesElab("n", "args", Mizar.prop) ++ MMTMeansElab("pred","n", "args", "m", "cases", "results", Some("default"))
		  )
	val MizPredMeansCompleteDef = MizPattern(LocalName("MizPredMeansCompleteDef"),
		   			    MMTArgs("n", "args", None) ++ MMTCases("m", "cases", "results", None),
						MMTDefElab("n",Mizar.prop) ++ MMTArgTypesElab("n", "args", Mizar.prop) ++ MMTMeansElab("pred","n", "args", "m", "cases", "results", None)
		  )

	val MizPredIsPartialDef = MizPattern(LocalName("MizPredIsPartialDef"),
		   			    MMTArgs("n", "args", None) ++ MMTCases("m", "cases", "results", Some("default")),
						MMTDefElab("n",Mizar.prop) ++ MMTArgTypesElab("n", "args", Mizar.prop) ++ MMTIsElab("n", "args", "m", "cases", "results", Some("default"))
		  )
	val MizPredIsCompleteDef = MizPattern(LocalName("MizPredIsCompleteDef"),
		   			    MMTArgs("n", "args", None) ++ MMTCases("m", "cases", "results", None),
						MMTDefElab("n",Mizar.prop) ++ MMTArgTypesElab("n", "args", Mizar.prop) ++ MMTIsElab("n", "args", "m", "cases", "results", None)
		  )

	val MizFuncMeansPartialDef = MizPattern(LocalName("MizFuncMeansPartialDef"),
		   			    MMTArgs("n", "args", Some("retType")) ++ MMTCases("m", "cases", "results", Some("default")),
						MMTDefElab("n",Mizar.any) ++ MMTArgTypesElab("n", "args", "retType") ++ MMTMeansElab("func","n", "args", "m", "cases", "results", Some("default"))
		  )
	val MizFuncMeansCompleteDef = MizPattern(LocalName("MizFuncMeansCompleteDef"),
		   			    MMTArgs("n", "args", Some("retType")) ++ MMTCases("m", "cases", "results", None),
						MMTDefElab("n",Mizar.any) ++ MMTArgTypesElab("n", "args", "retType") ++ MMTMeansElab("func","n", "args", "m", "cases", "results", None)
		  )

	val MizFuncIsPartialDef = MizPattern(LocalName("MizFuncIsPartialDef"),
		   			    MMTArgs("n", "args", Some("retType")) ++ MMTCases("m", "cases", "results", Some("default")),
						MMTDefElab("n",Mizar.any) ++ MMTArgTypesElab("n", "args", "retType") ++ MMTIsElab("n", "args", "m", "cases", "results", Some("default"))
		  )
	val MizFuncIsCompleteDef = MizPattern(LocalName("MizFuncIsCompleteDef"),
		   			    MMTArgs("n", "args", Some("retType")) ++ MMTCases("m", "cases", "results", None),
						MMTDefElab("n",Mizar.any) ++ MMTArgTypesElab("n", "args", "retType") ++ MMTIsElab("n", "args", "m", "cases", "results", None)
		  )
	val MizModeMeansPartialDef = MizPattern(LocalName("MizModeMeansPartialDef"),
		   			    MMTArgs("n", "args", None) ++ MMTCases("m", "cases", "results", Some("default")),
						MMTDefElab("n",Mizar.tp) ++ MMTArgTypesElab("n", "args", Mizar.tp) ++ MMTMeansElab("mode","n", "args", "m", "cases", "results", Some("default"))
		  )
	val MizModeMeansCompleteDef = MizPattern(LocalName("MizModeMeansCompleteDef"),
		   			    MMTArgs("n", "args", None) ++ MMTCases("m", "cases", "results", None),
						MMTDefElab("n",Mizar.tp) ++ MMTArgTypesElab("n", "args", Mizar.tp) ++ MMTMeansElab("mode","n", "args", "m", "cases", "results", None)
		  )

	val MizModeIsPartialDef = MizPattern(LocalName("MizModeIsPartialDef"),
		   			    MMTArgs("n", "args", None) ++ MMTCases("m", "cases", "results", Some("default")),
						MMTDefElab("n",Mizar.tp) ++ MMTArgTypesElab("n", "args", Mizar.tp) ++ MMTIsElab("n", "args", "m", "cases", "results", Some("default"))
		  )
	val MizModeIsCompleteDef = MizPattern(LocalName("MizModeIsCompleteDef"),
		   			    MMTArgs("n", "args", None) ++ MMTCases("m", "cases", "results", None),
						MMTDefElab("n",Mizar.tp) ++ MMTArgTypesElab("n", "args", Mizar.tp) ++ MMTIsElab("n", "args", "m", "cases", "results", None)
		  )

	val MizAttrMeansPartialDef = MizPattern(LocalName("MizAttrMeansPartialDef"),
		   			    MMTArgs("n", "args", None) ++ Context(VarDecl("mType", Some(Mizar.tp), None, None)) ++MMTCases("m", "cases", "results", Some("default")),
						MMTDefElab("n",Mizar.attr(OMV("mType"))) ++ MMTAttrTypingElab("n", "args", "mType") ++ MMTAttrMeansElab("n", "args", "mType", "m", "cases", "results", Some("default"))
		  )
	val MizAttrMeansCompleteDef = MizPattern(LocalName("MizAttrMeansCompleteDef"),
		   			    MMTArgs("n", "args", None) ++ Context(VarDecl("mType", Some(Mizar.tp), None, None)) ++MMTCases("m", "cases", "results", None),
						MMTDefElab("n",Mizar.attr(OMV("mType"))) ++ MMTAttrTypingElab("n", "args", "mType") ++ MMTAttrMeansElab("n", "args", "mType", "m", "cases", "results", None)
		  )
	val MizAttrIsPartialDef = MizPattern(LocalName("MizAttrIsPartialDef"),
		   			    MMTArgs("n", "args", None) ++ Context(VarDecl("mType", Some(Mizar.tp), None, None)) ++MMTCases("m", "cases", "results", Some("default")),
						MMTDefElab("n",Mizar.attr(OMV("mType"))) ++ MMTAttrTypingElab("n", "args", "mType") ++ MMTAttrIsElab("n", "args", "mType", "m", "cases", "results", Some("default"))
		  )
	val MizAttrIsCompleteDef = MizPattern(LocalName("MizAttrIsCompleteDef"),
		   			    MMTArgs("n", "args", None) ++ Context(VarDecl("mType", Some(Mizar.tp), None, None)) ++MMTCases("m", "cases", "results", None),
						MMTDefElab("n",Mizar.attr(OMV("mType"))) ++ MMTAttrTypingElab("n", "args", "mType") ++ MMTAttrIsElab("n", "args", "mType", "m", "cases", "results", None)
		  )

	private def genMSDecl(nrMS : Int) : Context = {
	  nrMS match {
	    case 1 => Context(VarDecl("MS1", Some(Mizar.constant("tp")), None, None))
	    case _ => genMSDecl(nrMS - 1) ++ Context(VarDecl("MS" + nrMS, Some(Mizar.constant("tp")), None, None))
	  }
	}

	private def genMSElab(nrMS : Int) : Context = {
	  nrMS match {
	    case 1 => Context(VarDecl("MS1_prop", Some(Mizar.proof(Mizar.forall("t",OMV(MMTUtils.mainPatternName),Mizar.is(OMV("t"),OMV("MS1"))))), None, None))
	    case _ => genMSElab(nrMS - 1) ++ Context(VarDecl("MS"+ nrMS + "_prop", Some(Mizar.proof(Mizar.forall("t",OMV(MMTUtils.mainPatternName),Mizar.is(OMV("t"),OMV("MS" + nrMS))))), None, None))
	  }
	}


	def MizStructDef(nrMS : Int) = {
	  MizPattern(LocalName("MizStructDef" + nrMS),
						MMTArgs("n","args",None) ++ genMSDecl(nrMS),
						Context(VarDecl(MMTUtils.mainPatternName,Some(MMTUtils.args("x", "n", MMTUtils.argTypes("x", "args", "n",Mizar.tp))) , None, None),
						  VarDecl("aggr", Some(MMTUtils.args("x", "n", MMTUtils.argTypes("p","x", "args", "n",Mizar.set))), None, None),
						  VarDecl("aggr_prop",Some(MMTUtils.args("x", "n", MMTUtils.argTypes("p","x", "args", "n",
						      Mizar.be(Mizar.apply(OMV("aggr"), OMV("x"), OMV("p")), Mizar.apply(OMV(MMTUtils.mainPatternName), OMV("x"), OMV("p")))))), None, None),
						  VarDecl("strict",Some(Arrow(Mizar.any, Mizar.prop)), None, None))  ++ genMSElab(nrMS))
	}


	val MizSelDef =  MizPattern(LocalName("MizSelector"),
			Context(VarDecl("mType", Some(Mizar.tp), None, None),
			    	VarDecl("retType", Some(Mizar.tp), None, None)),
			Context(VarDecl(MMTUtils.mainPatternName, Some(Arrow(Mizar.any, Mizar.any)), None, None),
					VarDecl("sel_prop", Some(Mizar.proof(Mizar.forall("t",OMV("mType"),Mizar.is(Mizar.apply(OMV(MMTUtils.mainPatternName),OMV("t")),OMV("retType"))))), None, None)))

}
