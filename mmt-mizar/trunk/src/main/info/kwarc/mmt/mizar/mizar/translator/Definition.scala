package info.kwarc.mmt.mizar.mizar.translator

import info.kwarc.mmt.mizar.mizar.objects._
import info.kwarc.mmt.mizar.mizar.reader._
import info.kwarc.mmt.mizar.mmt.objects._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.patterns._
import objects.Conversions._

object DefinitionTranslator {
	def translatePredMeansDef(p : MizPredMeansDef) = {
		val name = p.name match {
			case None => "R" + p.absnr
			case Some(s) => s
		}
		p.args.zipWithIndex.map(p => TranslationController.addLocusVarBinder(Index(SeqVar("x"), OMI(p._2))))
		TranslationController.addRetTerm(TranslationController.currentTheory ? name)

		val args = p.args.map(x => TypeTranslator.translateTyp(x._2))
		//val retType = TypeTranslator.translateTyp(p.retType)
		val cases = p.cases.map(x => PropositionTranslator.translateFormula(x._2))
		val results = p.cases.map(x => PropositionTranslator.translateFormula(x._1))
		val inst = p.form match {
			case Some(defForm) => 
				val default = PropositionTranslator.translateFormula(defForm)
				val matches = OMV("n") / OMI(args.length) ++ (SeqVar("args") / SeqItemList(args)) ++ 
								         "m" / OMI(cases.length) ++  SeqVar("cases") / SeqItemList(cases) ++ SeqVar("results") / SeqItemList(results) ++ "defRes"  / default
				val pattern = DefPatterns.MizPredMeansPartialDef
			    new Instance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.home.toMPath ? pattern.name, matches)

			case None => 	
				val matches = OMV("n") / OMI(args.length) ++ (SeqVar("args") / SeqItemList(args)) ++ 
								         "m" / OMI(cases.length) ++  SeqVar("cases") / SeqItemList(cases) ++ SeqVar("results") / SeqItemList(results)
				val pattern = DefPatterns.MizPredMeansCompleteDef
			    new Instance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.home.toMPath ? pattern.name, matches)
		}
		
		//to be removed
		//var const = new Constant(OMMOD(TranslationController.currentTheory), LocalName(name), None, None, null)
		//TranslationController.add(const)

		//println(inst)
		TranslationController.controller.add(inst)
		TranslationController.clearLocusVarContext()
	}
	
	def translatePredIsDef(p : MizPredIsDef) = {
		val name = p.name match {
			case None => "R" + p.absnr
			case Some(s) => s
		}
		p.args.zipWithIndex.map(p => TranslationController.addLocusVarBinder(Index(SeqVar("x"), OMI(p._2))))
			
		val args = p.args.map(x => TypeTranslator.translateTyp(x._2))
		//val retType = TypeTranslator.translateTyp(p.retType)
		val cases = p.cases.map(x => PropositionTranslator.translateFormula(x._2))
		val results = p.cases.map(x => TypeTranslator.translateTerm(x._1))
		val inst = p.term match {
			case Some(defTerm) => 
				val default = TypeTranslator.translateTerm(defTerm)
				val matches = OMV("n") / OMI(args.length) ++ (SeqVar("args") / SeqItemList(args)) ++ 
								         "m" / OMI(cases.length) ++  SeqVar("cases") / SeqItemList(cases) ++ SeqVar("results") / SeqItemList(results) ++ "defRes"  / default
				val pattern = DefPatterns.MizPredIsPartialDef
			    new Instance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.home.toMPath ? pattern.name, matches)

			case None => 	
				val matches = OMV("n") / OMI(args.length) ++ (SeqVar("args") / SeqItemList(args)) ++ 
								         "m" / OMI(cases.length) ++  SeqVar("cases") / SeqItemList(cases) ++ SeqVar("results") / SeqItemList(results)
				val pattern = DefPatterns.MizPredIsCompleteDef
				new Instance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.home.toMPath ? pattern.name, matches)
		}
		
		
		TranslationController.controller.add(inst)
		TranslationController.clearLocusVarContext()

	}
	
	
	def translateFuncMeansDef(f : MizFuncMeansDef) = {
		val name = f.name match {
			case None => f.kind + f.absnr
			case Some(s) => s
		}
		
		f.args.zipWithIndex.map(p => TranslationController.addLocusVarBinder(Index(SeqVar("x"), OMI(p._2))))
		TranslationController.addRetTerm(TranslationController.currentTheory ? name)
		val args = f.args.map(x => TypeTranslator.translateTyp(x._2))
		val retType = TypeTranslator.translateTyp(f.retType)
		val cases = f.cases.map(x => PropositionTranslator.translateFormula(x._2))
		val results = f.cases.map(x => PropositionTranslator.translateFormula(x._1))

		val inst = f.form match {
			case Some(defForm) => 

				val default = PropositionTranslator.translateFormula(defForm)

				val matches = OMV("n") / OMI(args.length) ++ (SeqVar("args") / SeqItemList(args)) ++ ("retType" / retType) ++
								         "m" / OMI(cases.length) ++  SeqVar("cases") / SeqItemList(cases) ++ SeqVar("results") / SeqItemList(results) ++ "defRes"  / default

				val pattern = DefPatterns.MizFuncMeansPartialDef

			    new Instance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.home.toMPath ? pattern.name, matches)

			case None => 	

				val matches = OMV("n") / OMI(args.length) ++ (SeqVar("args") / SeqItemList(args)) ++  ("retType" / retType) ++
								         "m" / OMI(cases.length) ++  SeqVar("cases") / SeqItemList(cases) ++ SeqVar("results") / SeqItemList(results)
				val pattern = DefPatterns.MizFuncMeansCompleteDef
			    new Instance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.home.toMPath ? pattern.name, matches)
								
		}
		
		//to be removed
		//var const = new Constant(OMMOD(TranslationController.currentTheory), LocalName(name), None, None, null)
		//TranslationController.add(const)
		
		//println(inst)
		TranslationController.controller.add(inst)
		TranslationController.clearLocusVarContext()

	}
	
	def translateFuncIsDef(f : MizFuncIsDef)  = {
		val name = f.name match {
			case None => f.kind + f.absnr
			case Some(s) => s
		}
			
		f.args.zipWithIndex.map(p => TranslationController.addLocusVarBinder(Index(SeqVar("x"), OMI(p._2))))

		val args = f.args.map(x => TypeTranslator.translateTyp(x._2))
		val retType = TypeTranslator.translateTyp(f.retType)
		val cases = f.cases.map(x => PropositionTranslator.translateFormula(x._2))

		val results = f.cases.map(x => TypeTranslator.translateTerm(x._1))
		val inst = f.term match {
			case Some(defTerm) => 
				val default = TypeTranslator.translateTerm(defTerm)
				val matches = OMV("n") / OMI(args.length) ++ (SeqVar("args") / SeqItemList(args)) ++  ("retType" / retType) ++
								         "m" / OMI(cases.length) ++  SeqVar("cases") / SeqItemList(cases) ++ SeqVar("results") / SeqItemList(results) ++ "defRes"  / default
				val pattern = DefPatterns.MizFuncIsPartialDef
			    new Instance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.home.toMPath ? pattern.name, matches)

			case None => 	
				val matches = OMV("n") / OMI(args.length) ++ (SeqVar("args") / SeqItemList(args)) ++  ("retType" / retType) ++
								         "m" / OMI(cases.length) ++  SeqVar("cases") / SeqItemList(cases) ++ SeqVar("results") / SeqItemList(results)
				val pattern = DefPatterns.MizFuncIsCompleteDef
			    new Instance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.home.toMPath ? pattern.name, matches)
		}
		

		TranslationController.controller.add(inst)
		TranslationController.clearLocusVarContext()

	}
	
	def translateModeMeansDef(m : MizModeMeansDef) = {
		val name = m.name match {
			case None => "M" + m.absnr
			case Some(s) => s
		}
		
		m.args.zipWithIndex.map(p => TranslationController.addLocusVarBinder(Index(SeqVar("x"), OMI(p._2))))
		TranslationController.addRetTerm(TranslationController.currentTheory ? name)

		val args = m.args.map(x => TypeTranslator.translateTyp(x._2))
		val cases = m.cases.map(x => PropositionTranslator.translateFormula(x._2))
		val results = m.cases.map(x => PropositionTranslator.translateFormula(x._1))
		val inst = m.form match {
			case Some(defForm) => 
				val default = PropositionTranslator.translateFormula(defForm)
				val matches = OMV("n") / OMI(args.length) ++ (SeqVar("args") / SeqItemList(args)) ++ 
								         "m" / OMI(cases.length) ++  SeqVar("cases") / SeqItemList(cases) ++ SeqVar("results") / SeqItemList(results) ++ "defRes"  / default
				val pattern = DefPatterns.MizModeMeansPartialDef
			    new Instance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.home.toMPath ? pattern.name, matches)
			case None => 	
				val matches = OMV("n") / OMI(args.length) ++ (SeqVar("args") / SeqItemList(args)) ++ 
								         "m" / OMI(cases.length) ++  SeqVar("cases") / SeqItemList(cases) ++ SeqVar("results") / SeqItemList(results)
				val pattern = DefPatterns.MizModeMeansCompleteDef
			    new Instance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.home.toMPath ? pattern.name, matches)
		}
		
		TranslationController.controller.add(inst)
		TranslationController.clearLocusVarContext()		
	}
	
	def translateModeIsDef(m : MizModeIsDef)  = {
		val name = m.name match {
			case None => "M" + m.absnr
			case Some(s) => s
		}
		
		m.args.zipWithIndex.map(p => TranslationController.addLocusVarBinder(Index(SeqVar("x"), OMI(p._2))))
		
		val args = m.args.map(x => TypeTranslator.translateTyp(x._2))
		val cases = m.cases.map(x => PropositionTranslator.translateFormula(x._2))
		val results = m.cases.map(x => TypeTranslator.translateTerm(x._1))
		val inst = m.term match {
			case Some(defTerm) => 
				val default = TypeTranslator.translateTerm(defTerm)
				val matches = OMV("n") / OMI(args.length) ++ (SeqVar("args") / SeqItemList(args)) ++ 
								         "m" / OMI(cases.length) ++  SeqVar("cases") / SeqItemList(cases) ++ SeqVar("results") / SeqItemList(results) ++ "defRes"  / default
				val pattern = DefPatterns.MizModeIsPartialDef
			    new Instance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.home.toMPath ? pattern.name, matches)
			case None => 	
				val matches = OMV("n") / OMI(args.length) ++ (SeqVar("args") / SeqItemList(args)) ++ 
								         "m" / OMI(cases.length) ++  SeqVar("cases") / SeqItemList(cases) ++ SeqVar("results") / SeqItemList(results)
				val pattern = DefPatterns.MizModeIsCompleteDef
			    new Instance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.home.toMPath ? pattern.name, matches)
		}
		
		//println(inst)
		TranslationController.controller.add(inst)
		TranslationController.clearLocusVarContext()		

	}
	
	
		def translateAttrMeansDef(a : MizAttrMeansDef) = {
		val name = a.name match {
			case None => "V" + a.absnr
			case Some(s) => s
		}
		
		a.args.zipWithIndex.map(p => TranslationController.addLocusVarBinder(Index(SeqVar("x"), OMI(p._2))))
		TranslationController.addRetTerm(TranslationController.currentTheory ? name)

		val args = a.args.map(x => TypeTranslator.translateTyp(x._2))
		val mType = TypeTranslator.translateTyp(a.retType)
		val cases = a.cases.map(x => PropositionTranslator.translateFormula(x._2))
		val results = a.cases.map(x => PropositionTranslator.translateFormula(x._1))
		val inst = a.form match {
			case Some(defForm) => 
				val default = PropositionTranslator.translateFormula(defForm)
				val matches =  (OMV("n") / OMI(args.length)) ++ (SeqVar("args") / SeqItemList(args)) ++ ("mType" / mType) ++  
								         "m" / OMI(cases.length) ++  SeqVar("cases") / SeqItemList(cases) ++ SeqVar("results") / SeqItemList(results) ++ "defRes"  / default
				val pattern = DefPatterns.MizAttrMeansPartialDef
			    new Instance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.home.toMPath ? pattern.name, matches)
			case None => 	
				val matches =  (OMV("n") / OMI(args.length)) ++ (SeqVar("args") / SeqItemList(args)) ++ ("mType" / mType) ++  
								         "m" / OMI(cases.length) ++  SeqVar("cases") / SeqItemList(cases) ++ SeqVar("results") / SeqItemList(results)
				val pattern = DefPatterns.MizAttrMeansCompleteDef
			    new Instance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.home.toMPath ? pattern.name, matches)
		}
		
		//println(inst)
		TranslationController.controller.add(inst)
		TranslationController.clearLocusVarContext()		
	}
			
	def translateAttrIsDef(a : MizAttrIsDef) = {
		val name = a.name match {
			case None => "V" + a.absnr
			case Some(s) => s
		}
		
		a.args.zipWithIndex.map(p => TranslationController.addLocusVarBinder(Index(SeqVar("x"), OMI(p._2))))
		TranslationController.addRetTerm(TranslationController.currentTheory ? name)

		val args = a.args.map(x => TypeTranslator.translateTyp(x._2))
		val mType = TypeTranslator.translateTyp(a.retType)
		val cases = a.cases.map(x => PropositionTranslator.translateFormula(x._2))
		val results = a.cases.map(x => TypeTranslator.translateTerm(x._1))
		val inst = a.term match {
			case Some(defTerm) => 
				val default = TypeTranslator.translateTerm(defTerm)
				val matches =  (OMV("n") / OMI(args.length)) ++ (SeqVar("args") / SeqItemList(args)) ++ ("mType" / mType) ++  
								         "m" / OMI(cases.length) ++  SeqVar("cases") / SeqItemList(cases) ++ SeqVar("results") / SeqItemList(results) ++ "defRes"  / default
				val pattern = DefPatterns.MizAttrIsPartialDef
			    new Instance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.home.toMPath ? pattern.name, matches)
			case None => 	
				val matches =  (OMV("n") / OMI(args.length)) ++ (SeqVar("args") / SeqItemList(args)) ++ ("mType" / mType) ++  
								         "m" / OMI(cases.length) ++  SeqVar("cases") / SeqItemList(cases) ++ SeqVar("results") / SeqItemList(results)
				val pattern = DefPatterns.MizAttrIsCompleteDef
			    new Instance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.home.toMPath ? pattern.name, matches)
		}
		
		//println(inst)
		TranslationController.controller.add(inst)
		TranslationController.clearLocusVarContext()		
	}
		
	private def genFieldsSub(s : List[Term]) : Substitution = {
	  s.length match {
	    case 0 => Substitution()
	    case 1 => (OMV("MS1") / s(0))
	    case _ => genFieldsSub(s.tail) ++ (("MS2" + s.length) / s(0))
	  }
	}
	
	def translateStructDef(s : MizStructDef) = {
	  val name = s.name match {
			case None => "L" + s.absnr
			case Some(str) => str
		}
	  
	  s.selDecls.map(x => {
	    val sname = "U" + x.absnr
	    TranslationController.addLocusVarBinder(OMV("mType"))
	    val rt = x.retType match {
	    	case None => Mizar.constant("set")
	    	case Some(t) => TypeTranslator.translateTyp(t)
	    }
	    val mt = TypeTranslator.translateTyp(x.mType)
	    val matches = (OMV("mType") / mt) ++ (OMV("retType") / rt)
	    val pattern = DefPatterns.MizSelDef
	    val i = new Instance(OMMOD(TranslationController.currentTheory), LocalName(sname), pattern.home.toMPath ? pattern.name, matches)
	    TranslationController.controller.add(i)
	    TranslationController.clearLocusVarContext()		

	  })
	  s.args.zipWithIndex.map(p => TranslationController.addLocusVarBinder(Index(SeqVar("x"), OMI(p._2))))

	  val args = s.args.map(x => TypeTranslator.translateTyp(x))
	  
	  val fields = s.fields.map(x => MMTResolve(x.aid, x.kind, x.absnr)).toList
	  
	  val mstructs = s.mstructs.map(x => TypeTranslator.translateTyp(x))
	  val nrFields = fields.length
	  
	  
	  
	  
	  
	  val inst = {
	    val matches = (OMV("n") / OMI(args.length)) ++ (SeqVar("args") / SeqItemList(args)) ++ genFieldsSub(fields.reverse)
	    
	    val pattern = DefPatterns.MizStructDef(nrFields)
	    new Instance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.home.toMPath ? pattern.name, matches)
	  }
	  
	  TranslationController.controller.add(inst)
	  TranslationController.clearLocusVarContext()		

	}
	
}