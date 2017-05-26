package info.kwarc.mmt.mizar.translator

import info.kwarc.mmt.mizar.objects._
import info.kwarc.mmt.mizar.reader._
import info.kwarc.mmt.mizar.mmtwrappers._

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import objects.Conversions._

import info.kwarc.mmt.lf._

import info.kwarc.mmt.mizar.mmtwrappers.MizInstance


object ArticleTranslator {
	def translateArticle(art : MizArticle) {		
		var el : List[MizAny] = art.elems.reverse
		el.map(transArtElems)
	}
	
	import TranslationController.makeConstant
	
	private def transArtElems(e : MizAny) : Unit = {
		e match {	
			case j : MizJustifiedTheorem => 
			  translateJustifiedTheorem(j)
			  TranslationController.theorems += 1
			case m : MizModeMeansDef => 
			  DefinitionTranslator.translateModeMeansDef(m)
			  TranslationController.defs += 1
			case m : MizModeIsDef => 
			  DefinitionTranslator.translateModeIsDef(m)	
			  TranslationController.defs += 1
			case p : MizPredMeansDef =>
			  DefinitionTranslator.translatePredMeansDef(p)
			  TranslationController.defs += 1
			case p : MizPredIsDef => 
			  DefinitionTranslator.translatePredIsDef(p)
			  TranslationController.defs += 1
			case f : MizFuncMeansDef =>
			  DefinitionTranslator.translateFuncMeansDef(f)
			  TranslationController.defs += 1
			case f : MizFuncIsDef => 
			  DefinitionTranslator.translateFuncIsDef(f)
			  TranslationController.defs += 1
			case a : MizAttrMeansDef => 
			  DefinitionTranslator.translateAttrMeansDef(a)
			  TranslationController.defs += 1
			case a : MizAttrIsDef => 
			  DefinitionTranslator.translateAttrIsDef(a)
			  TranslationController.defs += 1
			case m : MizExpMode => 
			  DefinitionTranslator.translateExpMode(m)
			  TranslationController.defs += 1
			case s : MizStructDef => 
			  DefinitionTranslator.translateStructDef(s)
			  TranslationController.defs += 1
			case n : MizNotation => 
			  translateNotation(n)
			  TranslationController.notations += 1
			case r : MizRegistration => 
			  SchemeRegTranslator.translateRegistration(r)
			  TranslationController.regs += 1
			case s : MizSchemeDef => 
			  SchemeRegTranslator.translateScheme(s)
			  TranslationController.schemes += 1
			case l : MizLemma => translateLemma(l)
			case s : MizSet => translateSet(s)
			case c : MizConsider => translateConsider(c)
			case r : MizReconsider => translateReconsider(r)
			case n : MizNow => translateNow(n)
			case i : MizIterEquality => translateIterEquality(i)
			case f : MizDefFunc => translateDefFunc(f)
			case p : MizDefPred => translateDefPred(p)
			case _ => println(e)
		}
	}
	
	def translateDefFunc(f : MizDefFunc) = {
	  val name = TranslationController.addGlobalConst(f.nr, "DF")
	  val tp = Arrow(f.argTypes.map(x => Mizar.any), Mizar.any)
	  
	  val con = f.argTypes map {t => 
	    val name = TranslationController.getFreeVar()
	      TranslationController.addLocusVarBinder(OMV(name))
	    val tp = TypeTranslator.translateTyp(t)
	    VarDecl(name, tp)
	  }
	  val df = Lambda(Context(con :_*), TypeTranslator.translateTerm(f.term))
	  
	  val c = makeConstant(name, Some(tp), Some(df))
	  TranslationController.add(c)	  	  
	}
	
	def translateDefPred(p : MizDefPred) = {
	  val name = TranslationController.addGlobalConst(p.nr, "DP")
	  val tp = Arrow(p.argTypes.map(x => Mizar.any), Mizar.any)
	  
	  val con = p.argTypes map {t => 
	    val name = TranslationController.getFreeVar()
	      TranslationController.addLocusVarBinder(OMV(name))
	    val tp = TypeTranslator.translateTyp(t)
	    VarDecl(name, tp)
	  }
	  val df = Lambda(Context(con :_*), PropositionTranslator.translateFormula(p.form))
	  
	  val c = makeConstant(name, Some(tp), Some(df))
	  TranslationController.add(c)
	}
	
	def translateIterEquality(i : MizIterEquality) = {
	  val name = TranslationController.getLmName(i.nr)
	  TranslationController.addGlobalProp(i.nr, name)
	  val terms = (i.term :: i.iterSteps.map(_.term)).map(TypeTranslator.translateTerm)
	  val form = Mizar.apply(Mizar.constant("eq"), terms :_*)
	  val tp = Mizar.proof(form)
	  val c = makeConstant(name, form)
	  TranslationController.add(c)	  
	}
	
	
	def translateNow(n : MizNow) = {
	  val form = PropositionTranslator.translateFormula(n.blockThesis.form) //TODO add justification
	  val name = TranslationController.getLmName(n.nr)
	  TranslationController.addGlobalProp(n.nr, name)
	  val c = makeConstant(name, form)
	  TranslationController.add(c)
	}
	
	def translateSet(s : MizSet) = {
	  val tm = TypeTranslator.translateTerm(s.term)
	  val tp = TypeTranslator.translateTyp(s.typ)
	  val name = TranslationController.addGlobalConst(s.constnr, "C")
	  val c = makeConstant(name, Some(tp), Some(tm))
	  TranslationController.add(c) 
	}
	
	def translateConsider(c : MizConsider) = {
	  val startnr = c.constnr
	  val ex_prop = PropositionTranslator.translateProposition(c.prop)
	  val ex_c = makeConstant(LocalName("ex_prop_" + startnr), ex_prop)
	  TranslationController.add(ex_c)
	  val typs = c.typs.map(TypeTranslator.translateTyp)
	  
	  var i : Int = 0
	  while (i < typs.length) {
	    val name = TranslationController.addGlobalConst(startnr + i, "C")
	    val const = makeConstant(name, typs(i))
	    TranslationController.add(const)
	    i += 1
	  }
	  
	  val props = c.props.map(PropositionTranslator.translateProposition)
	  i = 0
	  while (i < props.length) {
	    val name = "prop_" + startnr + "_" + i
       val const = makeConstant(name, props(i))
	    TranslationController.add(const)
	    i += 1
	  }
	   
	}
	
	def translateReconsider(r : MizReconsider) = {
	  val startnr = r.constnr
	  val tms = r.terms.map(p => (TypeTranslator.translateTyp(p._1),TypeTranslator.translateTerm(p._2)))
	  val prop = PropositionTranslator.translateProposition(r.prop)
	  
	  var i : Int = 0
	  while (i < tms.length) {
	    val name = TranslationController.addGlobalConst(startnr + i, "C")
       val const = makeConstant(name, Some(tms(i)._1), Some(tms(i)._2))
	    TranslationController.add(const)
	    
	    val propname = name + "_prop"
	    val c_prop = makeConstant(LocalName(propname), prop)
	    TranslationController.add(c_prop)
	    i += 1
	  }
	}
	
	def translateNotation(n : MizNotation) = {
		val tp = n.antonymic match  {
			case true => Mizar.not(MMTResolve(n.constrAid, n.kind, n.constrAbsnr))
			case false => MMTResolve(n.constrAid, n.kind, n.constrAbsnr)
		}
		val name = "N" + n.kind + n.nr 
		val nt = makeConstant(LocalName(name), tp)
		TranslationController.addSourceRef(nt, n)
		TranslationController.add(nt)
	}
	
	def translateJustifiedTheorem(j : MizJustifiedTheorem) {
	    val name = "T" + j.nr
	    TranslationController.addGlobalProp(j.prop.nr, name)
		val tp = Mizar.compact(Mizar.proof(PropositionTranslator.translateProposition(j.prop)))
		val df = None //Mizar.compact(JustificationTranslator.translateJustification(j.just))
		
		val jt = makeConstant(LocalName(name), Some(tp), df)
		
	    TranslationController.addSourceRef(jt, j)
		TranslationController.add(jt)
	}
	
	def translateLemma(l : MizLemma) = {
	  val name = TranslationController.getLmName(l.prop.nr)
	  TranslationController.addGlobalProp(l.prop.nr, name)
	  //val matches = ("prop" / PropositionTranslator.translateProposition(l.prop))
	  val matches = List(PropositionTranslator.translateProposition(l.prop))
	  val pattern = artPatterns.Lemma
	  val i = MizInstance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.path, matches)
	  TranslationController.addSourceRef(i, l)
	  TranslationController.add(i)
	}
}