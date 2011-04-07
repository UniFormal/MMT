package mizar.translator

import mizar.objects._
import mizar.reader._
import mmt.objects._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.presentation._
import info.kwarc.mmt.api.lf._
import scala.xml._

object Translator {
	var lib : List[String] = "HIDDEN" :: Nil

	def parseVocabularies(n : Node) : List[String] = {
		n.child.filter(x => (x.label == "Vocabulary")).map(parseVocabulary).toList 
}

def parseVocabulary(n : Node) : String = {
		val aid = (n.child(0) \ "@name").text
		aid
}

def getNode(source : String) : Node = {
		val src = scala.io.Source.fromFile(source)
		val cp = scala.xml.parsing.ConstructingParser.fromSource(src, false)
		val input : scala.xml.Node = cp.document()(0)
		src.close
		input
}

def printArticle(aid : String) {
	
	val docPath = "/home/mihnea/work/omdoc/set_theories/mizar/" + aid + ".omdoc"
	val pp = new scala.xml.PrettyPrinter(100,2)
	
	val out = new java.io.FileWriter(docPath)
	
	val doc = TranslationController.controller.get(new DPath(new xml.URI(aid)))
	val th = TranslationController.controller.get(new DPath(new xml.URI(aid)) ? aid)
	
	
	val docNode = pp.format(<omdoc xmlns="http://omdoc.org/ns" xmlns:om="http://www.openmath.org/OpenMath" base={docPath}>{th.toNode}</omdoc>)
	out.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + docNode.toString)
	out.close
}

def translationInit() {
	//controller-init

	TranslationController.controller.handle(ExecFile(new java.io.File("m2o-startup.mmt")))
}

def translateArticle(base : String, aid : String) {
	val name = aid.toLowerCase()
	println("Attempting translation for article " + name) //TODO use logger
	if (!lib.contains(aid)) {
		println("Translating article " +  name) //TODO use logger
		//files
		val xmlabs = "/home/mihnea/work/mizar/absxml/" + name + ".xmlabs" //TODO perhaps replace
		val dcx = base + name + ".dcx"
		val idx = base + name + ".idx"
		val frx = base + name + ".frx"
		val vcl = base + name + ".vcl"


		val voc = parseVocabularies(getNode(vcl))
		val fv = voc.filter(s => s != aid)
		fv.map(s => translateArticle(base,s))	


		UtilsReader.parseSymbols(getNode(dcx))
		UtilsReader.parseSymbols(getNode(idx))
		UtilsReader.parseFormats(getNode(frx))

		ArticleParser.parseArticle(getNode(xmlabs));
		val art : Option[MizArticle] = ParsingController.currentArticle
		art match {
		case None => println("Error Parsing article")
		case Some(article) => {
			ArticleTranslator.translateArticle(article)
			//TranslationController.controller.presenter(TranslationController.controller.get(new DPath(new xml.URI(article.title))), GlobalParams(ConsoleWriter, new DPath(new xml.URI("foundations/lf/ascii.omdoc")) ? "ascii" ))
			printArticle(article.title)
		}
		}
	} else {
		println("Article " + name + " already translated -- skipping") //TODO use logger
	}
}
}
