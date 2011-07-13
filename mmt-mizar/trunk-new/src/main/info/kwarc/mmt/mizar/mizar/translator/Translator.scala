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
import info.kwarc.mmt.api.presentation._
import info.kwarc.mmt.api.lf._
//import scala.xml._

object Translator {
	var lib : List[String] = "HIDDEN" :: Nil

	def parseVocabularies(n : scala.xml.Node) : List[String] = {
		n.child.filter(x => (x.label == "Vocabulary")).map(parseVocabulary).toList 
}

def parseVocabulary(n : scala.xml.Node) : String = {
		val aid = (n.child(0) \ "@name").text
		aid
}

def getNode(source : String) : scala.xml.Node = {
		val src = scala.io.Source.fromFile(source)
		val cp = scala.xml.parsing.ConstructingParser.fromSource(src, false)
		val input : scala.xml.Node = cp.document()(0)
		src.close
		input
}

def printArticle(aid : String) {
	val docPath = "/home/mihnea/kwarc/omdoc/set_theories/mizar/" + aid + ".omdoc" //TODO integrate with mmt
	val base = mmt.baseURI / "set_theories" / "mizar" / (aid + ".omdoc")
	val pp = new scala.xml.PrettyPrinter(100,2)
	
	val out = new java.io.FileWriter(docPath)
	
	val doc = TranslationController.controller.getDocument(new DPath(base))
	//println(doc.toNodeResolved(TranslationController.controller.library))
	
	//println(TranslationController.controller.library.toString)
	val th = TranslationController.controller.get(new DPath(base) ? aid)
	
	
	val nd : scala.xml.Node = 
	<omdoc xmlns="http://omdoc.org/ns" xmlns:om="http://www.openmath.org/OpenMath" base={base.toString}>
		{th.toNode}
	</omdoc>
	
	val docNode = pp.format(nd)
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
		val xmlabs = "absxml/" + name + ".xmlabs" //TODO perhaps replace
		val dcx = base + name + ".dcx"
		val idx = base + name + ".idx"
		val frx = base + name + ".frx"
		val vcl = base + name + ".vcl"
		val sgl = base + name + ".sgl"

		var voc : List[String] = Nil
		scala.io.Source.fromFile(sgl).getLines().foreach(s => if (s.charAt(0).isLetter) voc = voc :+ s)

		//parseVocabularies(getNode(vcl))
		val fv = voc.filter(s => s != aid)
		fv.map(s => translateArticle(base,s))	


		UtilsReader.parseSymbols(getNode(dcx))
		UtilsReader.parseSymbols(getNode(idx))
		UtilsReader.parseFormats(getNode(frx))
		
		
		ArticleParser.parseArticle(getNode(xmlabs))
		val article = ParsingController.buildArticle()


		TranslationController.currentAid = article.title

		val path = new DPath(mmt.baseURI / "set_theories" / "mizar" /  (article.title + ".omdoc"))
		val d = new Document(path)
		TranslationController.add(d)
		TranslationController.currentDocument = d.path

		val th = new DeclaredTheory(d.path, LocalPath(article.title :: Nil), Some(Mizar.MizarTh)) //None		

		TranslationController.add(th)
		fv.map(x => {if (!TranslationController.controller.library.imports(OMMOD(MMTUtils.getTheoryPath(x)),OMMOD(th.path)))
			TranslationController.add(PlainInclude(MMTUtils.getTheoryPath(x) , th.path))
		})
		
		//TranslationController.add(PlainInclude(Mizar.HiddenTh, th.path))
		TranslationController.currentTheory  = th.path
		
		TranslationController.controller.add(MRef(d.path, th.path, true))
		ArticleTranslator.translateArticle(article)
		//TranslationController.controller.presenter(TranslationController.controller.get(new DPath(new xml.URI(article.title))), GlobalParams(ConsoleWriter, new DPath(new xml.URI("foundations/lf/ascii.omdoc")) ? "ascii" ))
		printArticle(article.title)
		lib = aid :: lib
	} else {
		println("Article " + name + " already translated -- skipping") //TODO use logger
	}
}
}
