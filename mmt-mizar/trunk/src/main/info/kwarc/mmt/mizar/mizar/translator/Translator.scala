package info.kwarc.mmt.mizar.mizar.translator

import info.kwarc.mmt.mizar.mizar.objects._
import info.kwarc.mmt.mizar.mizar.reader._
import info.kwarc.mmt.mizar.mmt.objects._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.backend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.presentation._
import info.kwarc.mmt.lf._
//import scala.xml._

object MizarCompiler extends Compiler {
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
	
	
	def printArticle(mml : String, aid : String) {
		val docPath = mml + "/compiled/"  + aid + ".omdoc" 
		val base = Mizar.baseURI
		val pp = new scala.xml.PrettyPrinter(100,2)
	
		val th = TranslationController.controller.get(new DPath(base) ? aid)
		
		val out = new java.io.FileWriter(docPath)
	
		
		val nd : scala.xml.Node = 
		<omdoc xmlns="http://omdoc.org/ns" xmlns:om="http://www.openmath.org/OpenMath" base={base.toString}>
			{th.toNode}
		</omdoc>
	
		val docNode = pp.format(nd)
		out.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + docNode.toString)
		out.close
		
	}

override def init(report: Report, args : List[String] = Nil) {
	//controller-init

	TranslationController.controller.handle(ExecFile(new java.io.File("m2o-startup.mmt")))
}


def isApplicable(src : String) : Boolean = {
  val len = src.length
  src.substring(len-4,len) == ".miz"
}

def getBase(f : File) : String = {
  f.toJava.getParentFile().getParent() match {
    case null => "./"
    case s => s + "/"
  }
}

def getAid(f : File) : String = {
  val name = f.toJava.getName() 
  val posOfDot = name.lastIndexOf(".")
  if (posOfDot == -1) name else name.substring(0,posOfDot)

}

override def compile(in : File, out : File) : List[CompilerError] = {
  val base = getBase(in)
  
  val aid = getAid(in)
  
  translateArticle(base,aid.toUpperCase())
  Nil
}

def compileLibrary(files : List[File]) : List[CompilerError] = {
  files.map(f => translateArticle(getBase(f), getAid(f).toUpperCase()))
  
  Nil 
}


def translateArticle(mml : String, aid : String) : Unit = {
	val name = aid.toLowerCase()
	if (!lib.contains(aid)) {
		//files
		val xmlabs = mml + "/export/" + name + ".xmlabs" //TODO perhaps replace
		val dcx = mml + "/export/" + name + ".dcx"
		val idx = mml + "/export/" + name + ".idx"
		val frx = mml + "/export/" + name + ".frx"
		val sgl = mml + "/export/" + name + ".sgl"
		
		var voc : List[String] = Nil
		scala.io.Source.fromFile(sgl).getLines().foreach(s => if (s.charAt(0).isLetter) voc = voc :+ s)

		//parseVocabularies(getNode(vcl))
		val fv = voc.filter(s => s != aid)
		fv.map(s => translateArticle(mml,s))	

		println("Translating article " +  name) //TODO use logger

		UtilsReader.parseSymbols(getNode(dcx))
		UtilsReader.parseSymbols(getNode(idx))
		UtilsReader.parseFormats(getNode(frx))
		
		ParsingController.selectors(aid) = new scala.collection.mutable.HashMap[Int,Tuple2[Int,Int]]
		ParsingController.attributes(aid) = new scala.collection.mutable.HashMap[Int,Int]

		ArticleParser.parseArticle(getNode(xmlabs))
		val article = ParsingController.buildArticle()


		TranslationController.currentAid = article.title

		val path = new DPath(Mizar.baseURI)
		val d = new Document(path)
		
		TranslationController.add(d)
		TranslationController.currentDocument = d.path

		val th = new DeclaredTheory(d.path, LocalPath(article.title :: Nil), Some(Mizar.MizarPatternsTh)) //None		

		TranslationController.add(th)
		fv.map(x => {if (x != "HIDDEN" && !TranslationController.controller.library.imports(OMMOD(MMTUtils.getTheoryPath(x)),OMMOD(th.path)))
			TranslationController.add(PlainInclude(MMTUtils.getTheoryPath(x) , th.path))
		})
		
		//TranslationController.add(PlainInclude(Mizar.HiddenTh, th.path))
		TranslationController.currentTheory  = th.path
		
		TranslationController.controller.add(MRef(d.path, th.path, true))
		ArticleTranslator.translateArticle(article)
		ParsingController.dictionary.clear()
		//TranslationController.controller.presenter(TranslationController.controller.get(new DPath(new xml.URI(article.title))), GlobalParams(ConsoleWriter, new DPath(new xml.URI("foundations/lf/ascii.omdoc")) ? "ascii" ))
		
		//println("pm : " + TranslationController.pm)
		//println("pi : " + TranslationController.pi)
		//println("fm : " + TranslationController.fm)
		//println("fi : " + TranslationController.fi)
		//println("mm : " + TranslationController.mm)
		//println("mi : " + TranslationController.mi)
		//println("am : " + TranslationController.am)
		//println("ai : " + TranslationController.ai)
		
		printArticle(mml, article.title)
		lib = aid :: lib
	} 
}
}
