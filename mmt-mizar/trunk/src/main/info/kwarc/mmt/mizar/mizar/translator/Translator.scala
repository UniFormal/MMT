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

class MizarCompiler extends Compiler {
	val lib : collection.mutable.Map[Int,List[String]] = new collection.mutable.HashMap[Int,List[String]]
	def addToLib(version : Int, article : String) {
	  if (!lib.contains(version)) {
	    lib(version) = "HIDDEN" :: Nil //default initialization
	  }
	  lib(version) ::= article
	}
	
	def isInLib(version : Int, article : String) : Boolean = {
	  if (lib.contains(version)) {
	    lib(version).contains(article)
	  } else {
	    article == "HIDDEN"
	  }
	}
	

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
	
	
	def printArticle(mml : String, version : Int, aid : String) {
		val docPath = mml + "/compiled/" + version + "/" + aid + ".omdoc"
		val base = Mizar.mmlBase / version.toString
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

override def init(cont : Controller, args : List[String] = Nil) {
	this.controller = cont
    report = controller.report
    //controller-init
	//TranslationController.controller.handle(ExecFile(File(new java.io.File("m2o-startup.mmt"))))
    TranslationController.controller = cont
}


def isApplicable(src : String) : Boolean = {
  //val len = src.length
  //src.substring(len-4,len) == ".miz"
  src == "mizar"
}

def getBase(f : File) : String = {
  f.toJava.getParentFile().getParentFile().getParent() match {
    case null => "./"
    case s => s + "/"
  }
}

def getVersion(f : File) : Int = {
  f.toJava.getParentFile().getName() match {
    case null => 1132
    case s => s.toInt
  }
}

def getAid(f : File) : String = {
  val name = f.toJava.getName() 
  val posOfDot = name.lastIndexOf(".")
  if (posOfDot == -1) name else name.substring(0,posOfDot)

}

override def compile(in : File, out : File) : List[SourceError] = {
  val base = getBase(in)
  val version = getVersion(in)
  val aid = getAid(in)
  translateArticle(base, version, aid.toUpperCase())
  Nil
}

def compileLibrary(files : List[File]) : List[SourceError] = {
  files.map(f => translateArticle(getBase(f), getVersion(f), getAid(f).toUpperCase()))
  Nil 
}


def translateArticle(mml : String, version : Int, aid : String) : Unit = {
	val name = aid.toLowerCase()
	//println("attempting to translate article " + name)
	if (!isInLib(version, aid)) {
		//files
		val xmlabs = mml + "/export/" + version.toString + "/" + name + ".xmlabs" //TODO perhaps replace
		val dcx = mml + "/export/" + version.toString + "/" + name + ".dcx"
		val idx = mml + "/export/" + version.toString + "/" + name + ".idx"
		val frx = mml + "/export/" + version.toString + "/" + name + ".frx"
		val sgl = mml + "/export/" + version.toString + "/" + name + ".sgl"
		
		var voc : List[String] = Nil
		scala.io.Source.fromFile(sgl).getLines().foreach(s => if (s.charAt(0).isLetter) voc = voc :+ s)

		//parseVocabularies(getNode(vcl))
		val fv = voc.filter(s => s != aid)
		fv.map(s => translateArticle(mml, version, s))	

		println("Translating article " +  name) //TODO use logger
		TranslationController.currentBase = mml
        TranslationController.currentVersion = version
        TranslationController.currentAid = aid

		UtilsReader.parseSymbols(getNode(dcx))
		UtilsReader.parseSymbols(getNode(idx))
		UtilsReader.parseFormats(getNode(frx))
		
		ParsingController.selectors(aid) = new scala.collection.mutable.HashMap[Int,Tuple2[Int,Int]]
		ParsingController.attributes(aid) = new scala.collection.mutable.HashMap[Int,Int]

		//sets TranslationController.currentAid as article.title
		ArticleParser.parseArticle(getNode(xmlabs))
		val article = ParsingController.buildArticle()
		
		val path = TranslationController.currentDocument
		val d = new Document(path)
		
		TranslationController.add(d)

		val th = new DeclaredTheory(TranslationController.currentDocument, TranslationController.localPath, Some(Mizar.MizarPatternsTh))	

		TranslationController.add(th)
		fv.map(x => {if (x != "HIDDEN" && !TranslationController.controller.library.imports(OMMOD(MMTUtils.getTheoryPath(x)),OMMOD(th.path)))
			TranslationController.add(PlainInclude(MMTUtils.getTheoryPath(x) , th.path))
		})
		//TranslationController.add(PlainInclude(Mizar.HiddenTh, th.path))
		
		TranslationController.controller.add(MRef(d.path, th.path, true))
		ArticleTranslator.translateArticle(article)
	    
		printArticle(mml, version, article.title)
		addToLib(version, aid)
		
		TranslationController.clear()
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
		
	}
}
}
