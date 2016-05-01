package info.kwarc.mmt.mizar.mizar.translator

//TODO package name does not match directory structure

import info.kwarc.mmt.api._
import archives.BuildResult
import documents._
import info.kwarc.mmt.mizar.mizar.objects._
import info.kwarc.mmt.mizar.mizar.reader._
import info.kwarc.mmt.mizar.mmt.objects._
import modules._
import objects._
import symbols._
import utils._

//import scala.xml._

class MizarCompiler extends archives.Importer {
  val key = "mizar-omdoc"

  def inExts = List("miz")

  var lib:  List[String] = "HIDDEN" :: Nil

  def addToLib(article: String) {
    lib ::= article
  }

  def isInLib(article: String): Boolean = {
    lib.contains(article)
  }

  def parseVocabularies(n: scala.xml.Node): List[String] = {
    n.child.filter(x => x.label == "Vocabulary").map(parseVocabulary).toList
  }

  def parseVocabulary(n: scala.xml.Node): String = {
    val aid = (n.child.head \ "@name").text
    aid
  }

  def getNode(source: String): scala.xml.Node = {
    val src = scala.io.Source.fromFile(source)
    val cp = scala.xml.parsing.ConstructingParser.fromSource(src, preserveWS = false)
    val input: scala.xml.Node = cp.document()(0)
    src.close
    input
  }

  /*
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
  }*/

  override def start(args: List[String] = Nil) {
    //TranslationController.controller.handle(ExecFile(File(new java.io.File("m2o-startup.mmt"))))
    TranslationController.controller = controller
  }

  def getBase(f: File): String = {
    f.toJava.getParentFile.getParent match {
      case null => "./"
      case s => s + "/"
    }
  }

  def getAid(f: File): String = {
    val name = f.toJava.getName
    val posOfDot = name.lastIndexOf(".")
    if (posOfDot == -1) name else name.substring(0, posOfDot)

  }
  
  def importDocument(bf: archives.BuildTask, seCont: documents.Document => Unit): BuildResult = {
    val mmlBase = getBase(bf.inFile)
    val aid = getAid(bf.inFile)
    translateArticle(mmlBase, aid.toUpperCase(), bf.narrationDPath.^!)
    val mizdpath = getDPath(bf.narrationDPath.^!, aid.toLowerCase())
    val doc = controller.getDocument(mizdpath)
    log("INDEXING ARTICLE: " + bf.narrationDPath.last)
    seCont(doc)
    BuildResult.empty
  }

  def getDPath(docBase: DPath, name: String): DPath = {
    docBase / (name + ".omdoc")
  }

  /*
  def compileLibrary(files : List[File]) : List[SourceError] = {
    files.map(f => translateArticle(getBase(f), getVersion(f), getAid(f).toUpperCase()))
    Nil
  }
  */

  def translateArticle(mml: String, aid: String, docBase: DPath) {
    val name = aid.toLowerCase()
    if (isInLib(aid)) //already translated it
      return
    //files
    val xmlabs = mml + "/source/" + name + ".xmlabs" //TODO perhaps replace
    val dcx = mml + "/source/" + name + ".dcx"
    val idx = mml + "/source/" + name + ".idx"
    val frx = mml + "/source/" + name + ".frx"
    val sgl = mml + "/source/" + name + ".sgl"

    var voc: List[String] = Nil
    scala.io.Source.fromFile(sgl).getLines().foreach(s => if (s.charAt(0).isLetter) voc = voc :+ s)

    //parseVocabularies(getNode(vcl))
    val fv = voc.filter(s => s != aid)
    fv.foreach(s => translateArticle(mml, s, docBase))

    println("Translating article " + name)

    TranslationController.currentBase = mml
    TranslationController.currentAid = aid
    UtilsReader.parseFormats(getNode(frx))
    UtilsReader.parseSymbols(getNode(dcx))
    UtilsReader.parseSymbols(getNode(idx))

    ParsingController.selectors(aid) = new scala.collection.mutable.HashMap[Int, (Int, Int)]
    ParsingController.attributes(aid) = new scala.collection.mutable.HashMap[Int, Int]

    //sets TranslationController.currentAid as article.title
    ArticleParser.parseArticle(getNode(xmlabs))
    val article = ParsingController.buildArticle()

    val dpath = getDPath(docBase, name)
    val doc = new Document(dpath, true)
    TranslationController.add(doc)
    val th = new DeclaredTheory(TranslationController.currentThyBase, TranslationController.localPath, Some(Mizar.MizarPatternsTh))

    TranslationController.add(th)
    fv.map(x => {
      if (x != "HIDDEN" && !TranslationController.controller.library.hasImplicit(OMMOD(MMTUtils.getTheoryPath(x)), OMMOD(th.path)))
        TranslationController.add(PlainInclude(MMTUtils.getTheoryPath(x), th.path))
    })
    //TranslationController.add(PlainInclude(Mizar.HiddenTh, th.path))
    TranslationController.add(MRef(doc.path, th.path))
    ArticleTranslator.translateArticle(article)
    TranslationController.clear()
    ParsingController.dictionary.clear()
    addToLib(aid)
  }
}
