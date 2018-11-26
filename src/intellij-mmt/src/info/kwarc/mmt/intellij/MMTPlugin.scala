package info.kwarc.mmt.intellij

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives.lmh.MathHub
import info.kwarc.mmt.api.frontend.{Controller, MMTConfig, ReportHandler}
import info.kwarc.mmt.api.utils.{File, MMTSystem}
import info.kwarc.mmt.intellij.checking.{Checker, ErrorViewer}

import scala.collection.mutable

class MMTPluginInterface(homestr : String) {
  val home = File(homestr)
  private var controller : Controller = _

  private object Abbreviations {
    lazy val pairstrings = MMTSystem.getResourceAsString("latex/unicode-latex-map").split("\n")
    lazy val pairs : List[(String,String)] = pairstrings.collect{ case s if s.nonEmpty =>
      val ls = s.split('|')
      if (ls.length != 2) {
        println(ls.mkString(", "))
        ???
      }
      ( /*"""\""" + */ ls.head.trim/*.drop(1)*/,ls.last.trim)
    }.filterNot(p => List("❚","❙","❘").contains(p._2)).toList
  }
  private object LocalMathHub {
    lazy val mathhub = new MathHub(controller,home,MathHub.defaultURL,true) {
      lazy val all = available_()
    }
    def remotes = mathhub.all.filter(id => !localArchs.exists(_._1 == id))
    def archives = controller.backend.getArchives.map(_.id)
    def localGroups = archives.map { id =>
      if (id.contains("/")) id.split('/').head
      else "Others"
    }.distinct
    def remoteGroups = mathhub.all.collect{
      case id if id.contains("/") =>
        id.split('/').head
    }.distinct.filter(!localGroups.contains(_))
    def localArchs = archives.map{ id => (id,
      if (id.contains("/")) id.split('/').mkString(".")
      else "Others." + id)
    }
  }

  def archiveInfo(id : String) = {
    val archive = controller.backend.getArchive(id).get
    val meta_inf = archive.root / "META-INF"
    val source = archive / archives.source
    val scala = archive.root / "scala" // TODO redirectable dimension, if existent
    (source.toString,meta_inf.toString,scala.toString)
  }
  def localGroups() = LocalMathHub.localGroups.toArray
  def remoteGroups() = LocalMathHub.remoteGroups.toArray
  def localArchs() = LocalMathHub.localArchs.toArray
  def remoteArchs() = LocalMathHub.remotes.toArray
  def version() = controller.getVersion
  private lazy val ev = new ErrorViewer(controller)
  def errorViewer() = ev
  private lazy val check = new Checker(controller,ev)
  def checker() = check
  def shell(_doLine : Any) = {
    val doLine = _doLine.asInstanceOf[{
      def apply(v1 : String) : Unit
    }]
    val handler = new ReportHandler("IntelliJ Shell") {
      override def apply(ind: Int, caller: => String, group: String, msgParts: List[String]): Unit = {
        msgParts.foreach {msg => doLine(indentString(ind) + group + ": " + msg)}
      }
    }
    controller.report.addHandler(handler)
  }
  def handleLine(s : String) = controller.handleLine(s)
  def abbrevs() : Array[(String,String)] = Abbreviations.pairs.toArray
  def clear() : Unit = controller.clear
  def getArchiveRoots() = LocalMathHub.archives.map { id =>
    controller.backend.getArchive(id).get.root.toString
  }.toArray
  def install(id : String) = {
    LocalMathHub.mathhub.installEntry(id,None,true)
    while (controller.backend.getArchive(id).isEmpty) Thread.sleep(100)
  }

  // private val psithings : mutable.HashMap[Any,(DPath,NamespaceMap)] = mutable.HashMap.empty
  def parseNamespace(s: String) : String = Path.parseD(s, controller.getNamespaceMap).toString
  def getNamespaceFromFile(filestr : String) : String = {
    val file = File(filestr)
    controller.backend.resolvePhysical(file) match {
      case Some((a,_)) => a.ns match {
        case Some(d : DPath) => return d.toString
        case _ =>
      }
      case _ =>
    }
    controller.getNamespaceMap.base.doc.toString
  }
  def getNamespaceMap(ns : String, pars : List[(String,String)]) = {
    val dp = Path.parseD(ns,controller.getNamespaceMap)
    var nsm = controller.getNamespaceMap(dp)
    pars foreach { case (a,p) =>
      nsm = nsm.add(a,Path.parse(p,nsm).toPath)
    }
    nsm
  }
  def resolvePathSimple(str : String,nsm : AnyRef) = {
    val nsMap = nsm.asInstanceOf[NamespaceMap]
    Path.parse(str,nsMap).toString
  }
  def resolvePath(wordS: String,nsm : AnyRef) = { // TODO from [[NotationBasedParser.makeIdentifier]]
    var word = wordS
    val nsMap = nsm.asInstanceOf[NamespaceMap]
    val segments = utils.stringToList(word, "\\?")
    // recognizing identifiers ?THY?SYM is awkward because it would require always lexing initial ? as identifiers
    // but we cannot always prepend ? because the identifier could also be NS?THY
    // Therefore, we turn word into ?word using a heuristic
    segments match {
      case fst :: _ :: Nil if !fst.contains(':') && fst != "" && Character.isUpperCase(fst.charAt(0)) =>
        word = "?" + word
      case _ =>
    }
    // recognizing prefix:REST is awkward because : is usually used in notations
    // therefore, we turn prefix/REST into prefix:/REST if prefix is a known namespace prefix
    // this introduces the (less awkward problem) that relative paths may not start with a namespace prefix
    val beforeFirstSlash = segments.headOption.getOrElse(word).takeWhile(_ != '/')
    if (!beforeFirstSlash.contains(':') && nsMap.get(beforeFirstSlash).isDefined) {
      word = beforeFirstSlash + ":" + word.substring(beforeFirstSlash.length)
    }
    try {
      Path.parse(word, nsMap).toString
    } catch {
      case ParseError(msg) =>
        null
    }
  }
  def getReference(uri : String) = Path.parse(uri) match {
    case mp : MPath if mp.parent.uri.scheme contains "scala" =>
      Some(SemanticObject.mmtToJava(mp,true))
    case _ => None
  }

  def init(): Unit = {
    controller = new Controller
    /** Options */
    val mslf = home / "startup.msl"
    if (mslf.toJava.exists())
      controller.runMSLFile(mslf, None)
    else {
      mslf.createNewFile()
      File.append(mslf,"extension info.kwarc.mmt.odk.Plugin")
    }

    val rc = home / "mmtrc"
    if(!rc.toJava.exists()) {
      rc.createNewFile()
      File.append(rc,"\n","#backends\n","lmh .")
    }

    controller.loadConfig(MMTConfig.parse(rc), false)

    /** MathHub Folder */
    controller.setHome(home)
    controller.addArchive(home)
  }
}
