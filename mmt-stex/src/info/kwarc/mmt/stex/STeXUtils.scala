package info.kwarc.mmt.stex

import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.utils._

import scala.io.{BufferedSource, Codec}
import scala.util.matching.Regex

object STeXUtils {
  val c = java.io.File.pathSeparator

  def mathHubDir(bt: BuildTask): File = bt.archive.root.up.up.up

  def extBase(bt: BuildTask): File = mathHubDir(bt) / "ext"

  def stexStyDir(bt: BuildTask): File = extBase(bt) / "sTeX" / "sty"

  def styPath(bt: BuildTask): File = mathHubDir(bt) / "sty"

  def sysEnv(v: String): String = sys.env.getOrElse(v, "")

  def env(bt: BuildTask): List[(String, String)] = {
    val sty = "STEXSTYDIR"
    val tex = "TEXINPUTS"
    val styEnv = sysEnv(sty)
    List(
      sty -> (if (styEnv.isEmpty) stexStyDir(bt).toString else styEnv),
      tex -> (".//" + c + styPath(bt) + c + stexStyDir(bt) + "//"
        + c + sysEnv(tex)))
  }

  def archString(a: Archive): String = a.root.up.getName + "/" + a.root.getName

  def getLang(f: File): Option[String] = f.stripExtension.getExtension

  def getAmbleFile(preOrPost: String, bt: BuildTask): File = {
    getAmbleFile(preOrPost, bt.archive, getLang(bt.inFile))
  }

  private def getLangAmbleFile(defaultFile: File, lang: Option[String]): File =
    if (lang.isDefined) {
      val langFile = defaultFile.stripExtension.setExtension(lang.get + ".tex")
      if (langFile.exists)
        langFile
      else defaultFile
    }
    else defaultFile

  def groupMetaInf(a: Archive): File = a.root.up / "meta-inf"

  def getAmbleFile(preOrPost: String, a: Archive, lang: Option[String]): File = {
    def ambleFile(root: File): File = (root / "lib" / preOrPost).setExtension("tex")
    val repoFile = getLangAmbleFile(ambleFile(a.root), lang)
    if (repoFile.exists())
      repoFile
    else getLangAmbleFile(ambleFile(groupMetaInf(a)), lang)
  }

  def readSourceRebust(f: File): BufferedSource =
    scala.io.Source.fromFile(f)(Codec.ISO8859)

  def stripComment(line: String): String = {
    val idx = line.indexOf('%')
    idx match {
      case -1 => line
      case 0 => ""
      case _ =>
        val l = line.substring(0, idx)
        if (l.charAt(idx - 1) == '\\')
          l + "%" + stripComment(line.substring(idx + 1))
        else l
    }
  }

  def langFiles(lang: Option[String], files: List[String]): List[String] =
    files.filter(f => getLang(File(f)) == lang)

  def mkRegGroup(l: List[String]): String = l.mkString("(", "|", ")")

  private val importKeys: List[String] = List(
    "guse", "gimport", "usemhmodule", "importmhmodule", "includemhproblem", "begin\\{modnl\\}",
    "mhinputref", "mhtikzinput", "cmhtikzinput", "tikzinput", "ctikzinput"
  )
  val importRegs: Regex = ("\\\\" + mkRegGroup(importKeys)).r
  val groups: Regex = "\\\\\\w*\\*?(\\[(.*?)\\])?\\{(.*?)\\}.*".r
  val includeMhProblem: Regex = "\\\\includemhproblem(\\[(.*?)\\])?\\{(.*?)\\}.*".r
  val beginModnl: Regex = "\\\\begin\\{modnl\\}\\[.*?\\]?\\{(.*?)\\}.*".r
  val mhinputRef: Regex = "\\\\mhinputref(\\[(.*?)\\])?\\{(.*?)\\}.*".r
  val tikzinput: Regex = ".*\\\\c?m?h?tikzinput(\\[(.*?)\\])?\\{(.*?)\\}.*".r
  val smsKeys: List[String] = List(
    "gadopt", "symdef", "abbrdef", "symvariant", "keydef", "listkeydef",
    "importmodule", "gimport", "adoptmodule", "importmhmodule", "adoptmhmodule"
  )
  val smsTopKeys: List[String] = List(
    "module", "importmodulevia", "importmhmodulevia"
  )
  val smsRegs: Regex = {
    val alt: String = smsTopKeys.mkString("\\{(", "|", ")\\}")
    ("^\\\\(" + mkRegGroup(smsKeys) + "|begin" + alt + "|end" + alt + ")").r
  }
  val importMhModule: Regex = "\\\\importmhmodule\\[(.*?)\\](.*?)".r
  val gimport: Regex = "\\\\gimport\\*?(\\[(.*?)\\])?\\{(.*?)\\}.*".r


  def entryToPath(p: String) = File(p).setExtension("tex").toFilePath

  def noAmble(f: File): Boolean = {
    val source = readSourceRebust(f)
    var res = false
    for (l <- source.getLines(); if !res)
      if (l.trim.startsWith("\\documentclass")) res = true
    res
  }

  def getProfile(a: Archive): Option[String] = {
    val key = "profile"
    var opt = a.properties.get(key)
    if (opt.isEmpty) {
      val gm: File = groupMetaInf(a) / "MANIFEST.MF"
      if (gm.exists && gm.isFile)
        opt = File.readProperties(gm).get(key)
    }
    opt
  }
}

