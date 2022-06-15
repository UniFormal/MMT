package info.kwarc.mmt.stex.parsing

import info.kwarc.mmt.api.{DPath, LNStep, MPath, SimpleStep}
import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.utils.File

class Dictionary(controller:Controller) {
  private class Module(path:MPath) {
    var rules: List[TeXRule] = Nil
  }
  private var current_archive: List[Archive] = Nil
  private var current_file: List[File] = Nil
  private var current_modules : List[Module] = Nil
  private var all_modules : List[Module] = Nil
  def inFile[A](file : File,a:Option[Archive] = None)(f: => A) : A = {
    a.foreach(current_archive ::= _)
    current_file ::= file
    val ret : A = f
    current_file = current_file.tail
    a.foreach(_ => current_archive = current_archive.tail)
    ret
  }
  def openModule(name : String) = {
    var rel = current_archive.headOption match {
      case Some(a) =>
        (a / info.kwarc.mmt.api.archives.source).relativize(current_file.head).stripExtension
      case _ => current_file.head.stripExtension
    }
    val lang = rel.getExtension match {
      case Some(l) =>
        rel = rel.stripExtension
        Some(l)
      case None =>
        None
    }

    val dp : DPath = (current_archive.headOption match {
      case Some(a) => a.ns.map(_.asInstanceOf[DPath])
      case _ => None
    }) match {
      case Some(ns) =>
        ns / (if (rel.name == name) rel.up.segments else rel.segments).map(SimpleStep)
      case None =>
        if (rel.name == name) DPath(rel.up.toURI) else DPath(rel.toURI)
    }

    val mp = dp ? name
    val mod = new Module(mp)
    current_modules ::= mod
    all_modules ::= mod
    mp
  }

}

class STeXRules {

}
