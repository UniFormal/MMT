package info.kwarc.mmt.mizar

import info.kwarc.mmt._
import info.kwarc.mmt.api._
import documents._
import modules._
import info.kwarc.mmt.mizar.mmt.objects._
import org.omdoc.latin.foundations.mizar.MizarPatterns

/*
class Plugin extends frontend.Plugin {
  val theory = Mizar.MizarPatternsTh
  val dependencies = Nil
  
  override def start(args: List[String]) {
    val dpath = theory.doc
    val doc = new Document(dpath, true)
    controller.add(doc)
    val thy = new DeclaredTheory(dpath, theory.name, Some(sequences.LFS._path))
    val incl = symbols.PlainInclude(Mizar.HiddenTh, thy.path)
    thy.add(incl)
    MizarPatterns.patterns foreach {p => 
      thy.add(p)
    }
    controller.add(thy)
    controller.add(MRef(dpath, thy.path))
  }
}
*/