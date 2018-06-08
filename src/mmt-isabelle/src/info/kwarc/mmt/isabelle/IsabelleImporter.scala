package info.kwarc.mmt.isabelle

import info.kwarc.mmt.api._
import archives._
import symbols._
import modules._
import documents._
import objects._
import utils._

class IsabelleImporter extends Importer {
  def key = "isabelle-omdoc"
  def inExts = List("thy")
  
  def importDocument(bt: BuildTask, index: Document => Unit) = {
    val inText = File.read(bt.inFile)
    
    // a document corresponding to the source file
    val doc = new Document(bt.narrationDPath, root = true)
    controller add doc

    // a theory corresponding to a toplevel declaration inside the source file
    val thy = new DeclaredTheory(doc.path, LocalName("sometheory"), Some(Isabelle.pure), Theory.noParams, Theory.noBase)
    controller add thy
    controller add MRef(doc.path, thy.path)
    
    // a constant in that theory
    // type
    val tp = None
    // definiens
    val df = Some(OMSemiFormal(Text("dummy", inText)))
    val cons = Constant(OMMOD(thy.path), LocalName("someconstant"), Nil, tp, df, None)
    controller add cons
    
    index(doc)
    
    // generate the build result (dependency management is not mature yet but probably not needed for Isabelle anyway) 
    // dependencies
    val deps = Nil
    // provided resources
    val provided = List(LogicalDependency(thy.path))
    BuildSuccess(deps, provided)
  }
}

object Isabelle {
  /** namespace for MMT definitions of Isabelle built-in features (i.e., things not in the Isabelle library) */
  val isaLogicBase = DPath(URI("http", "isabelle.in.tum.de") / "logic")
  val pure = isaLogicBase ? "Pure"  
}