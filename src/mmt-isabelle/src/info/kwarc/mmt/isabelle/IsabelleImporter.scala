package info.kwarc.mmt.isabelle

import info.kwarc.mmt.api._
import archives._
import symbols._
import modules._
import documents._
import objects._
import utils._
import parser._

import info.kwarc.mmt.lf

import Isabelle._

class IsabelleImporter extends Importer {
  def key = "isabelle-omdoc"
  def inExts = List("thy")
  
  def importDocument(bt: BuildTask, index: Document => Unit) = {
    val inText = File.read(bt.inFile)
    
    // a document corresponding to the source file
    val doc = new Document(bt.narrationDPath, root = true)
    controller add doc
    
    val session = ???
    
    // a theory corresponding to a toplevel declaration inside the source file
    val thy = new DeclaredTheory(isaLibraryBase, LocalName(session, "sometheory"), Some(Isabelle.pure), Theory.noParams, Theory.noBase)
    val sref = SourceRef(bt.base / bt.inPath, ???)
    SourceRef.update(thy, sref)
    controller add thy
    controller add MRef(doc.path, thy.path)
    
    // a constant in that theory: c: prop->prop = lambda x:prop.x=>x
    // type: 
    val tp = Fun(Prop(), Prop())
    // definiens
    val x = LocalName("x")
    val t = Imp(OMV(x), OMV(x))
    SourceRef.update(t, ???) // any Term can have a source reference
    val df = Lambda("x", Prop(), t)
    
    val cons = Constant(thy.toTerm, LocalName(Kind.const, "sometheory", "c"), Nil, Some(tp), Some(df), None)
    controller add cons

    // -----------------------
    
    // to refer to cons, we need to build its URI from its Isabelle name
    val fullName = "sometheory.c"
    val theoryFullName = ??? // e.g., getSession(fullName) / getTheoryName(fullName)
    val consPath = isaLibraryBase ? theoryFullName ? Kind.const / fullName
    assert(cons.path == consPath)

    // a polymorphic constant that refers to the previous constant
    val a = OMV("a")
    val tp2 = Typargs(List("a"), Fun(a,Prop()))
    val df2 = Typargs(List("a"), Lambda("x", a, Eq(Prop(), OMV("x"), Apply(OMS(consPath), OMV("x")))))
    val cons2 = Constant(thy.toTerm, LocalName("c2"), Nil, Some(tp2), Some(df2), None)
    controller add cons2
    
    // write the document to disk
    index(doc)
    
    // generate the build result (dependency management is not mature yet but probably not needed for Isabelle anyway) 
    BuildResult.fromImportedDocument(doc)
  }
}

/** convenience funtions for building Isabelle objects */
object Isabelle {
 
  /** common namespace for all theories in all sessions in all Isabelle archives */
  val isaLibraryBase = DPath(URI("http", "isabelle.in.tum.de") / "Isabelle")
   
  /** namespace for MMT definitions of Isabelle built-in features (i.e., things not in the Isabelle library) */
  val isaLogicBase = lf.LF._base
  val pure = isaLogicBase ? "Pure"
  
  object Type {
    def apply() = OMS(lf.Typed.ktype)
  }
  
  object Fun {
    def apply(from: Term, to: Term) = lf.Arrow(from, to)
  }
  
  object Lambda {
    def apply(name: String, tp: Term, body: Term) = lf.Lambda(LocalName(name), tp, body)
  }

  object Apply {
    def apply(fun: Term, arg: Term) = lf.ApplySpine(fun, arg)
  }
  
  object Typargs {
    def apply(names: List[String], t: Term) = {
      val con = names map {n => OMV(n) % Type()}
      lf.Pi(con, t)
    }
  }

  object Prop {
    val path = pure ? "prop"
    def apply() = OMS(path)
  }
  
  object All {
    val path = pure ? "Pure.all"
    def apply(name: String, tp: Term, body: Term) =
      lf.Apply(OMS(path), lf.Lambda(LocalName(name), tp, body))
  }
  
  object Imp {
    val path = pure ? "Pure.imp"
    def apply(left: Term, right: Term) = lf.ApplySpine(OMS(path), left, right)
  }
  
  object Eq {
    val path = pure ? "Pure.eq"
    def apply(tp: Term, left: Term, right: Term) = lf.ApplySpine(OMS(path), tp, left, right)
  }
  
  object Kind {
    val const = "const"
  }
}