package info.kwarc.mmt.isabelle

import info.kwarc.mmt.api._
import archives._
import symbols._
import modules._
import documents._
import objects._
import utils._

import Isabelle._

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
    
    // a constant in that theory: c: prop->prop = lambda x:prop.x=>x
    // type: 
    val tp = Fun(List(Prop()), Prop())
    // definiens
    val x = LocalName("x")
    val df = Lambda(Context(VarDecl(x, Prop())), Implies(List(OMV(x)), OMV(x)))
    val cons = Constant(OMMOD(thy.path), LocalName("c"), Nil, Some(tp), Some(df), None)
    controller add cons
    
    // write the document to disk
    index(doc)
    
    // generate the build result (dependency management is not mature yet but probably not needed for Isabelle anyway) 
    // dependencies
    val deps = Nil
    // provided resources
    val provided = List(LogicalDependency(thy.path))
    BuildSuccess(deps, provided)
  }
}

/** convenience funtions for building Isabelle objects */
// @Makarius: adapt as needed to match Isabelle internals (e.g., I didn't know if Fun is binary or n-ary, or if Implies is applied via Apply)
object Isabelle {
  /** namespace for MMT definitions of Isabelle built-in features (i.e., things not in the Isabelle library) */
  val isaLogicBase = DPath(URI("http", "isabelle.in.tum.de") / "logic")
  val pure = isaLogicBase ? "Pure"
  
  object Fun {
    val path = pure ? "fun"
    def apply(from: List[Term], to: Term) = OMA(OMS(path), from ::: List(to))
  }
  
  object Lambda {
    val path = pure ? "lambda"
    def apply(bindings: Context, body: Term) = OMBIND(OMS(path), bindings, body)
  }

  object Apply {
    val path = pure ? "apply"
    def apply(fun: Term, arg: List[Term]) = OMA(OMS(path), fun::arg)
  }

  object Prop {
    val path = pure ? "prop"
    def apply() = OMS(path)
  }
  
  object Forall {
    val path = pure ? "forall"
    def apply(name: String, tp: Term, body: Term) = OMBIND(OMS(path), Context(VarDecl(LocalName(name), tp)), body)
  }
  
  object Implies {
    val path = pure ? "implies"
    def apply(left: List[Term], right: Term) = OMA(OMS(path), left ::: List(right))
  }
  
  object Equal {
    val path = pure ? "equal"
    def apply(tp: Term, left: Term, right: Term) = OMA(OMS(path), List(tp, left, right))
  }
}