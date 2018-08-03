package info.kwarc.mmt.isabelle

import info.kwarc.mmt.api._
import archives._
import symbols._
import modules._
import documents._
import objects._
import opaque._
import utils._
import parser._

import info.kwarc.mmt.lf

class Importer extends archives.Importer
{
  importer =>

  val key = "isabelle-omdoc"

  def inExts = List("thy")


  /* Isabelle environment and session */

  object Isabelle extends Isabelle(importer.log(_))

  override def start(args: List[String])
  {
    super.start(args)
    Isabelle.init()
  }

  override def destroy
  {
    Isabelle.exit()
    super.destroy
  }


  /* import theory file */

  def importDocument(bt: BuildTask, index: Document => Unit): BuildResult =
  {
    /* use theories */

    val root_name = Isabelle.import_name(bt.inFile.canonical.stripExtension.getPath)

    val use_theories_result =
      Isabelle.use_theories(List(root_name.path.split_ext._1.implode))


    /* theory exports */

    val thy_exports =
    {
      val node_theories =
        for {(name, status) <- use_theories_result.nodes if status.ok}
          yield {
            val snapshot = use_theories_result.snapshot(name)
            val provider = isabelle.Export.Provider.snapshot(snapshot)
            (name, Isabelle.read_theory(provider, name))
          }
      (Isabelle.pure_name, Isabelle.pure_theory) :: node_theories
    }


    /* documents */

    for ((thy_name, theory) <- thy_exports) {
      val doc = new Document(DPath(bt.base / theory.name), root = true)
      controller.add(doc)

      val thy = Isabelle.declared_theory(thy_name)
      controller.add(thy)
      controller.add(MRef(doc.path, thy.path))

      // theory source
      if (thy_name != Isabelle.pure_name) {
        val thy_text = isabelle.File.read(thy_name.path)
        val thy_text_output = isabelle.Symbol.decode(thy_text.replace(' ', '\u00a0'))
        controller.add(new OpaqueText(thy.asDocument.path, OpaqueText.defaultFormat, StringFragment(thy_text_output)))
      }

      // types
      for (c <- theory.types) {
        controller.add(Constant(thy.toTerm, Isabelle.entity_name(thy_name, c.entity), Nil, None, None, None))
      }

      // consts
      for (c <- theory.consts) {
        controller.add(Constant(thy.toTerm, Isabelle.entity_name(thy_name, c.entity), Nil, None, None, None))
      }

      // facts
      for (c <- theory.facts) {
        controller.add(Constant(thy.toTerm, Isabelle.entity_name(thy_name, c.entity), Nil, None, None, None))
      }

      index(doc)
    }

    BuildResult.empty
  }

  // FIXME demo
  def importDocumentExample(bt: BuildTask, index: Document => Unit) =
  {
    val Pure = "Pure"

    /** common namespace for all theories in all sessions in all Isabelle archives */
    val isaLibraryBase = DPath(URI("https", "isabelle.in.tum.de") / "Isabelle")

    /** namespace for MMT definitions of Isabelle built-in features (i.e., things not in the Isabelle library) */
    val logicBase = lf.LF._base
    val pure = logicBase ? Pure

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
      val path = pure ? isabelle.Pure_Thy.PROP
      def apply() = OMS(path)
    }

    object All {
      val path = pure ? isabelle.Pure_Thy.ALL
      def apply(name: String, tp: Term, body: Term) =
        lf.Apply(OMS(path), lf.Lambda(LocalName(name), tp, body))
    }

    object Imp {
      val path = pure ? isabelle.Pure_Thy.IMP
      def apply(left: Term, right: Term) = lf.ApplySpine(OMS(path), left, right)
    }

    object Eq {
      val path = pure ? isabelle.Pure_Thy.EQ
      def apply(tp: Term, left: Term, right: Term) = lf.ApplySpine(OMS(path), tp, left, right)
    }

    object Kind {
      val const = "const"
    }

    isabelle.Isabelle_System.init()

    val inText = File.read(bt.inFile)

    // a document corresponding to the source file
    val doc = new Document(bt.narrationDPath, root = true)
    controller add doc
    
    val session = ???
    
    // a theory corresponding to a toplevel declaration inside the source file
    val thy = new DeclaredTheory(isaLibraryBase, LocalName(session, "sometheory"), Some(pure), Theory.noParams, Theory.noBase)
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

class Isabelle(log: String => Unit)
{
  /* logging */

  val logger: isabelle.Logger =
    new isabelle.Logger { def apply(msg: => String): Unit = log(msg) }

  val progress =
    new isabelle.Progress {
      override def echo(msg: String): Unit = log(msg)
      override def theory(session: String, theory: String): Unit =
        log(isabelle.Progress.theory_message(session, theory))
    }


  /* options */

  lazy val options: isabelle.Options = isabelle.Options.init()

  lazy val store: isabelle.Sessions.Store = isabelle.Sessions.store(options)
  val cache: isabelle.Term.Cache = isabelle.Term.make_cache()


  /* names */

  /*common namespace for all theories in all sessions in all Isabelle archives*/
  val library_base: DPath = DPath(URI("https", "isabelle.in.tum.de") / "Isabelle")

  def declared_theory(node_name: isabelle.Document.Node.Name): DeclaredTheory =
  {
    val mod = library_base ? node_name.theory
    Theory.empty(mod.doc, mod.name, Some(mod))
  }

  def entity_name(node_name: isabelle.Document.Node.Name, entity: isabelle.Export_Theory.Entity): LocalName =
    LocalName(node_name.theory, entity.kind.toString, entity.name)

  def PURE: String = isabelle.Thy_Header.PURE


  /* session */

  private var _session: Option[isabelle.Thy_Resources.Session] = None

  def session: isabelle.Thy_Resources.Session =
    _session.getOrElse(isabelle.error("No Isabelle/PIDE session"))

  def resources: isabelle.Thy_Resources = session.resources

  def import_name(s: String): isabelle.Document.Node.Name =
    resources.import_name(isabelle.Sessions.DRAFT, "", s)

  def init()
  {
    isabelle.Isabelle_System.init()

    val build_results =
      isabelle.Build.build(options, progress = progress, sessions = List(PURE))
    if (!build_results.ok) isabelle.error("Failed to build Isabelle/Pure")

    val session_options =
      isabelle.Dump.make_options(options, isabelle.Dump.known_aspects)

    val session_deps =
      isabelle.Sessions.load_structure(session_options).
        selection_deps(isabelle.Sessions.Selection.all, progress = progress)

    val include_sessions = session_deps.sessions_structure.imports_topological_order

    _session =
      Some(isabelle.Thy_Resources.start_session(session_options, PURE,
        include_sessions = include_sessions, progress = progress, log = logger))
  }

  def exit()
  {
    session.stop()
    _session = None
  }


  /* theories */

  def pure_name: isabelle.Document.Node.Name = import_name(PURE)

  lazy val pure_theory: isabelle.Export_Theory.Theory =
    isabelle.Export_Theory.read_pure_theory(store, cache = Some(cache))

  def read_theory(provider: isabelle.Export.Provider, name: isabelle.Document.Node.Name)
    : isabelle.Export_Theory.Theory =
  {
    isabelle.Export_Theory.read_theory(
      provider, isabelle.Sessions.DRAFT, name.theory, cache = Some(cache))
  }

  def use_theories(theories: List[String]): isabelle.Thy_Resources.Theories_Result =
    session.use_theories(theories, progress = progress)
}
