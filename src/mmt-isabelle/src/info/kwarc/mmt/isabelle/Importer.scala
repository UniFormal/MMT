package info.kwarc.mmt.isabelle

import scala.collection.SortedMap
import scala.util.matching.Regex
import info.kwarc.mmt.lf
import info.kwarc.mmt.api._
import frontend.Controller
import archives.{Archive, NonTraversingImporter}
import info.kwarc.mmt.api.parser.{SourcePosition, SourceRef, SourceRegion}
import notations._
import symbols._
import modules._
import documents._
import objects._
import opaque._
import utils._

/**
 * An application that starts the Isabelle [[Importer]].
 * This is called by the shell-script mmt_import, via which it is available as a tool from within Isabelle.
 */
object Importer
{
  /** MMT names and archives **/

  /*common namespace for all theories in all sessions in all Isabelle archives*/
  val isabelle_base: DPath = DPath(URI("https", "isabelle.in.tum.de") / "Isabelle")

  def module_name(node_name: isabelle.Document.Node.Name): MPath = isabelle_base ? node_name.theory

  def declared_theory(
    node_name: isabelle.Document.Node.Name,
    meta_theory: Option[MPath]): DeclaredTheory =
  {
    val module = module_name(node_name)
    Theory.empty(module.doc, module.name, meta_theory)
  }

  class Indexed_Name(val name: String)
  {
    def apply(i: Int): String = name + "(" + i + ")"
    private val Pattern = (Regex.quote(name) + """\((\d+)\)""").r
    def unapply(s: String): Option[Int] =
      s match { case Pattern(isabelle.Value.Int(i)) => Some(i) case _ => None }
  }

  def init_archive(dir: isabelle.Path)
  {
    val meta_inf = dir + isabelle.Path.explode("META-INF/MANIFEST.MF")
    isabelle.Isabelle_System.mkdirs(meta_inf.dir)
    isabelle.File.write(meta_inf, "id: Isabelle\ntitle: Isabelle\n")
  }



  /** source with position: offset, line, column (counting 16-bit Char addresses from 0) **/

  object Source
  {
    def apply(text0: String): Source =
    {
      val text = isabelle.Symbol.decode(isabelle.Line.normalize(text0))
      val line_doc = isabelle.Line.Document(text)
      val text_chunk = isabelle.Symbol.Text_Chunk(text)
      new Source(line_doc, text_chunk)
    }

    val empty: Source = apply("")
  }

  final class Source private(line_doc: isabelle.Line.Document, text_chunk: isabelle.Symbol.Text_Chunk)
  {
    def is_empty: Boolean = line_doc.lines.isEmpty

    def text: String = line_doc.text
    override def toString: String = line_doc.text

    def position(offset: isabelle.Text.Offset): SourcePosition =
    {
      if (offset >= 0 && offset <= line_doc.text_length) {
        val line_pos = line_doc.position(offset)
        SourcePosition(offset, line_pos.line, line_pos.column)
      }
      else isabelle.error("Bad position offset " + offset)
    }

    def region(range: isabelle.Text.Range): SourceRegion =
    {
      if (!range.is_singularity) {
        SourceRegion(position(range.start), position(range.stop - 1))
      }
      else isabelle.error("Bad range singularity " + range)
    }

    def symbol_position(symbol_offset: isabelle.Symbol.Offset): SourcePosition =
      position(text_chunk.decode(symbol_offset))

    def symbol_region(symbol_range: isabelle.Symbol.Range): SourceRegion =
      region(text_chunk.decode(symbol_range))

    def ref(opt_uri: Option[URI], pos: isabelle.Position.T): Option[SourceRef] =
      for { uri <- opt_uri; range <- isabelle.Position.Range.unapply(pos) }
      yield {
        try { SourceRef(uri, symbol_region(range)) }
        catch { case isabelle.ERROR(msg) => isabelle.error(msg + "\nin " + uri) }
      }
  }



  /** Isabelle export structures **/

  sealed case class Theory_Export(
    node_name: isabelle.Document.Node.Name,
    node_source: Source,
    parents: List[String],
    segments: List[Theory_Segment])

  sealed case class Theory_Segment(
    element: isabelle.Thy_Element.Element_Command = isabelle.Thy_Element.atom(isabelle.Command.empty),
    classes: List[isabelle.Export_Theory.Class] = Nil,
    types: List[isabelle.Export_Theory.Type] = Nil,
    consts: List[isabelle.Export_Theory.Const] = Nil,
    facts: List[isabelle.Export_Theory.Fact_Multi] = Nil,
    locales: List[isabelle.Export_Theory.Locale] = Nil)
  {
    val header: String =
      element.head.span.content.iterator.takeWhile(tok => !tok.is_begin).map(_.source).mkString
    def header_relevant: Boolean =
      header.nonEmpty &&
        (classes.nonEmpty || types.nonEmpty || consts.nonEmpty || facts.nonEmpty || locales.nonEmpty)

    def facts_single: List[isabelle.Export_Theory.Fact_Single] =
      (for {
        decl_multi <- facts.iterator
        decl <- decl_multi.split.iterator
      } yield decl).toList
  }



  /** MMT import structures **/

  object Env
  {
    val empty: Env = new Env(Map.empty)
  }

  final class Env private(rep: Map[String, Term])
  {
    def get(x: String): Term = rep.getOrElse(x, OMV(x))
    def + (entry: (String, Term)): Env = new Env(rep + entry)
  }

  val dummy_type_scheme: (List[String], isabelle.Term.Typ) = (Nil, isabelle.Term.dummyT)

  object Item
  {
    object Key
    {
      object Ordering extends scala.math.Ordering[Key]
      {
        def compare(key1: Key, key2: Key): Int =
          key1.kind compare key2.kind match {
            case 0 => key1.name compare key2.name
            case ord => ord
          }
      }
    }

    sealed case class Key(kind: isabelle.Export_Theory.Kind.Value, name: String)
    {
      override def toString: String = kind.toString + " " + isabelle.quote(name)
    }
  }

  sealed case class Item(
    theory_source: Option[URI],
    theory_path: MPath,
    node_name: isabelle.Document.Node.Name,
    node_source: Source,
    entity: isabelle.Export_Theory.Entity,
    syntax: Option[isabelle.Export_Theory.Infix] = None,
    type_scheme: (List[String], isabelle.Term.Typ) = dummy_type_scheme)
  {
    val key: Item.Key = Item.Key(entity.kind, entity.name)

    def local_name: LocalName = LocalName(node_name.theory, entity.kind.toString, entity.name)
    def global_name: GlobalName = constant(None, None).path

    def notation: NotationContainer =
    {
      val notation = NotationContainer()
      val prefix_notation =
        new TextNotation(Prefix(Delim(entity.xname), 0, 0), Precedence.infinite, None)
      val infix_notation =
        syntax.map(infix =>
        {
          val assoc =
            infix.assoc match {
              case isabelle.Export_Theory.Assoc.NO_ASSOC => None
              case isabelle.Export_Theory.Assoc.LEFT_ASSOC => Some(true)
              case isabelle.Export_Theory.Assoc.RIGHT_ASSOC => Some(false)
            }
          val delim = Delim(isabelle.Symbol.decode(infix.delim))
          val fixity = Infix(delim, type_scheme._1.length, 2, assoc)
          new TextNotation(fixity, Precedence.integer(infix.pri), None)
        })

      if (infix_notation.isDefined) notation.presentationDim.set(infix_notation.get)
      notation.presentationDim.set(prefix_notation)

      notation
    }

    def constant(tp: Option[Term], df: Option[Term]): Constant =
    {
      val c = Constant(OMID(theory_path), local_name, Nil, tp, df, None, notation)
      for (sref <- node_source.ref(theory_source, entity.pos)) SourceRef.update(c, sref)
      c
    }

    def typargs(typ: isabelle.Term.Typ): List[isabelle.Term.Typ] =
    {
      var subst = Map.empty[String, isabelle.Term.Typ]
      def bad_match(): Nothing = isabelle.error("Bad type arguments for " + key + ": " + typ)
      def raw_match(arg: (isabelle.Term.Typ, isabelle.Term.Typ))
      {
        arg match {
          case (isabelle.Term.TFree(a, _), ty) =>
            subst.get(a) match {
              case None => subst += (a -> ty)
              case Some(ty1) => if (ty != ty1) bad_match()
            }
          case (isabelle.Term.Type(c1, args1), isabelle.Term.Type(c2, args2)) if c1 == c2 =>
            (args1 zip args2).foreach(raw_match)
          case _ => bad_match()
        }
      }

      raw_match(type_scheme._2, typ)
      type_scheme._1.map(subst(_))
    }
  }



  /** Isabelle to MMT importer **/

  val default_init_archive_dir: isabelle.Path = isabelle.Path.explode("isabelle_mmt")
  val default_logic: String = isabelle.Thy_Header.PURE

  def importer(options: isabelle.Options,
    logic: String = default_logic,
    dirs: List[isabelle.Path] = Nil,
    select_dirs: List[isabelle.Path] = Nil,
    selection: isabelle.Sessions.Selection = isabelle.Sessions.Selection.empty,
    archive_dirs: List[isabelle.Path],
    init_archive_dir: isabelle.Path,
    chapter_archive: String => Option[String],
    progress: isabelle.Progress = isabelle.No_Progress)
  {
    val controller = new Controller

    object MMT_Importer extends NonTraversingImporter { val key = "isabelle-omdoc" }
    controller.extman.addExtension(MMT_Importer, Nil)

    val archives: List[Archive] =
      (init_archive_dir :: archive_dirs).flatMap(dir =>
          controller.backend.openArchive(dir.absolute_file))
      match {
        case Nil =>
          init_archive(init_archive_dir)
          controller.backend.openArchive(init_archive_dir.absolute_file) match {
            case Nil => isabelle.error("Failed to initialize archive in " + init_archive_dir)
            case archives => archives
          }
        case archives => archives
      }

    archives.foreach(archive => progress.echo("Adding " + archive))

    object Isabelle extends
      Isabelle(options, progress, logic, dirs, select_dirs, selection, archives, chapter_archive)

    def import_theory(thy_export: Theory_Export)
    {
      progress.echo("Importing theory " + thy_export.node_name + " ...")

      val thy_name = thy_export.node_name
      val thy_base_name = isabelle.Long_Name.base_name(thy_export.node_name.theory)
      val thy_is_pure: Boolean = thy_name == Isabelle.pure_name

      // archive
      val (archive, thy_source_path, thy_doc_path) = Isabelle.theory_archive(thy_name)

      // document
      val doc = new Document(thy_doc_path, root = true)
      controller.add(doc)

      // theory content
      val thy_content =
        Isabelle.begin_theory(thy_export,
          if (thy_is_pure) None else Some(URI(thy_source_path.file.toPath.toUri)),
          if (thy_is_pure) None else Some(Isabelle.pure_path))

      controller.add(thy_content.thy)
      controller.add(MRef(doc.path, thy_content.thy.path))

      if (thy_is_pure) {
        controller.add(Include(thy_content.thy.toTerm, lf.PLF._path, Nil))
      }

      // PIDE theory source
      if (!thy_export.node_source.is_empty) {
        isabelle.Isabelle_System.mkdirs(thy_source_path.dir)
        isabelle.File.write(thy_source_path, thy_export.node_source.text)
      }

      def decl_error(entity: isabelle.Export_Theory.Entity)(body: => Unit)
      {
        try { body }
        catch {
          case isabelle.ERROR(msg) =>
            isabelle.error(msg + "\nin declaration of " + entity + isabelle.Position.here(entity.pos))
        }
      }

      for (segment <- thy_export.segments) {
        // source text
        if (segment.header_relevant) {
          val text = isabelle.Symbol.decode(segment.header.replace(' ', '\u00a0'))
          val opaque =
            new OpaqueText(thy_content.thy.asDocument.path,
              OpaqueText.defaultFormat, StringFragment(text))
          controller.add(opaque)
        }

        // classes
        for (decl <- segment.classes) {
          decl_error(decl.entity) {
            val item = thy_content.declare_item(decl.entity)
            val tp = Isabelle.Class()
            controller.add(item.constant(Some(tp), None))
          }
        }

        // types
        for (decl <- segment.types) {
          decl_error(decl.entity) {
            val item = thy_content.declare_item(decl.entity, decl.syntax)
            val tp = Isabelle.Type(decl.args.length)
            val df = decl.abbrev.map(rhs => Isabelle.Type.abs(decl.args, thy_content.value.import_type(rhs)))
            controller.add(item.constant(Some(tp), df))
          }
        }

        // consts
        for (decl <- segment.consts) {
          decl_error(decl.entity) {
            val item = thy_content.declare_item(decl.entity, decl.syntax, (decl.typargs, decl.typ))
            val tp = Isabelle.Type.all(decl.typargs, thy_content.value.import_type(decl.typ))
            val df = decl.abbrev.map(rhs => Isabelle.Type.abs(decl.typargs, thy_content.value.import_term(rhs)))
            controller.add(item.constant(Some(tp), df))
          }
        }

        // facts
        for (decl <- segment.facts_single) {
          decl_error(decl.entity) {
            val item = thy_content.declare_item(decl.entity)
            val tp = thy_content.value.import_prop(decl.prop)
            controller.add(item.constant(Some(tp), None))
          }
        }

        // locales
        for (locale <- segment.locales) {
          decl_error(locale.entity) {
            val content = thy_content.value
            val item = thy_content.declare_item(locale.entity)
            val loc_name = item.local_name
            val loc_thy = Theory.empty((thy_content.thy.path / loc_name).toDPath, loc_name, None)

            // type parameters
            val type_env =
              (Env.empty /: locale.typargs) {
                case (env, (a, _)) =>
                  val c = Constant(loc_thy.toTerm, LocalName(a), Nil, Some(Isabelle.Type()), None, None)
                  loc_thy.add(c)
                  env + (a -> c.toTerm)
              }

            // sort constraints
            for { (prop, i) <- content.import_sorts(locale.typargs).zipWithIndex } {
              val name = LocalName(Isabelle.Locale.Sorts(i + 1))
              loc_thy.add(Constant(loc_thy.toTerm, name, Nil, Some(prop), None, None))
            }

            // term parameters
            val term_env =
              (type_env /: locale.args) {
                case (env, (x, ty)) =>
                  val tp = content.import_type(ty, type_env)
                  val c = Constant(loc_thy.toTerm, LocalName(x), Nil, Some(tp), None, None)
                  loc_thy.add(c)
                  env + (x -> c.toTerm)
              }

            // logical axioms
            for { (prop, i) <- locale.axioms.map(content.import_prop(_, term_env)).zipWithIndex } {
              val name = LocalName(Isabelle.Locale.Axioms(i + 1))
              loc_thy.add(Constant(loc_thy.toTerm, name, Nil, Some(prop), None, None))
            }

            controller.add(new NestedModule(thy_content.thy.toTerm, loc_name, loc_thy))
          }
        }
      }

      thy_content.end_theory()

      MMT_Importer.importDocument(archive, doc)
    }

    try { Isabelle.import_session(import_theory) }
    finally { Isabelle.session.stop() }

    progress.echo("Finished import of " + Isabelle.report_imported)
  }


  /* command line entry point */

  def main(args: Array[String])
  {
    isabelle.Command_Line.tool0 {
      var archive_dirs: List[isabelle.Path] = Nil
      var chapter_archive_map: Map[String, String] = Map.empty
      var chapter_archive_default = ""
      var base_sessions: List[String] = Nil
      var select_dirs: List[isabelle.Path] = Nil
      var init_archive_dir = default_init_archive_dir
      var requirements = false
      var exclude_session_groups: List[String] = Nil
      var all_sessions = false
      var dirs: List[isabelle.Path] = Nil
      var session_groups: List[String] = Nil
      var logic = default_logic
      var options = isabelle.Dump.make_options(isabelle.Options.init(), isabelle.Dump.known_aspects)
      var verbose = false
      var exclude_sessions: List[String] = Nil

      val getopts = isabelle.Getopts("""
Usage: isabelle mmt_import [OPTIONS] [SESSIONS ...]

  Options are:
    -A DIR       add archive directory
    -C CH=AR     add mapping of chapter CH to archive AR, or default "_=AR"
    -B NAME      include session NAME and all descendants
    -D DIR       include session directory and select its sessions
    -I DIR       init archive directory (default: """ + default_init_archive_dir + """)
    -R           operate on requirements of selected sessions
    -X NAME      exclude sessions from group NAME and all descendants
    -a           select all sessions
    -d DIR       include session directory
    -g NAME      select session group NAME
    -l NAME      logic session name (default: """ + isabelle.quote(default_logic) + """)
    -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)
    -v           verbose mode
    -x NAME      exclude session NAME and all descendants

  Import specified sessions into MMT output directory.
""",
      "A:" -> (arg => archive_dirs = archive_dirs ::: List(isabelle.Path.explode(arg))),
      "C:" -> (arg =>
        isabelle.space_explode('=', arg) match {
          case ch :: ar =>
            val archive = ar.mkString("=")
            if (ch == "_") chapter_archive_default = archive
            else chapter_archive_map += (ch -> archive)
          case Nil => isabelle.error("Malformed chapter to archive mapping")
        }),
      "B:" -> (arg => base_sessions = base_sessions ::: List(arg)),
      "D:" -> (arg => { select_dirs = select_dirs ::: List(isabelle.Path.explode(arg)) }),
      "I:" -> (arg => { init_archive_dir = isabelle.Path.explode(arg) }),
      "R" -> (_ => requirements = true),
      "X:" -> (arg => exclude_session_groups = exclude_session_groups ::: List(arg)),
      "a" -> (_ => all_sessions = true),
      "d:" -> (arg => { dirs = dirs ::: List(isabelle.Path.explode(arg)) }),
      "g:" -> (arg => session_groups = session_groups ::: List(arg)),
      "l:" -> (arg => logic = arg),
      "o:" -> (arg => { options += arg }),
      "v" -> (_ => verbose = true),
      "x:" -> (arg => exclude_sessions = exclude_sessions ::: List(arg)))

      val sessions = getopts(args)

      val selection =
        isabelle.Sessions.Selection(
          requirements = requirements,
          all_sessions = all_sessions,
          base_sessions = base_sessions,
          exclude_session_groups = exclude_session_groups,
          exclude_sessions = exclude_sessions,
          session_groups = session_groups,
          sessions = sessions)

      val progress =
        new isabelle.Console_Progress(verbose = verbose) {
          override def theory(theory: isabelle.Progress.Theory): Unit =
            if (verbose) echo("Processing " + theory.print_theory + theory.print_percentage)
        }

      val start_date = isabelle.Date.now()
      if (verbose) progress.echo("Started at " + isabelle.Build_Log.print_date(start_date) + "\n")

      try {
        importer(options,
          logic = logic,
          dirs = dirs,
          select_dirs = select_dirs,
          selection = selection,
          archive_dirs = archive_dirs,
          init_archive_dir = init_archive_dir,
          chapter_archive =
            (ch: String) => chapter_archive_map.get(ch) orElse
              isabelle.proper_string(chapter_archive_default),
          progress = progress)
      }
      finally {
        val end_date = isabelle.Date.now()
        if (verbose) progress.echo("\nFinished at " + isabelle.Build_Log.print_date(end_date))
        progress.echo((end_date.time - start_date.time).message_hms + " elapsed time")
      }
    }
  }



  /** Isabelle session with imported content **/

  class Isabelle(
    options: isabelle.Options,
    progress: isabelle.Progress,
    logic: String,
    dirs: List[isabelle.Path],
    select_dirs: List[isabelle.Path],
    selection: isabelle.Sessions.Selection,
    archives: List[Archive],
    chapter_archive: String => Option[String])
  {
    val store: isabelle.Sessions.Store = isabelle.Sessions.store(options)
    val cache: isabelle.Term.Cache = isabelle.Term.make_cache()


    /* dependencies */

    private val session_deps: isabelle.Sessions.Deps =
    {
      val sessions_structure0 =
        isabelle.Sessions.load_structure(options, dirs = dirs, select_dirs = select_dirs)

      val selection1 =
      {
        val sessions_structure = sessions_structure0.selection(selection)

        val default_record_proofs = options.int("record_proofs")
        val sessions_record_proofs =
          for {
            name <- sessions_structure.build_topological_order
            info <- sessions_structure.get(name)
            if info.options.int("record_proofs") > default_record_proofs
          } yield name

        val excluded =
          for (name <- sessions_structure.build_descendants(sessions_record_proofs))
            yield {
              progress.echo_warning("Skipping session " + name + "  (option record_proofs)")
              name
            }

        selection.copy(exclude_sessions = excluded ::: selection.exclude_sessions)
      }

      val selection_size = sessions_structure0.selection(selection1).build_graph.size
      if (selection_size > 1) progress.echo("Loading " + selection_size + " sessions ...")

      sessions_structure0.selection_deps(selection1, progress = progress)
    }


    /* session */

    val session: isabelle.Headless.Session =
    {
      val build_rc =
        isabelle.Build.build_logic(options, logic, build_heap = true, progress = progress,
          dirs = dirs ::: select_dirs)
      if (build_rc != 0) isabelle.error("Failed to build Isabelle/" + logic)

      isabelle.Headless.start_session(options, logic,
        session_dirs = dirs ::: select_dirs,
        include_sessions = session_deps.sessions_structure.imports_topological_order,
        progress = progress)
    }


    /* resources */

    def resources: isabelle.Headless.Resources = session.resources

    def import_name(s: String): isabelle.Document.Node.Name =
      resources.import_name(isabelle.Sessions.DRAFT, "", s)

    def theory_qualifier(name: isabelle.Document.Node.Name): String =
      resources.session_base.theory_qualifier(name)

    def theory_archive(name: isabelle.Document.Node.Name): (Archive, isabelle.Path, DPath) =
    {
      def err(msg: String): Nothing =
        isabelle.error(msg + " (theory " + isabelle.quote(name.toString) + ")")

      val session_name = theory_qualifier(name)
      val session_info =
        session_deps.sessions_structure.get(session_name) getOrElse
          err("Undefined session info " + isabelle.quote(session_name))

      val chapter = session_info.chapter

      val archive =
        archives match {
          case List(archive) => archive
          case _ =>
            val archive_name =
              chapter_archive(chapter) getOrElse
                err("Unspecified archive for chapter " + isabelle.quote(chapter))

            archives.find(archive => archive.root.getName == archive_name) getOrElse
              err("Unknown archive for chapter " + isabelle.quote(chapter))
        }
      val subdir = if (archive.root.getName == chapter) None else Some(chapter)

      val source_path =
        isabelle.File.path(archive.root.toJava) +
          isabelle.Path.basic("source") +
          isabelle.Path.explode(subdir.getOrElse("")) +
          isabelle.Path.basic(session_name) +
          isabelle.Path.basic(name.theory).ext("theory")

      val doc_path =
        DPath(archive.narrationBase / subdir.toList / session_name / name.theory_base_name)

      (archive, source_path, doc_path)
    }


    /* import session theories */

    private val import_theories = session_deps.used_theories_condition(progress.echo_warning)
    import_theories.foreach(theory_archive)

    def import_session(import_theory: Theory_Export => Unit)
    {
      if (import_theories.isEmpty) {
        progress.echo_warning("Nothing to import")
      }
      else {
        import_theory(pure_theory_export)

        object Consumer
        {
          sealed case class Bad_Theory(
            name: isabelle.Document.Node.Name,
            status: isabelle.Document_Status.Node_Status,
            errors: List[String])

          private val consumer_bad_theories = isabelle.Synchronized(List.empty[Bad_Theory])

          private val consumer =
            isabelle.Consumer_Thread.fork(name = "mmt_import")(
              consume = (args: (isabelle.Document.Snapshot, isabelle.Document_Status.Node_Status)) =>
              {
                val (snapshot, status) = args
                val name = snapshot.node_name
                if (status.ok) {
                  try { import_theory(read_theory_export(snapshot)) }
                  catch {
                    case exn: Throwable if !isabelle.Exn.is_interrupt(exn) =>
                      val msg = isabelle.Exn.message(exn)
                      progress.echo("FAILED to import theory " + name)
                      progress.echo_error_message(msg)
                      consumer_bad_theories.change(Bad_Theory(name, status, List(msg)) :: _)
                  }
                }
                else {
                  val msgs =
                    for ((tree, pos) <- snapshot.messages if isabelle.Protocol.is_error(tree))
                    yield {
                      "Error" + isabelle.Position.here(pos) + ":\n" +
                        isabelle.XML.content(isabelle.Pretty.formatted(List(tree)))
                    }
                  progress.echo("FAILED to process theory " + name)
                  msgs.foreach(progress.echo_error_message)
                  consumer_bad_theories.change(Bad_Theory(name, status, msgs) :: _)
                }
                true
              })

          def apply(
              snapshot: isabelle.Document.Snapshot,
              node_status: isabelle.Document_Status.Node_Status): Unit =
            consumer.send((snapshot, node_status))

          def shutdown(): List[Bad_Theory] =
          {
            consumer.shutdown()
            consumer_bad_theories.value.reverse
          }
        }

        session.use_theories(
          import_theories.map(_.theory),
          check_delay = options.seconds("mmt_check_delay"),
          commit = Some(Consumer.apply _),
          commit_cleanup_delay = options.seconds("mmt_cleanup_delay"),
          watchdog_timeout = options.seconds("mmt_watchdog_timeout"),
          progress = progress)

        val bad_theories = Consumer.shutdown()
        val bad_msgs =
          for { bad <- bad_theories }
          yield {
            val msg =
              "FAILED theory " + bad.name +
                (if (bad.status.consolidated) "" else ": " + bad.status.percentage + "% finished") +
                (if (bad.errors.isEmpty) "" else bad.errors.mkString("\n", "\n", ""))
            isabelle.Output.clean_yxml(msg)
          }
        if (bad_msgs.nonEmpty) isabelle.error(bad_msgs.mkString("\n\n"))
      }
    }


    /* Pure theory */

    def PURE: String = isabelle.Thy_Header.PURE
    def pure_name: isabelle.Document.Node.Name = import_name(PURE)

    lazy val pure_path: MPath = declared_theory(pure_name, None).path

    lazy val pure_theory: isabelle.Export_Theory.Theory =
      isabelle.Export_Theory.read_pure_theory(store, cache = Some(cache))

    def pure_theory_export: Theory_Export =
    {
      val segment =
        Theory_Segment(
          classes = pure_theory.classes,
          types =
            for {
              decl <- pure_theory.types
              if decl.entity.name != isabelle.Pure_Thy.DUMMY &&
                decl.entity.name != isabelle.Pure_Thy.FUN
            } yield decl,
          consts = pure_theory.consts,
          facts = pure_theory.facts,
          locales = pure_theory.locales)
      Theory_Export(pure_name, Source.empty, Nil, List(segment))
    }

    private def pure_entity(entities: List[isabelle.Export_Theory.Entity], name: String): GlobalName =
      entities.collectFirst(
        { case entity if entity.name == name => Item(None, pure_path, pure_name, Source.empty, entity).global_name
        }).getOrElse(isabelle.error("Unknown entity " + isabelle.quote(name)))

    def pure_type(name: String): GlobalName = pure_entity(pure_theory.types.map(_.entity), name)
    def pure_const(name: String): GlobalName = pure_entity(pure_theory.consts.map(_.entity), name)

    object Type
    {
      def apply(n: Int = 0): Term =
      {
        val t = OMS(lf.Typed.ktype)
        if (n == 0) t else lf.Arrow(isabelle.Library.replicate(n, t), t)
      }

      def all(as: List[String], t: Term): Term =
        if (as.isEmpty) t else lf.Pi(as.map(a => OMV(a) % Type()), t)

      def abs(as: List[String], t: Term): Term =
        if (as.isEmpty) t else lf.Lambda(as.map(a => OMV(a) % Type()), t)

      def app(t: Term, tps: List[Term]): Term =
        if (tps.isEmpty) t else OMA(lf.Apply.term, t :: tps)
    }

    object Prop
    {
      lazy val path: GlobalName = pure_type(isabelle.Pure_Thy.PROP)
      def apply(): Term = OMS(path)
    }

    object Class
    {
      def apply(): Term = lf.Arrow(Type(), Prop())
    }

    object All
    {
      lazy val path: GlobalName = pure_const(isabelle.Pure_Thy.ALL)
      def apply(x: String, tp: Term, body: Term): Term =
        lf.Apply(OMS(path), lf.Lambda(LocalName(x), tp, body))
    }

    object Imp
    {
      lazy val path: GlobalName = pure_const(isabelle.Pure_Thy.IMP)
      def apply(t: Term, u: Term): Term = lf.ApplySpine(OMS(path), t, u)
    }

    object Eq
    {
      lazy val path: GlobalName = pure_const(isabelle.Pure_Thy.EQ)
      def apply(tp: Term, t: Term, u: Term): Term = lf.ApplySpine(OMS(path), tp, t, u)
    }

    object Locale
    {
      object Sorts extends Indexed_Name("sorts")
      object Axioms extends Indexed_Name("axioms")
    }


    /* user theories */

    def read_theory_export(snapshot: isabelle.Document.Snapshot): Theory_Export =
    {
      val node_name = snapshot.node_name
      val theory_name = node_name.theory

      val theory =
        isabelle.Export_Theory.read_theory(isabelle.Export.Provider.snapshot(snapshot),
          isabelle.Sessions.DRAFT, theory_name, cache = Some(cache))

      val syntax = resources.session_base.node_syntax(snapshot.version.nodes, node_name)

      val segments =
      {
        val relevant_elements =
          isabelle.Thy_Element.parse_elements(syntax.keywords, snapshot.node.commands.toList).
            filter(elem => elem.head.span.is_kind(syntax.keywords, isabelle.Keyword.theory, false))
        val relevant_ids =
          (for { element <- relevant_elements.iterator; cmd <- element.outline_iterator }
            yield cmd.id).toSet

        val node_command_ids = snapshot.command_id_map

        for (element <- relevant_elements)
        yield {
          def defined(entity: isabelle.Export_Theory.Entity): Boolean =
          {
            def for_entity: String =
              " for " + entity + " in theory " + isabelle.quote(theory_name)

            val entity_id =
              entity.id match {
                case Some(id) => id
                case None => isabelle.error("Missing command id" + for_entity)
              }
            val entity_command =
              node_command_ids.get(entity_id) match {
                case Some(cmd) if relevant_ids(cmd.id) => cmd
                case _ =>
                  val msg = "No command with suitable id" + for_entity
                  snapshot.state.lookup_id(entity_id) match {
                    case None => isabelle.error(msg)
                    case Some(st) =>
                      isabelle.error(msg + " -- it refers to command " +
                        isabelle.Symbol.cartouche_decoded(st.command.source) + " in " +
                        isabelle.quote(st.command.node_name.node))
                  }
              }
            element.outline_iterator.exists(cmd => cmd.id == entity_command.id)
          }
          Theory_Segment(
            element = element,
            classes = for (decl <- theory.classes if defined(decl.entity)) yield decl,
            types = for (decl <- theory.types if defined(decl.entity)) yield decl,
            consts = for (decl <- theory.consts if defined(decl.entity)) yield decl,
            facts = for (decl <- theory.facts if defined(decl.entity)) yield decl,
            locales = for (decl <- theory.locales if defined(decl.entity)) yield decl)
        }
      }
      Theory_Export(node_name, Source(snapshot.node.source), theory.parents, segments)
    }

    def use_theories(theories: List[String]): isabelle.Headless.Use_Theories_Result =
      session.use_theories(theories, progress = progress)


    /* theory content */

    object Content
    {
      val empty: Content = new Content(SortedMap.empty[Item.Key, Item](Item.Key.Ordering))
      def merge(args: TraversableOnce[Content]): Content = (empty /: args)(_ ++ _)
    }

    final class Content private(private val rep: SortedMap[Item.Key, Item])
    {
      content =>

      def size: Int = rep.size

      def report_kind(kind: isabelle.Export_Theory.Kind.Value): String =
        rep.count({ case (_, item) => item.entity.kind == kind }).toString + " " + kind.toString

      def report: String =
        isabelle.commas(
          List(
            isabelle.Export_Theory.Kind.LOCALE,
            isabelle.Export_Theory.Kind.CLASS,
            isabelle.Export_Theory.Kind.TYPE,
            isabelle.Export_Theory.Kind.CONST,
            isabelle.Export_Theory.Kind.FACT).map(report_kind))

      def get(key: Item.Key): Item = rep.getOrElse(key, isabelle.error("Undeclared " + key.toString))
      def get_class(name: String): Item = get(Item.Key(isabelle.Export_Theory.Kind.CLASS, name))
      def get_type(name: String): Item = get(Item.Key(isabelle.Export_Theory.Kind.TYPE, name))
      def get_const(name: String): Item = get(Item.Key(isabelle.Export_Theory.Kind.CONST, name))
      def get_fact(name: String): Item = get(Item.Key(isabelle.Export_Theory.Kind.FACT, name))
      def get_locale(name: String): Item = get(Item.Key(isabelle.Export_Theory.Kind.LOCALE, name))

      def is_empty: Boolean = rep.isEmpty
      def defined(key: Item.Key): Boolean = rep.isDefinedAt(key)

      def declare(item: Item): Content =
      {
        if (defined(item.key)) {
          isabelle.error("Duplicate " + item.key.toString + " in theory " +
            isabelle.quote(item.node_name.theory))
        }
        else content + item
      }

      def + (item: Item): Content =
        if (defined(item.key)) content
        else new Content(rep + (item.key -> item))

      def ++ (other: Content): Content =
        if (content eq other) content
        else if (is_empty) other
        else (content /: other.rep)({ case (map, (_, item)) => map + item })

      override def toString: String =
        rep.iterator.map(_._2).mkString("Content(", ", ", ")")


      /* MMT import of Isabelle classes, types, terms etc. */

      def import_class(name: String): Term = OMS(get_class(name).global_name)

      def import_type(ty: isabelle.Term.Typ, env: Env = Env.empty): Term =
      {
        try {
          ty match {
            case isabelle.Term.Type(isabelle.Pure_Thy.FUN, List(a, b)) =>
              lf.Arrow(import_type(a, env), import_type(b, env))
            case isabelle.Term.Type(name, args) =>
              val op = OMS(get_type(name).global_name)
              if (args.isEmpty) op else OMA(lf.Apply.term, op :: args.map(content.import_type(_, env)))
            case isabelle.Term.TFree(a, _) => env.get(a)
            case isabelle.Term.TVar(xi, _) =>
              isabelle.error("Illegal schematic type variable " + xi.toString)
          }
        }
        catch { case isabelle.ERROR(msg) => isabelle.error(msg + "\nin type " + ty) }
      }

      def import_term(tm: isabelle.Term.Term, env: Env = Env.empty): Term =
      {
        def term(bounds: List[String], t: isabelle.Term.Term): Term =
          t match {
            case isabelle.Term.Const(c, ty) =>
              val item = get_const(c)
              Type.app(OMS(item.global_name), item.typargs(ty).map(content.import_type(_, env)))
            case isabelle.Term.Free(x, _) => env.get(x)
            case isabelle.Term.Var(xi, _) =>
              isabelle.error("Illegal schematic variable " + xi.toString)
            case isabelle.Term.Bound(i) =>
              val x =
                try { bounds(i) }
                catch {
                  case _: IndexOutOfBoundsException =>
                    isabelle.error("Loose de-Bruijn index " + i)
                }
              OMV(x)
            case isabelle.Term.Abs(x, ty, b) =>
              lf.Lambda(LocalName(x), import_type(ty, env), term(x :: bounds, b))
            case isabelle.Term.App(a, b) =>
              lf.Apply(term(bounds, a), term(bounds, b))
          }

        try { term(Nil, tm) }
        catch { case isabelle.ERROR(msg) => isabelle.error(msg + "\nin term " + tm) }
      }

      def import_sorts(typargs: List[(String, isabelle.Term.Sort)]): List[Term] =
        typargs.flatMap({ case (a, s) => s.map(c => lf.Apply(import_class(c), OMV(a))) })

      def import_prop(prop: isabelle.Export_Theory.Prop, env: Env = Env.empty): Term =
      {
        val types = prop.typargs.map(_._1)
        val sorts = import_sorts(prop.typargs)
        val vars = prop.args.map({ case (x, ty) => OMV(x) % import_type(ty, env) })
        val t = import_term(prop.term, env)
        Type.all(types, lf.Arrow(sorts, if (vars.isEmpty) t else lf.Pi(vars, t)))
      }
    }


    /* management of imported content */

    private val imported = isabelle.Synchronized(Map.empty[String, Content])

    def report_imported: String =
    {
      val theories = imported.value
      val items = Content.merge(theories.valuesIterator)
      theories.size.toString + " theories with " + items.size + " items: " + items.report
    }

    def theory_content(name: String): Content =
      imported.value.getOrElse(name, isabelle.error("Unknown theory " + isabelle.quote(name)))

    def begin_theory(
      thy_export: Theory_Export,
      thy_source: Option[URI],
      meta_theory: Option[MPath]): Theory_Content_Var =
    {
      val thy = declared_theory(thy_export.node_name, meta_theory)
      for (uri <- thy_source) SourceRef.update(thy, SourceRef(uri, SourceRegion.none))

      val parent_content = Content.merge(thy_export.parents.map(theory_content))
      new Theory_Content_Var(thy_source, thy, thy_export.node_name, thy_export.node_source, parent_content)
    }

    class Theory_Content_Var private[Isabelle](
      thy_source: Option[URI],
      val thy: DeclaredTheory,
      val node_name: isabelle.Document.Node.Name,
      node_source: Source,
      val parent_content: Content)
    {
      private val content = isabelle.Synchronized(parent_content)
      def value: Content = content.value

      def declare_item(
        entity: isabelle.Export_Theory.Entity,
        syntax: Option[isabelle.Export_Theory.Infix] = None,
        type_scheme: (List[String], isabelle.Term.Typ) = dummy_type_scheme): Item =
      {
        val item = Item(thy_source, thy.path, node_name, node_source, entity, syntax, type_scheme)
        content.change(_.declare(item))
        item
      }

      def end_theory(): Unit = imported.change(map => map + (node_name.theory -> value))
    }
  }
}
