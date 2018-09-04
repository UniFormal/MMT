package info.kwarc.mmt.isabelle

import scala.math.Ordering
import scala.collection.SortedMap
import info.kwarc.mmt.lf
import info.kwarc.mmt.api._
import frontend.Controller
import archives._
import symbols._
import modules._
import documents._
import objects._
import opaque._
import utils._


object Importer
{
  /* errors */

  class Isabelle_Error(msg: String) extends Error("Isabelle error: " + msg)


  /* names */

  /*common namespace for all theories in all sessions in all Isabelle archives*/
  val library_base: DPath = DPath(URI("https", "isabelle.in.tum.de") / "Isabelle")

  def declared_theory(node_name: isabelle.Document.Node.Name): DeclaredTheory =
  {
    val mod = library_base ? node_name.theory
    Theory.empty(mod.doc, mod.name, Some(mod))
  }


  /* formal items */

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

  val dummy_type_scheme: (List[String], isabelle.Term.Typ) = (Nil, isabelle.Term.dummyT)

  sealed case class Item(
    node_name: isabelle.Document.Node.Name,
    entity: isabelle.Export_Theory.Entity,
    type_scheme: (List[String], isabelle.Term.Typ) = dummy_type_scheme)
  {
    val key: Item.Key = Item.Key(entity.kind, entity.name)

    def local_name: LocalName = LocalName(node_name.theory, entity.kind.toString, entity.name)
    def global_name: GlobalName = constant(None, None).path

    def constant(tp: Option[Term], df: Option[Term]): Constant =
      Constant(declared_theory(node_name).toTerm, local_name, Nil, tp, df, None)

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
            (args1 zip args2).foreach(raw_match(_))
          case _ => bad_match()
        }
      }

      raw_match(type_scheme._2, typ)
      type_scheme._1.map(subst(_))
    }
  }

  object Items
  {
    val empty: Items = new Items(SortedMap.empty[Item.Key, Item](Item.Key.Ordering))
    def merge(args: TraversableOnce[Items]): Items = (empty /: args)(_ ++ _)
  }

  final class Items private(private val rep: SortedMap[Item.Key, Item])
  {
    def get(key: Item.Key): Item = rep.getOrElse(key, isabelle.error("Undeclared " + key.toString))
    def get_class(name: String): Item = get(Item.Key(isabelle.Export_Theory.Kind.CLASS, name))
    def get_type(name: String): Item = get(Item.Key(isabelle.Export_Theory.Kind.TYPE, name))
    def get_const(name: String): Item = get(Item.Key(isabelle.Export_Theory.Kind.CONST, name))
    def get_locale(name: String): Item = get(Item.Key(isabelle.Export_Theory.Kind.LOCALE, name))

    def is_empty: Boolean = rep.isEmpty
    def defined(key: Item.Key): Boolean = rep.isDefinedAt(key)

    def declare(item: Item): Items =
    {
      if (defined(item.key)) {
        isabelle.error("Duplicate " + item.key.toString + " in theory " +
          isabelle.quote(item.node_name.theory))
      }
      else this + item
    }

    def + (item: Item): Items =
      if (defined(item.key)) this
      else new Items(rep + (item.key -> item))

    def ++ (other: Items): Items =
      if (this eq other) this
      else if (is_empty) other
      else (this /: other.rep)({ case (map, (_, item)) => map + item })

    override def toString: String =
      rep.iterator.map(_._2).mkString("Items(", ", ", ")")
  }


  /* theory export */

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

  sealed case class Theory_Export(
    node_name: isabelle.Document.Node.Name,
    parents: List[String],
    segments: List[Theory_Segment])



  /** Isabelle session specification: command-line arguments vs. JSON **/

  object Arguments
  {
    val extension: String = "isabelle"
    val source_file: isabelle.Path = isabelle.Path.explode("source/arguments").ext(extension)

    val default_output_dir: String = "isabelle_mmt"
    val default_logic: String = isabelle.Thy_Header.PURE

    def command_line(args: List[String]): Arguments =
    {
      var base_sessions: List[String] = Nil
      var select_dirs: List[String] = Nil
      var output_dir = default_output_dir
      var requirements = false
      var exclude_session_groups: List[String] = Nil
      var all_sessions = false
      var dirs: List[String] = Nil
      var session_groups: List[String] = Nil
      var logic = default_logic
      var options: List[String] = Nil
      var verbose = false
      var exclude_sessions: List[String] = Nil

      val options0 = isabelle.Options.init()

      val getopts = isabelle.Getopts("""
Usage: isabelle mmt_import [OPTIONS] [SESSIONS ...]

  Options are:
    -B NAME      include session NAME and all descendants
    -D DIR       include session directory and select its sessions
    -O DIR       output directory for MMT (default: """ + isabelle.quote(default_output_dir) + """)
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
        "B:" -> (arg => base_sessions = base_sessions ::: List(arg)),
        "D:" -> (arg => { isabelle.Path.explode(arg); select_dirs = select_dirs ::: List(arg) }),
        "O:" -> (arg => { isabelle.Path.explode(arg); output_dir = arg }),
        "R" -> (_ => requirements = true),
        "X:" -> (arg => exclude_session_groups = exclude_session_groups ::: List(arg)),
        "a" -> (_ => all_sessions = true),
        "d:" -> (arg => { isabelle.Path.explode(arg); dirs = dirs ::: List(arg) }),
        "g:" -> (arg => session_groups = session_groups ::: List(arg)),
        "l:" -> (arg => logic = arg),
        "o:" -> (arg => { options0 + arg; options = options ::: List(arg) }),
        "v" -> (_ => verbose = true),
        "x:" -> (arg => exclude_sessions = exclude_sessions ::: List(arg)))

      val sessions = getopts(args)

      Arguments(base_sessions = base_sessions,
        select_dirs = select_dirs,
        output_dir = output_dir,
        requirements = requirements,
        exclude_session_groups = exclude_session_groups,
        all_sessions = all_sessions,
        dirs = dirs,
        session_groups = session_groups,
        logic = logic,
        options = options,
        verbose = verbose,
        exclude_sessions = exclude_sessions,
        sessions = sessions)
    }

    def read_json(path: isabelle.Path): Arguments =
    {
      try { json(isabelle.JSON.parse(isabelle.File.read(path))) }
      catch { case isabelle.ERROR(msg) => isabelle.error(msg + "\nin file " + path) }
    }

    def json(json: isabelle.JSON.T): Arguments =
    {
      def err: Nothing = isabelle.error("Bad JSON arguments: " + json)

      json match {
        case isabelle.JSON.Object(obj) if obj.keySet.subsetOf(domain) =>
          (for {
            base_sessions <- isabelle.JSON.strings_default(obj, "base_sessions")
            select_dirs <- isabelle.JSON.strings_default(obj, "select_dirs")
            output_dir <- isabelle.JSON.string_default(obj, "output_dir", default_output_dir)
            requirements <- isabelle.JSON.bool_default(obj, "requirements")
            exclude_session_groups <- isabelle.JSON.strings_default(obj, "exclude_session_groups")
            all_sessions <- isabelle.JSON.bool_default(obj, "all_sessions")
            dirs <- isabelle.JSON.strings_default(obj, "dirs")
            session_groups <- isabelle.JSON.strings_default(obj, "session_groups")
            logic <- isabelle.JSON.string_default(obj, "logic", default_logic)
            options <- isabelle.JSON.strings_default(obj, "options")
            verbose <- isabelle.JSON.bool_default(obj, "verbose")
            exclude_sessions <- isabelle.JSON.strings_default(obj, "exclude_sessions")
            sessions <- isabelle.JSON.strings_default(obj, "sessions")
          } yield {
            Arguments(base_sessions = base_sessions,
              select_dirs = select_dirs,
              output_dir = output_dir,
              requirements = requirements,
              exclude_session_groups = exclude_session_groups,
              all_sessions = all_sessions,
              dirs = dirs,
              session_groups = session_groups,
              logic = logic,
              options = options,
              verbose = verbose,
              exclude_sessions = exclude_sessions,
              sessions = sessions)
          }).getOrElse(err)
        case _ => err
      }
    }

    private lazy val domain: Set[String] =
      Arguments().json.asInstanceOf[isabelle.JSON.Object.T].keySet
  }

  sealed case class Arguments(
    base_sessions: List[String] = Nil,
    select_dirs: List[String] = Nil,
    output_dir: String = Arguments.default_output_dir,
    requirements: Boolean = false,
    exclude_session_groups: List[String] = Nil,
    all_sessions: Boolean = false,
    dirs: List[String] = Nil,
    session_groups: List[String] = Nil,
    logic: String = Arguments.default_logic,
    options: List[String] = Nil,
    verbose: Boolean = false,
    exclude_sessions: List[String] = Nil,
    sessions: List[String] = Nil)
  {
    def selection: isabelle.Sessions.Selection =
      isabelle.Sessions.Selection(
        requirements = requirements,
        all_sessions = all_sessions,
        base_sessions = base_sessions,
        exclude_session_groups = exclude_session_groups,
        exclude_sessions = exclude_sessions,
        session_groups = session_groups,
        sessions = sessions)

    def json: isabelle.JSON.T =
      Map("base_sessions" -> base_sessions,
        "select_dirs" -> select_dirs,
        "output_dir" -> output_dir,
        "requirements" -> requirements,
        "exclude_session_groups" -> exclude_session_groups,
        "all_sessions" -> all_sessions,
        "dirs" -> dirs,
        "session_groups" -> session_groups,
        "logic" -> logic,
        "options" -> options,
        "verbose" -> verbose,
        "exclude_sessions" -> exclude_sessions,
        "sessions" -> sessions)

    def write_json(path: isabelle.Path): Unit =
    {
      isabelle.Isabelle_System.mkdirs(path.dir)
      isabelle.File.write(path, isabelle.JSON.Format(json))
    }
  }



  /** command-line tool **/

  def main(args: Array[String])
  {
    isabelle.Command_Line.tool0 {
      val arguments = Arguments.command_line(args.toList)
      val output_dir = isabelle.Path.explode(arguments.output_dir)
      arguments.write_json(output_dir + Arguments.source_file)

      val meta_inf = output_dir + isabelle.Path.explode("META-INF/MANIFEST.MF")
      isabelle.Isabelle_System.mkdirs(meta_inf.dir)
      isabelle.File.write(meta_inf, "id: Isabelle\ntitle: Isabelle\n")

      val controller = new Controller
      controller.setHome(File(output_dir.file))

      controller.handleLine("extension " + classOf[Importer].getName)
      if (arguments.verbose) {
        controller.handleLine("log console")
        controller.handleLine("log+ archive")
        controller.handleLine("log+ Isabelle")
      }
      controller.handleLine("mathpath archive .")
      controller.handleLine("build Isabelle Isabelle")
    }
  }
}

class Importer extends archives.Importer
{
  importer =>

  val key = "Isabelle"
  def inExts = List(Importer.Arguments.extension)

  def importDocument(bt: BuildTask, index: Document => Unit): BuildResult =
  {
    try {
      val arguments =
        Importer.Arguments.read_json(isabelle.Path.explode(isabelle.File.standard_path(bt.inFile.toJava)))

      object Isabelle extends Isabelle(importer.log(_), arguments)

      Isabelle.export_session((thy_export: Importer.Theory_Export) =>
      {
        val thy_name = thy_export.node_name
        val thy_qualifier = Isabelle.resources.session_base.theory_qualifier(thy_name)
        val thy_base_name = isabelle.Long_Name.base_name(thy_export.node_name.theory)

        // items
        var items = Isabelle.begin_theory(thy_export)

        def declare_item(entity: isabelle.Export_Theory.Entity, type_scheme: (List[String], isabelle.Term.Typ))
        : Importer.Item =
        {
          val item = Importer.Item(thy_name, entity, type_scheme)
          items = items.declare(item)
          item
        }

        // document
        val doc = new Document(DPath(bt.base / thy_qualifier / thy_base_name), root = true)
        controller.add(doc)

        val thy = Importer.declared_theory(thy_name)
        controller.add(thy)
        controller.add(MRef(doc.path, thy.path))

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
            controller.add(new OpaqueText(thy.asDocument.path, OpaqueText.defaultFormat, StringFragment(text)))
          }

          // classes
          for (decl <- segment.classes) {
            decl_error(decl.entity) {
              val item = declare_item(decl.entity, Importer.dummy_type_scheme)
              val tp = Isabelle.Class()
              controller.add(item.constant(Some(tp), None))
            }
          }

          // types
          for (decl <- segment.types) {
            decl_error(decl.entity) {
              val item = declare_item(decl.entity, Importer.dummy_type_scheme)
              val tp = Isabelle.Type(decl.args.length)
              val df = decl.abbrev.map(rhs => Isabelle.Type.abs(decl.args, Isabelle.import_type(items, rhs)))
              controller.add(item.constant(Some(tp), df))
            }
          }

          // consts
          for (decl <- segment.consts) {
            decl_error(decl.entity) {
              val item = declare_item(decl.entity, (decl.typargs, decl.typ))
              val tp = Isabelle.Type.all(decl.typargs, Isabelle.import_type(items, decl.typ))
              val df = decl.abbrev.map(rhs => Isabelle.Type.abs(decl.typargs, Isabelle.import_term(items, rhs)))
              controller.add(item.constant(Some(tp), df))
            }
          }

          // facts
          for (decl <- segment.facts_single) {
            decl_error(decl.entity) {
              val item = declare_item(decl.entity, Importer.dummy_type_scheme)
              val tp = Isabelle.import_prop(items, decl.prop)
              controller.add(item.constant(Some(tp), None))
            }
          }

          // locales
          for (decl <- segment.locales) {
            decl_error(decl.entity) {
              val item = declare_item(decl.entity, Importer.dummy_type_scheme)
              val tp = Isabelle.import_locale(items, decl)
              controller.add(item.constant(Some(tp), None))
            }
          }
        }

        Isabelle.end_theory(thy_export, items)
        index(doc)
      })
    }
    catch { case isabelle.ERROR(msg) => throw new Importer.Isabelle_Error(msg) }

    BuildResult.empty
  }
}

class Isabelle(log: String => Unit, arguments: Importer.Arguments)
{
  /* logging */

  val logger: isabelle.Logger =
    new isabelle.Logger { def apply(msg: => String): Unit = log(msg) }

  val progress: isabelle.Progress =
    new isabelle.Progress {
      override def echo(msg: String): Unit = log(msg)
      override def theory_percentage(session: String, theory: String, percentage: Int): Unit =
      {
        if (arguments.verbose)
          echo(isabelle.Progress.theory_message(session, theory) + ": " + percentage + "%")
      }
    }


  /* options */

  isabelle.Isabelle_System.init()

  val options: isabelle.Options =
    (isabelle.Dump.make_options(isabelle.Options.init(), isabelle.Dump.known_aspects) /: arguments.options)(_ + _)

  val store: isabelle.Sessions.Store = isabelle.Sessions.store(options)
  val cache: isabelle.Term.Cache = isabelle.Term.make_cache()


  /* session */

  private var _session: Option[isabelle.Thy_Resources.Session] = None

  def session: isabelle.Thy_Resources.Session =
    _session.getOrElse(isabelle.error("No Isabelle/PIDE session"))

  def resources: isabelle.Thy_Resources = session.resources

  def import_name(s: String): isabelle.Document.Node.Name =
    resources.import_name(isabelle.Sessions.DRAFT, "", s)

  def start_session(): isabelle.Thy_Resources.Theories_Result =
  {
    val dirs = arguments.dirs.map(isabelle.Path.explode)
    val select_dirs = arguments.select_dirs.map(isabelle.Path.explode)

    val session_deps: isabelle.Sessions.Deps =
      isabelle.Sessions.load_structure(options, dirs = dirs, select_dirs = select_dirs).
        selection_deps(arguments.selection, progress = progress)

    val build_results =
      isabelle.Build.build(options, progress = progress,
        dirs = dirs ::: select_dirs, sessions = List(arguments.logic))
    if (!build_results.ok) isabelle.error("Failed to build Isabelle/" + arguments.logic)

    _session =
      Some(isabelle.Thy_Resources.start_session(options, arguments.logic,
        session_dirs = dirs ::: select_dirs,
        include_sessions = session_deps.sessions_structure.imports_topological_order,
        progress = progress, log = logger))

    session.use_theories(
      session_deps.sessions_structure.build_topological_order.
        flatMap(session_name => session_deps.session_bases(session_name).used_theories.map(_.theory)),
      progress = progress)
  }

  def stop_session()
  {
    session.stop()
    _session = None
  }

  def export_session(export: Importer.Theory_Export => Unit)
  {
    val use_theories_result = start_session()

    export(pure_theory_export)

    val (nodes_ok, nodes_bad) =
      use_theories_result.nodes.partition({ case (_, st) => st.ok && st.consolidated })

    for ((name, st) <- nodes_bad) {
      progress.echo_error_message("Bad theory " + name +
        (if (st.consolidated) "" else ": " + st.percentage + "% finished"))
    }

    for {
      (name, _) <- nodes_bad
      snapshot = use_theories_result.snapshot(name)
      (msg, pos) <- snapshot.messages if isabelle.Protocol.is_error(msg)
    } {
      progress.echo_error_message(
        "Error" + isabelle.Position.here(pos) + ":\n" +
        isabelle.XML.content(isabelle.Pretty.formatted(List(msg))))
    }

    for { (name, _) <- nodes_ok } {
      val thy_export = read_theory_export(use_theories_result.snapshot(name))
      export(thy_export)
    }

    stop_session()

    if (nodes_bad.nonEmpty) {
      isabelle.error("theory " +
        isabelle.Library.commas(nodes_bad.map(p => p._1.theory).sorted) + " FAILED")
    }
  }


  /* Pure theory */

  def PURE: String = isabelle.Thy_Header.PURE
  def pure_name: isabelle.Document.Node.Name = import_name(PURE)

  lazy val pure_theory: isabelle.Export_Theory.Theory =
    isabelle.Export_Theory.read_pure_theory(store, cache = Some(cache))

  def pure_theory_export: Importer.Theory_Export =
  {
    val segment =
      Importer.Theory_Segment(
        classes = pure_theory.classes,
        types =
          for {
            decl <- pure_theory.types
            if decl.entity.name != isabelle.Pure_Thy.DUMMY && decl.entity.name != isabelle.Pure_Thy.FUN
          } yield decl,
        consts = pure_theory.consts,
        facts = pure_theory.facts,
        locales = pure_theory.locales)
    Importer.Theory_Export(pure_name, Nil, List(segment))
  }

  private def pure_entity(entities: List[isabelle.Export_Theory.Entity], name: String): GlobalName =
    entities.collectFirst({ case entity if entity.name == name => Importer.Item(pure_name, entity).global_name }).
      getOrElse(isabelle.error("Unknown entity " + isabelle.quote(name)))

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


  /* user theories */

  def read_theory_export(snapshot: isabelle.Document.Snapshot): Importer.Theory_Export =
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
                      isabelle.quote(st.command.source) + " in " +
                      isabelle.quote(st.command.node_name.node))
                }
            }
          element.outline_iterator.exists(cmd => cmd.id == entity_command.id)
        }
        Importer.Theory_Segment(
          element = element,
          classes = for (decl <- theory.classes if defined(decl.entity)) yield decl,
          types = for (decl <- theory.types if defined(decl.entity)) yield decl,
          consts = for (decl <- theory.consts if defined(decl.entity)) yield decl,
          facts = for (decl <- theory.facts if defined(decl.entity)) yield decl,
          locales = for (decl <- theory.locales if defined(decl.entity)) yield decl)
      }
    }
    Importer.Theory_Export(node_name, theory.parents, segments)
  }

  def use_theories(theories: List[String]): isabelle.Thy_Resources.Theories_Result =
    session.use_theories(theories, progress = progress)


  /* imported theory items */

  private val imported = isabelle.Synchronized(Map.empty[String, Importer.Items])

  def the_theory(name: String): Importer.Items =
    imported.value.getOrElse(name, isabelle.error("Unknown theory " + isabelle.quote(name)))

  def begin_theory(thy_export: Importer.Theory_Export): Importer.Items =
    Importer.Items.merge(thy_export.parents.map(the_theory(_)))

  def end_theory(thy_export: Importer.Theory_Export, items: Importer.Items): Unit =
    imported.change(map => map + (thy_export.node_name.theory -> items))

  def import_class(items: Importer.Items, name: String): Term =
    OMS(items.get_class(name).global_name)

  def import_type(items: Importer.Items, ty: isabelle.Term.Typ): Term =
  {
    def typ(t: isabelle.Term.Typ): Term = import_type(items, t)
    try {
      ty match {
        case isabelle.Term.Type(isabelle.Pure_Thy.FUN, List(a, b)) => lf.Arrow(typ(a), typ(b))
        case isabelle.Term.Type(name, args) =>
          val op = OMS(items.get_type(name).global_name)
          if (args.isEmpty) op else OMA(lf.Apply.term, op :: args.map(typ(_)))
        case isabelle.Term.TFree(a, _) => OMV(a)
        case isabelle.Term.TVar(xi, _) => isabelle.error("Illegal schematic type variable " + xi.toString)
      }
    }
    catch { case isabelle.ERROR(msg) => isabelle.error(msg + "\nin type " + ty) }
  }

  def import_term(items: Importer.Items, tm: isabelle.Term.Term): Term =
  {
    def typ(ty: isabelle.Term.Typ): Term = import_type(items, ty)
    def term(bounds: List[String], t: isabelle.Term.Term): Term =
      t match {
        case isabelle.Term.Const(c, ty) =>
          val item = items.get_const(c)
          Type.app(OMS(item.global_name), item.typargs(ty).map(typ(_)))
        case isabelle.Term.Free(x, ty) => OMV(x)
        case isabelle.Term.Var(xi, _) => isabelle.error("Illegal schematic variable " + xi.toString)
        case isabelle.Term.Bound(i) =>
          val x =
            try { bounds(i) }
            catch { case _: IndexOutOfBoundsException => isabelle.error("Loose de-Bruijn index " + i) }
          OMV(x)
        case isabelle.Term.Abs(x, ty, b) => lf.Lambda(LocalName(x), typ(ty), term(x :: bounds, b))
        case isabelle.Term.App(a, b) => lf.Apply(term(bounds, a), term(bounds, b))
      }

    try { term(Nil, tm) }
    catch { case isabelle.ERROR(msg) => isabelle.error(msg + "\nin term " + tm) }
  }

  def import_args(
    items: Importer.Items,
    typargs: List[(String, isabelle.Term.Sort)],
    args: List[(String, isabelle.Term.Typ)]): (List[String], List[Term], List[VarDecl]) =
  {
    val types = typargs.map(_._1)
    val sorts = typargs.flatMap({ case (a, s) => s.map(c => lf.Apply(import_class(items, c), OMV(a))) })
    val vars = args.map({ case (x, ty) => OMV(x) % import_type(items, ty) })
    (types, sorts, vars)
  }

  def import_prop(items: Importer.Items, prop: isabelle.Export_Theory.Prop): Term =
  {
    val (types, sorts, vars) = import_args(items, prop.typargs, prop.args)
    val t = import_term(items, prop.term)
    Type.all(types, lf.Arrow(sorts, if (vars.isEmpty) t else lf.Pi(vars, t)))
  }

  def import_locale(items: Importer.Items, locale: isabelle.Export_Theory.Locale): Term =
  {
    val (types, sorts, vars) = import_args(items, locale.typargs, locale.args)
    val t = Prop()
    Type.all(types, lf.Arrow(sorts, if (vars.isEmpty) t else lf.Pi(vars, t)))
  }
}
