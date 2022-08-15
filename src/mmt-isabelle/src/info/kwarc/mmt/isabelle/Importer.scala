package info.kwarc.mmt.isabelle

import scala.collection.SortedMap
import scala.util.matching.Regex
import info.kwarc.mmt.lf
import info.kwarc.mmt.api._
import frontend.{Controller, ReportHandler}
import archives.{Archive, Build, NonTraversingImporter}
import info.kwarc.mmt.api.parser.{SourcePosition, SourceRegion, SourceRef}
import notations._
import symbols._
import modules._
import documents._
import objects._
import opaque._
import utils._

/**
 * Isabelle/MMT [[Importer]] as command-line tool within the Isabelle environment.
 */
object Importer {
  /** MMT system environment **/

  def init_environment(
    options: isabelle.Options,
    progress: isabelle.Progress = new isabelle.Progress,
    archive_dirs: List[isabelle.Path] = Nil,
    init_archive: Boolean = false
  ): (Controller, List[Archive]) = {
    val controller = new Controller

    controller.report.addHandler(
      new ReportHandler("Isabelle/MMT") {
        override def apply(ind: Int, caller: => String, group: String, lines: List[String]): Unit =
          for (line <- lines) progress.echo(" " * ind + group + ": " + line)
      })

    for {
      config <-
        List(File(isabelle.Path.explode("$ISABELLE_MMT_ROOT/deploy/mmtrc").file),
          MMTSystem.userConfigFile)
      if config.exists
    } controller.loadConfigFile(config, false)

    def add_archive(root: File): Unit = {
      progress.echo("Adding " + root)
      controller.addArchive(root)
    }

    isabelle.Isabelle_System.getenv("ISABELLE_MMT_URTHEORIES") match {
      case "" => progress.echo_warning("Missing settings for ISABELLE_MMT_URTHEORIES")
      case dir => add_archive(isabelle.Path.explode(dir).absolute_file)
    }

    val init_archive_dir =
      (if (init_archive) options.proper_string("mmt_archive_dir") else None).
        map(isabelle.Path.explode)

    val archives: List[Archive] =
      (init_archive_dir.toList ::: archive_dirs).flatMap(dir =>
        controller.backend.openArchive(dir.absolute_file))
      match {
        case Nil if init_archive_dir.isDefined =>
          val meta_inf = init_archive_dir.get + isabelle.Path.explode("META-INF/MANIFEST.MF")
          isabelle.Isabelle_System.make_directory(meta_inf.dir)

          val id = options.proper_string("mmt_archive_id").map("id: " + _)
          val title = options.proper_string("mmt_archive_title").map("title: " + _)
          val narration_base = "narration-base: https://isabelle.in.tum.de"
          isabelle.File.write(meta_inf,
            (id.toList ::: title.toList ::: List(narration_base)).mkString("", "\n", "\n"))

          controller.backend.openArchive(init_archive_dir.get.absolute_file) match {
            case Nil => isabelle.error("Failed to initialize archive in " + init_archive_dir)
            case archives => archives
          }
        case archives => archives
      }

    for (archive <- archives) add_archive(archive.root)

    (controller, archives)
  }



  /** type checking **/

  def solver_error(solver: checking.Solver, err: => String): Unit = {
    val errs =
      for {
        solver_error <- solver.getErrors
        history_entry <- solver_error.history.steps
      } yield history_entry.present(solver.presentObj)

    if (errs.nonEmpty) isabelle.error(isabelle.cat_lines(err :: errs))
  }

  def check_term(controller: Controller, context: Context, t: Term): Unit = {
    checking.Solver.check(controller, Stack(context), t) match {
      case Left(_) =>
      case Right(solver) => solver_error(solver, "Failed to check term: " + t)
    }
  }

  def check_term_type(controller: Controller, context: Context, t: Term, tp: Term): Unit = {
    checking.Solver.checkType(controller, context, t, tp) match {
      case None =>
      case Some(solver) =>
        solver_error(solver, "Failed to check term: " + t + "\nagainst type: " + tp)
    }
  }



  /** source with position: offset, line, column (counting 16-bit Char addresses from 0) **/

  object Source {
    def apply(text0: String): Source = {
      val text = isabelle.Symbol.decode(isabelle.Line.normalize(text0))
      val line_doc = isabelle.Line.Document(text)
      val text_chunk = isabelle.Symbol.Text_Chunk(text)
      new Source(line_doc, text_chunk)
    }

    val empty: Source = apply("")
  }

  final class Source private(line_doc: isabelle.Line.Document, text_chunk: isabelle.Symbol.Text_Chunk) {
    def is_empty: Boolean = line_doc.lines.isEmpty

    def text: String = line_doc.text
    override def toString: String = line_doc.text

    def position(offset: isabelle.Text.Offset): SourcePosition = {
      if (offset >= 0 && offset <= line_doc.text_length) {
        val line_pos = line_doc.position(offset)
        SourcePosition(offset, line_pos.line, line_pos.column)
      }
      else isabelle.error("Bad position offset " + offset)
    }

    def region(range: isabelle.Text.Range): SourceRegion = {
      if (!range.is_singularity) {
        SourceRegion(position(range.start), position(range.stop - 1))
      }
      else isabelle.error("Bad range singularity " + range)
    }

    def ref(opt_uri: Option[URI], range: isabelle.Text.Range): Option[SourceRef] =
      for { uri <- opt_uri } yield { SourceRef(uri, region(range)) }

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

  sealed case class Proof_Text(
    index: Int,
    range: isabelle.Text.Range,
    timing: isabelle.Document_Status.Overall_Timing)

  sealed case class Theory_Export(
    node_name: isabelle.Document.Node.Name,
    node_source: Source = Source.empty,
    node_timing: isabelle.Document_Status.Overall_Timing = isabelle.Document_Status.Overall_Timing.empty,
    node_meta_data: isabelle.Properties.T = Nil,
    parents: List[String] = Nil,
    segments: List[Theory_Segment],
    constdefs: List[isabelle.Export_Theory.Constdef],
    typedefs: List[isabelle.Export_Theory.Typedef])

  sealed case class Theory_Segment(
    element: isabelle.Thy_Element.Element_Command = isabelle.Thy_Element.atom(isabelle.Command.empty),
    element_timing: isabelle.Document_Status.Overall_Timing = isabelle.Document_Status.Overall_Timing.empty,
    command_kind: Option[String] = None,
    document_tags: List[String] = Nil,
    document_command: Boolean = false,
    meta_data: isabelle.Properties.T = Nil,
    heading: Option[Int] = None,
    proof: Option[Proof_Text] = None,
    messages: List[String] = Nil,
    classes: List[isabelle.Export_Theory.Class] = Nil,
    types: List[isabelle.Export_Theory.Type] = Nil,
    consts: List[isabelle.Export_Theory.Const] = Nil,
    axioms: List[isabelle.Export_Theory.Axiom] = Nil,
    thms: List[isabelle.Export_Theory.Thm] = Nil,
    locales: List[isabelle.Export_Theory.Locale] = Nil,
    locale_dependencies: List[isabelle.Export_Theory.Locale_Dependency] = Nil,
    datatypes: List[isabelle.Export_Theory.Datatype] = Nil,
    spec_rules: List[isabelle.Export_Theory.Spec_Rule] = Nil
  ) {
    val header: String =
      element.head.span.content.iterator.takeWhile(tok => !tok.is_begin).map(_.source).mkString
    def header_relevant: Boolean =
      header.nonEmpty &&
        (document_command || classes.nonEmpty || types.nonEmpty || consts.nonEmpty ||
          thms.nonEmpty || locales.nonEmpty || locale_dependencies.nonEmpty)

    def command_name: String = element.head.span.name

    def is_definition: Boolean =
      command_kind match {
        case Some(kind) => isabelle.Keyword.theory_defn(kind)
        case None => false
      }

    def is_statement: Boolean =
      Set("axiomatization", "lemma", "theorem", "proposition", "corollary", "schematic_goal")(command_name)

    def is_axiomatization: Boolean = command_name == "axiomatization"

    def is_experimental: Boolean = element.iterator.exists(cmd => cmd.span.name == "sorry")
  }



  /** bootstrap theory according to MathHub/MMT/urtheories/source/isabelle.mmt **/

  object Bootstrap {
    val theory: MPath = lf.Typed._base ? "Isabelle"

    object Unknown {
      val path: GlobalName = GlobalName(theory, LocalName("unknown"))
      def apply(deps: List[GlobalName] = Nil): Term =
        if (deps.isEmpty) OMS(path) else OMA(OMS(path), deps.map(OMS(_)))
      def detect(t: Term): Boolean = t.head.contains(path)
    }

    object Type {
      def apply(n: Int = 0): Term = {
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

    object Prop {
      val path: GlobalName = GlobalName(theory, LocalName("prop"))
      def apply(): Term = OMS(path)
    }

    object Ded {
      val path: GlobalName = GlobalName(theory, LocalName("ded"))
      def apply(t: Term): Term = lf.Apply(OMS(path), t)
    }

    object Proof_Text {
      val path: GlobalName = GlobalName(theory, LocalName("proof_text"))
      def apply(): Term = OMS(path)
    }
  }



  /** MMT import structures **/

  class Indexed_Name(val name: String) {
    def apply(i: Int): String = name + "(" + i + ")"
    private val Pattern = (Regex.quote(name) + """\((\d+)\)""").r
    def unapply(s: String): Option[Int] =
      s match { case Pattern(isabelle.Value.Int(i)) => Some(i) case _ => None }
  }

  object Env {
    val empty: Env = new Env(Map.empty)
  }

  final class Env private(rep: Map[String, Term]) {
    def get(x: String): Term = rep.getOrElse(x, OMV(x))
    def + (entry: (String, Term)): Env = new Env(rep + entry)
  }

  def notation(
    xname: Option[String],
    implicit_args: Int,
    syntax: isabelle.Export_Theory.Syntax
  ): NotationContainer = {
    val notation = NotationContainer()

    def prefix_notation(delim: String, impl: Int): TextNotation =
      new TextNotation(Prefix(Delim(isabelle.Symbol.decode(delim)), impl, 0), Precedence.infinite, None, false)

    def xname_notation: List[TextNotation] = xname.toList.map(prefix_notation(_, 0))

    val text_notations =
      syntax match {
        case isabelle.Export_Theory.No_Syntax => xname_notation
        case isabelle.Export_Theory.Prefix(delim) =>
          List(prefix_notation(delim, implicit_args))
        case infix : isabelle.Export_Theory.Infix =>
          val infix_notation = {
            val assoc =
              infix.assoc match {
                case isabelle.Export_Theory.Assoc.NO_ASSOC => None
                case isabelle.Export_Theory.Assoc.LEFT_ASSOC => Some(true)
                case isabelle.Export_Theory.Assoc.RIGHT_ASSOC => Some(false)
              }
            val delim = Delim(isabelle.Symbol.decode(infix.delim))
            val fixity = Infix(delim, implicit_args, 2, assoc)
            new TextNotation(fixity, Precedence.integer(infix.pri), None, false)
          }
          xname_notation ::: List(infix_notation)
      }
    text_notations.foreach(notation.parsingDim.set(_))

    notation
  }

  val dummy_type_scheme: (List[String], isabelle.Term.Typ) = (Nil, isabelle.Term.dummyT)

  object Item {
    object Key {
      object Ordering extends scala.math.Ordering[Key] {
        def compare(key1: Key, key2: Key): Int =
          key1.entity_kind compare key2.entity_kind match {
            case 0 => key1.entity_name compare key2.entity_name
            case ord => ord
          }
      }
    }

    sealed case class Key(entity_kind: String, entity_name: String) {
      override def toString: String = entity_kind + " " + isabelle.quote(entity_name)
    }

    object Name {
      def apply(theory_path: MPath, entity_kind: String, entity_name: String): Name =
        new Name(theory_path, Key(entity_kind, entity_name))
    }

    final class Name private(val theory_path: MPath, val key: Key) {
      def entity_kind: String = key.entity_kind
      def entity_name: String = key.entity_name
      def local: LocalName = LocalName(entity_name + "|" + entity_kind)
      def global: GlobalName = GlobalName(theory_path, local)
    }

    def apply(
      theory_path: MPath,
      entity_kind: String,
      entity_name: String,
      entity_xname: String = "",
      entity_pos: isabelle.Position.T = isabelle.Position.none,
      syntax: isabelle.Export_Theory.Syntax = isabelle.Export_Theory.No_Syntax,
      type_scheme: (List[String], isabelle.Term.Typ) = dummy_type_scheme,
      theory_source: Option[URI] = None,
      node_source: Source = Source.empty
    ): Item = {
      val name = Name(theory_path, entity_kind.intern, entity_name.intern)
      new Item(name, if (entity_xname.nonEmpty) entity_xname else entity_name,
        entity_pos, syntax, type_scheme, theory_source, node_source)
    }
  }

  final class Item private(
    val name: Item.Name,
    val entity_xname: String,
    val entity_pos: isabelle.Position.T,
    val syntax: isabelle.Export_Theory.Syntax,
    val type_scheme: (List[String], isabelle.Term.Typ),
    val theory_source: Option[URI],
    val node_source: Source
  ) {
    def source_ref: Option[SourceRef] =
      node_source.ref(theory_source, entity_pos)

    def source_ref_range(range: isabelle.Text.Range): Option[SourceRef] =
      node_source.ref(theory_source, range)

    def constant(tp: Option[Term], df: Option[Term]): Constant = {
      val notC = notation(Some(entity_xname), type_scheme._1.length, syntax)
      val c = Constant(OMID(name.theory_path), name.local, Nil, tp, df, None, notC)
      for (sref <- source_ref) SourceRef.update(c, sref)
      c
    }
  }

  def dependencies(term: Term): Set[ContentPath] = {
    var result = Set.empty[ContentPath]
    def deps(obj: Obj): Unit = {
      obj match {
        case OMS(name) => if (!result.contains(name)) result += name
        case _ => for ((_, sub) <- obj.subobjects) deps(sub)
      }
    }
    deps(term)
    result
  }

  object Sorts extends Indexed_Name("sorts")

  object Locale {
    object Axioms extends Indexed_Name("axioms")
  }

  object Datatypes {
    object Kind extends Enumeration {
      val DATATYPE = Value("datatype")
      val CODATATYPE = Value("codatatype")
    }
    val HEAD = "head"
    object Constructors extends Indexed_Name("constructors")
  }

  object Spec_Rules {
    object Kind extends Enumeration {
      val DEFINITION = Value("definition")
      val RECURSIVE_DEFINITION = Value("recursive_definition")
      val INDUCTIVE_DEFINITION = Value("inductive_definition")
      val COINDUCTIVE_DEFINITION = Value("coinductive_definition")
      val SPECIFICATION = Value("specification")
    }
    object Terms extends Indexed_Name("terms")
    object Rules extends Indexed_Name("rules")
  }


  /* theory content and statistics */

  private def print_int(n: Int, len: Int = 8): String =
    String.format(java.util.Locale.ROOT, "%" + len + "d", Integer.valueOf(n))

  object Triples_Stats {
    def empty: Triples_Stats = Triples_Stats(SortedMap.empty)
    def make(triples: List[isabelle.RDF.Triple]): Triples_Stats = triples.foldLeft(empty)(_ + _)
    def merge(args: TraversableOnce[Triples_Stats]): Triples_Stats = args.foldLeft(empty)(_ + _)
  }

  sealed case class Triples_Stats(stats: SortedMap[String, Int]) {
    def + (name: String, count: Int): Triples_Stats =
      Triples_Stats(stats + (name -> (stats.getOrElse(name, 0) + count)))

    def + (t: isabelle.RDF.Triple): Triples_Stats = this + (t.predicate, 1)

    def + (other: Triples_Stats): Triples_Stats =
      other.stats.foldLeft(this)({ case (a, (b, n)) => a + (b, n) })

    def total: Int = stats.iterator.map(_._2).sum

    def report: String =
      (for {(name, count) <- stats.iterator }
        yield { print_int(count, len = 12) + " " + name }).mkString("\n")
  }

  object Content {
    val empty: Content =
      new Content(
        SortedMap.empty[Item.Key, Item.Name](Item.Key.Ordering),
        SortedMap.empty[String, Triples_Stats])
    def merge(args: TraversableOnce[Content]): Content = args.foldLeft(empty)(_ ++ _)
  }

  final class Content private(
    private val item_names: SortedMap[Item.Key, Item.Name],  // imported entities
    private val triples: SortedMap[String, Triples_Stats]  // RDF triples per theory
  ) {
    content =>

    def items_size: Int = item_names.size
    def all_names: List[Item.Name] = item_names.iterator.map(_._2).toList
    def all_triples: Triples_Stats = Triples_Stats.merge(triples.iterator.map(_._2))

    def report_kind(kind: String): String =
      print_int(item_names.count({ case (_, name) => name.entity_kind == kind }), len = 12) + " " + kind

    def report: String = {
      val kinds =
        List(
          isabelle.Export_Theory.Kind.LOCALE,
          isabelle.Export_Theory.Kind.LOCALE_DEPENDENCY,
          isabelle.Export_Theory.Kind.CLASS,
          isabelle.Export_Theory.Kind.TYPE,
          isabelle.Export_Theory.Kind.CONST,
          isabelle.Export_Theory.Kind.AXIOM,
          isabelle.Export_Theory.Kind.THM).map(_.toString) :::
        Datatypes.Kind.values.toList.map(_.toString) :::
        Spec_Rules.Kind.values.toList.map(_.toString)
      isabelle.Library.cat_lines(kinds.map(report_kind))
    }

    def get(key: Item.Key): Item.Name = item_names.getOrElse(key, isabelle.error("Undeclared " + key.toString))
    def get_class(name: String): Item.Name = get(Item.Key(isabelle.Export_Theory.Kind.CLASS.toString, name))
    def get_type(name: String): Item.Name = get(Item.Key(isabelle.Export_Theory.Kind.TYPE.toString, name))
    def get_const(name: String): Item.Name = get(Item.Key(isabelle.Export_Theory.Kind.CONST.toString, name))
    def get_axiom(name: String): Item.Name = get(Item.Key(isabelle.Export_Theory.Kind.AXIOM.toString, name))
    def get_thm(name: String): Item.Name = get(Item.Key(isabelle.Export_Theory.Kind.THM.toString, name))
    def get_locale(name: String): Item.Name = get(Item.Key(isabelle.Export_Theory.Kind.LOCALE.toString, name))
    def get_locale_dependency(name: String): Item.Name =
      get(Item.Key(isabelle.Export_Theory.Kind.LOCALE_DEPENDENCY.toString, name))

    def is_empty: Boolean = item_names.isEmpty
    def defined(key: Item.Key): Boolean = item_names.isDefinedAt(key)

    def declare(theory_name: String, name: Item.Name): Content =
      if (defined(name.key)) {
        isabelle.error("Duplicate " + name.key.toString + " in theory " + isabelle.quote(theory_name))
      }
      else content + name

    def + (name: Item.Name): Content =
      if (defined(name.key)) content
      else new Content(item_names + (name.key -> name), triples)

    def + (entry: (String, Triples_Stats)): Content =
      triples.get(entry._1) match {
        case None => new Content(item_names, triples + entry)
        case Some(stats) =>
          if (stats == entry._2) content
          else isabelle.error("Incoherent triple stats for theory: " + isabelle.quote(entry._1))
      }

    def ++ (other: Content): Content =
      if (content eq other) content
      else if (is_empty) other
      else {
        val items1 = other.item_names.foldLeft(content)({ case (map, (_, name)) => map + name }).item_names
        val triples1 = other.triples.foldLeft(content)({ case (map, entry) => map + entry }).triples
        new Content(items1, triples1)
      }

    override def toString: String =
      item_names.iterator.map(_._2).mkString("Content(", ", ", ")")


    /* MMT import of Isabelle classes, types, terms etc. */

    def import_class(name: String): Term = OMS(get_class(name).global)

    def import_type(ty: isabelle.Term.Typ, env: Env = Env.empty): Term = {
      def typ(t: isabelle.Term.Typ): Term =
        t match {
          case isabelle.Term.Type(isabelle.Pure_Thy.FUN, List(a, b)) => lf.Arrow(typ(a), typ(b))
          case isabelle.Term.Type(isabelle.Pure_Thy.PROP, Nil) => Bootstrap.Prop()
          case isabelle.Term.Type(name, args) =>
            val op = OMS(get_type(name).global)
            if (args.isEmpty) op else OMA(lf.Apply.term, op :: args.map(typ))
          case isabelle.Term.TFree(a, _) => env.get(a)
          case isabelle.Term.TVar(xi, _) =>
            isabelle.error("Illegal schematic type variable " + xi.toString)
        }

      try { typ(ty) }
      catch { case isabelle.ERROR(msg) => isabelle.error(msg + "\nin type " + ty) }
    }

    object OFCLASS {  // FIXME workaround for Isabelle2020 on case-insensitive file-system
      import isabelle.Term._

      def unapply(t: Term): Option[(Typ, String)] =
        t match {
          case App(Const(Class_Const(c), List(ty)), Const(isabelle.Pure_Thy.TYPE, List(ty1)))
            if ty == ty1 => Some((ty, c))
          case _ => None
        }
    }

    def import_term(tm: isabelle.Term.Term, env: Env = Env.empty, bounds: List[String] = Nil): Term = {
      def typ(t: isabelle.Term.Typ): Term = import_type(t, env)

      def term(bs: List[String], t: isabelle.Term.Term): Term =
        t match {
          case isabelle.Term.Const(c, typargs) =>
            Bootstrap.Type.app(OMS(get_const(c).global), typargs.map(typ))
          case isabelle.Term.Free(x, _) => env.get(x)
          case isabelle.Term.Var(xi, _) =>
            isabelle.error("Illegal schematic variable " + xi.toString)
          case isabelle.Term.Bound(i) =>
            try { OMV(bs(i)) }
            catch { case _: IndexOutOfBoundsException => isabelle.error("Loose bound variable " + i) }
          case isabelle.Term.Abs(x, ty, b) =>
            lf.Lambda(LocalName(x), typ(ty), term(x :: bs, b))
          case OFCLASS(ty, c) =>
            lf.Apply(import_class(c), typ(ty))
          case isabelle.Term.App(a, b) =>
            lf.Apply(term(bs, a), term(bs, b))
        }

      try { term(bounds, tm) }
      catch { case isabelle.ERROR(msg) => isabelle.error(msg + "\nin term " + tm) }
    }

    def import_sorts(typargs: List[(String, isabelle.Term.Sort)]): List[Term] =
      typargs.flatMap({ case (a, s) => s.map(c => Bootstrap.Ded(lf.Apply(import_class(c), OMV(a)))) })

    def import_prop(prop: isabelle.Export_Theory.Prop, env: Env = Env.empty): Term = {
      val types = prop.typargs.map(_._1)
      val vars = prop.args.map({ case (x, ty) => OMV(x) % import_type(ty, env) })
      val sorts = import_sorts(prop.typargs)

      val t = import_term(prop.term, env)
      val p = lf.Arrow(sorts, Bootstrap.Ded(t))
      Bootstrap.Type.all(types, if (vars.isEmpty) p else lf.Pi(vars, p))
    }

    def import_proof(prf: isabelle.Term.Proof, env: Env = Env.empty): Term = {
      def typ(t: isabelle.Term.Typ): Term = import_type(t, env)
      def term(bs: List[String], t: isabelle.Term.Term): Term = import_term(t, env = env, bounds = bs)

      def proof(bs: List[String], ps: List[String], prf: isabelle.Term.Proof): Term =
        prf match {
          case isabelle.Term.PBound(i) =>
            try { OMV(ps(i)) }
            catch { case _: IndexOutOfBoundsException => isabelle.error("Loose bound variable (proof) " + i) }
          case isabelle.Term.Abst(x, ty, b) =>
            lf.Lambda(LocalName(x), typ(ty), proof(x :: bs, ps, b))
          case isabelle.Term.AbsP(x, hy, b) =>
            lf.Lambda(LocalName(x), term(bs, hy), proof(bs, x :: ps, b))
          case isabelle.Term.Appt(p, t) =>
            lf.Apply(proof(bs, ps, p), term(bs, t))
          case isabelle.Term.AppP(p, q) =>
            lf.Apply(proof(bs, ps, p), proof(bs, ps, q))
          case axm: isabelle.Term.PAxm =>
            Bootstrap.Type.app(OMS(get_axiom(axm.name).global), axm.types.map(typ))
          case thm: isabelle.Term.PThm =>
            Bootstrap.Type.app(OMS(get_thm(thm.name).global), thm.types.map(typ))
          case _ => isabelle.error("Bad proof term encountered:\n" + prf)
        }

      try { proof(Nil, Nil, prf) }
      catch { case isabelle.ERROR(msg) => isabelle.error(msg + "\nin proof term " + prf) }
    }


    /* nested declarations */

    def declare_import_types(thy: Theory, typargs: List[(String, isabelle.Term.Sort)]): Importer.Env = {
      typargs.foldLeft(Env.empty) {
        case (env, (a, _)) =>
          val c = Constant(thy.toTerm, LocalName(a), Nil, Some(Bootstrap.Type()), None, None)
          thy.add(c)
          env + (a -> c.toTerm)
      }
    }

    def declare_import_sorts(thy: Theory, typargs: List[(String, isabelle.Term.Sort)]): Unit = {
      for { (prop, i) <- content.import_sorts(typargs).zipWithIndex } {
        val name = LocalName(Sorts(i + 1))
        thy.add(Constant(thy.toTerm, name, Nil, Some(prop), None, None))
      }
    }
  }



  /** Isabelle to MMT importer **/

  private class State(options: isabelle.Options) {
    val store: isabelle.Sessions.Store = isabelle.Sessions.store(options)
    val cache: isabelle.Term.Cache = isabelle.Term.Cache.make()

    private val imported = isabelle.Synchronized(Map.empty[String, Content])

    def report_imported: String = {
      val theories = imported.value
      val content = Content.merge(theories.valuesIterator)
      val all_triples = content.all_triples

      print_int(theories.size) + " theories\n" +
        print_int(content.items_size) + " individuals:\n" + content.report + "\n" +
        print_int(all_triples.total) + " relations:\n" + all_triples.report
    }

    def theory_content(theory: String): Content =
      imported.value.getOrElse(theory, isabelle.error("Missing theory export " + isabelle.quote(theory)))

    def end_theory(theory: String, content: Content): Unit =
      imported.change(map => map + (theory -> content))
  }

  def importer(
    state: State,
    session: isabelle.Dump.Session,
    archive_dirs: List[isabelle.Path],
    chapter_archive: String => Option[String]
  ): Unit = {
    val options = session.context.options
    val progress = session.context.progress

    val (controller, archives) =
      init_environment(options, progress = progress, archive_dirs = archive_dirs, init_archive = true)

    object MMT_Importer extends NonTraversingImporter {
      val key = "isabelle-omdoc"

      //these methods are called by MMT if the importer is called from MMT
      def build(a: Archive, which: Build, in: FilePath, errorCont: Option[ErrorHandler]) : Unit = {
        throw LocalError("not implemented")
      }
      def clean(a: Archive, in: FilePath): Unit = {
        throw LocalError("not implemented")
      }
    }
    controller.extman.addExtension(MMT_Importer, Nil)

    val proof_terms_enabled = options.bool("mmt_proof_terms")

    object Isabelle extends Isabelle(state, session, archives, chapter_archive)

    def import_theory(thy_export: Theory_Export): Unit = {
      progress.echo("Processing theory " + thy_export.node_name + " ...")

      val thy_name = thy_export.node_name
      val thy_base_name = isabelle.Long_Name.base_name(thy_export.node_name.theory)
      val thy_is_pure: Boolean = thy_name == Isabelle.pure_name

      // archive
      val thy_archive = Isabelle.theory_archive(thy_name)

      // document
      val doc = new Document(thy_archive.doc_path, FileLevel)
      controller.add(doc)

      // theory content
      val thy_draft =
        Isabelle.begin_theory(thy_export,
          if (thy_is_pure) None else Some(thy_archive.archive_source_uri))
      val thy = thy_draft.thy

      controller.add(thy)
      controller.add(MRef(doc.path, thy.path))

      thy_draft.rdf_triple(Ontology.unary(thy.path, Ontology.ULO.theory))
      for ((a, b) <- thy_export.node_meta_data) {
        thy_draft.rdf_triple(Ontology.binary(thy.path, a, b))
      }

      def rdf_timing(timing: Double): Unit = {
        if (timing > 0.0) {
          thy_draft.rdf_triple(
            Ontology.binary(thy.path, Ontology.ULO.check_time,
              isabelle.RDF.long(isabelle.Time.seconds(timing).ms)))
        }
      }
      rdf_timing(thy_export.node_timing.total)

      if (thy_is_pure) {
        controller.add(PlainInclude(Bootstrap.theory, thy.path))
      }
      for (parent <- thy_export.parents) {
        controller.add(PlainInclude(Isabelle.make_theory(parent).path, thy.path))
      }

      def add_constant(item: Item, tp: Term, df: Option[Term]): Unit = {
        val context = Context(thy.path)
        if (options.bool("mmt_type_checking")) {
          for (t <- Iterator(tp) ++ df.iterator if !Bootstrap.Unknown.detect(t)) {
            check_term(controller, context, t)
          }
          if (df.isDefined && !Bootstrap.Unknown.detect(df.get)) {
            check_term_type(controller, context, df.get, tp)
          }
        }
        val c = item.constant(Some(tp), df)
        controller.add(c)

        for {
          t <- Iterator(tp) ++ df.iterator
          dep <- dependencies(t)
        } {
          thy_draft.rdf_triple(Ontology.binary(c.path, Ontology.ULO.uses, dep))
        }
      }

      def add_text(text: String): Unit = {
        val string = StringFragment(isabelle.Symbol.decode(text.replace(' ', '\u00a0')))
        val opaque = new OpaqueText(thy.asDocument.path, OpaqueText.defaultFormat, string)
        controller.add(opaque)
      }

      // PIDE theory source
      if (!thy_export.node_source.is_empty) {
        val path = thy_archive.archive_source_path
        val text_decoded = thy_export.node_source.text
        val text_encoded = isabelle.Symbol.encode(text_decoded)

        isabelle.Isabelle_System.make_directory(path.dir)
        isabelle.File.write(path, text_decoded)

        thy_draft.rdf_triple(
          Ontology.binary(thy.path, Ontology.ULO.external_size,
            isabelle.RDF.int(isabelle.UTF8.bytes(text_encoded).length)))
      }

      def decl_error[A](entity: isabelle.Export_Theory.Entity)(body: => A): Option[A] = {
        try { Some(body) }
        catch {
          case isabelle.ERROR(msg) =>
            isabelle.error(msg + "\nin declaration of " + entity + isabelle.Position.here(entity.pos))
            None
        }
      }

      for (segment <- thy_export.segments) {
        def make_dummy(kind: isabelle.Export_Theory.Kind.Value, i: Int): Item = {
          val name = isabelle.Long_Name.implode(List(thy_name.theory_base_name, i.toString))
          val pos = segment.element.head.span.position
          Item(thy.path, kind.toString, name, entity_pos = pos)
        }

        // input text
        if (segment.header_relevant) add_text(segment.header)

        // output text
        if (segment.messages.nonEmpty) {
          add_text("Output:")
          for (msg <- segment.messages) add_text(msg)
        }

        // document headings
        for (i <- segment.heading) {
          val item = make_dummy(isabelle.Export_Theory.Kind.DOCUMENT_HEADING, i)
          thy_draft.declare_item(item, segment.document_tags, segment.meta_data)
          thy_draft.rdf_triple(Ontology.unary(item.name.global, Ontology.ULO.section))
        }

        // classes
        for (decl <- segment.classes) {
          decl_error(decl.entity) {
            val item = thy_draft.declare_entity(decl.entity, segment.document_tags, segment.meta_data)
            thy_draft.rdf_triple(Ontology.unary(item.name.global, Ontology.ULO.universe))
            val tp = Isabelle.Class()
            add_constant(item, tp, None)
          }
        }

        // types
        for (decl <- segment.types) {
          decl_error(decl.entity) {
            val item = thy_draft.make_item(decl.entity, decl.syntax)
            thy_draft.declare_item(item, segment.document_tags, segment.meta_data)

            thy_draft.rdf_triple(Ontology.unary(item.name.global, Ontology.ULO.`type`))
            if (thy_export.typedefs.exists(typedef => typedef.name == item.name.entity_name)) {
              thy_draft.rdf_triple(Ontology.unary(item.name.global, Ontology.ULO.derived))
            }

            val tp = Bootstrap.Type(decl.args.length)
            val df = decl.abbrev.map(rhs => Bootstrap.Type.abs(decl.args, thy_draft.content.import_type(rhs)))
            add_constant(item, tp, df)
          }
        }

        // consts
        for (decl <- segment.consts) {
          decl_error(decl.entity) {
            val item = thy_draft.make_item(decl.entity, decl.syntax, (decl.typargs, decl.typ))
            thy_draft.declare_item(item, segment.document_tags, segment.meta_data)

            if (segment.is_axiomatization) {
              thy_draft.rdf_triple(Ontology.unary(item.name.global, Ontology.ULO.primitive))
            }

            if (decl.propositional) {
              thy_draft.rdf_triple(Ontology.unary(item.name.global, Ontology.ULO.predicate))
            }
            else {
              thy_draft.rdf_triple(Ontology.unary(item.name.global, Ontology.ULO.function))
            }

            val tp = Bootstrap.Type.all(decl.typargs, thy_draft.content.import_type(decl.typ))
            val df = decl.abbrev.map(rhs => Bootstrap.Type.abs(decl.typargs, thy_draft.content.import_term(rhs)))
            add_constant(item, tp, df)
          }
        }

        // axioms
        for (decl <-segment.axioms) {
          val item = thy_draft.declare_entity(decl.entity, segment.document_tags, segment.meta_data)
          val tp = thy_draft.content.import_prop(decl.prop)
          add_constant(item, tp, None)
        }

        // theorems
        val thms: List[Item] =
          segment.thms.flatMap({ decl =>
            decl_error(decl.entity) {
              val item = thy_draft.declare_entity(decl.entity, segment.document_tags, segment.meta_data)
              thy_draft.rdf_triple(Ontology.unary(item.name.global, Ontology.ULO.statement))

              if (segment.is_definition) {
                thy_draft.rdf_triple(Ontology.unary(item.name.global, Ontology.ULO.definition))
              }

              if (segment.is_statement) {
                thy_draft.rdf_triple(Ontology.unary(item.name.global, Ontology.ULO.para))
                thy_draft.rdf_triple(
                  Ontology.binary(item.name.global, Ontology.ULO.paratype, segment.command_name))
              }

              if (segment.is_axiomatization) {
                thy_draft.rdf_triple(Ontology.unary(item.name.global, Ontology.ULO.primitive))
              }
              else {
                thy_draft.rdf_triple(Ontology.unary(item.name.global, Ontology.ULO.derived))
              }

              if (segment.is_experimental) {
                thy_draft.rdf_triple(Ontology.unary(item.name.global, Ontology.ULO.experimental))
              }

              val deps = decl.deps.map(dep => thy_draft.content.get_thm(dep).global)
              for (dep <- deps) {
                thy_draft.rdf_triple(Ontology.binary(item.name.global, Ontology.ULO.uses, dep))
              }

              val tp = thy_draft.content.import_prop(decl.prop)
              val prf =
                if (proof_terms_enabled) thy_draft.content.import_proof(decl.proof)
                else Bootstrap.Unknown(deps)
              add_constant(item, tp, Some(prf))

              item
            }
          })

        // optional proof text
        for (proof <- segment.proof) yield {
          val item = make_dummy(isabelle.Export_Theory.Kind.PROOF_TEXT, proof.index)
          val c = item.constant(Some(Bootstrap.Proof_Text()), None)
          for (sref <- item.source_ref_range(proof.range)) SourceRef.update(c, sref)
          controller.add(c)

          rdf_timing(proof.timing.total)

          for (thm <- thms) {
            thy_draft.rdf_triple(Ontology.binary(c.path, Ontology.ULO.justifies, thm.name.global))
          }
        }

        // locales
        for (locale <- segment.locales) {
          decl_error(locale.entity) {
            val content = thy_draft.content
            val item = thy_draft.declare_entity(locale.entity, segment.document_tags, segment.meta_data)
            thy_draft.rdf_triple(Ontology.unary(item.name.global, Ontology.ULO.theory))
            val loc_name = item.name.local
            val loc_thy = Theory.empty(thy.path.doc, thy.name / loc_name, None)

            def loc_decl(d: Declaration): Unit = {
              loc_thy.add(d)
              thy_draft.rdf_triple(Ontology.binary(loc_thy.path, Ontology.ULO.specifies, d.path))
              thy_draft.rdf_triple(Ontology.binary(d.path, Ontology.ULO.specified_in, loc_thy.path))
            }

            // type parameters
            val type_env = content.declare_import_types(loc_thy, locale.typargs)

            // term parameters
            val term_env =
              locale.args.foldLeft(type_env) {
                case (env, ((x, ty), syntax)) =>
                  val notC = notation(None, 0, syntax)
                  val tp = content.import_type(ty, type_env)
                  val c = Constant(loc_thy.toTerm, LocalName(x), Nil, Some(tp), None, None, notC)
                  loc_decl(c)
                  env + (x -> c.toTerm)
              }

            // sort constraints
            content.declare_import_sorts(loc_thy, locale.typargs)

            // logical axioms
            for { (prop, i) <- locale.axioms.map(content.import_prop(_, env = term_env)).zipWithIndex } {
              val name = LocalName(Locale.Axioms(i + 1))
              loc_decl(Constant(loc_thy.toTerm, name, Nil, Some(prop), None, None))
            }

            controller.add(new NestedModule(thy.toTerm, loc_name, loc_thy))
          }
        }

        // locale dependencies (inclusions)
        for (dep <- segment.locale_dependencies if dep.is_inclusion) {
          decl_error(dep.entity) {
            val item = thy_draft.declare_entity(dep.entity, segment.document_tags, segment.meta_data)
            val content = thy_draft.content

            val from = OMMOD(content.get_locale(dep.source).global.toMPath)
            val to = OMMOD(content.get_locale(dep.target).global.toMPath)

            val view = View(thy.path.doc, thy.name / item.name.local, from, to, false)
            controller.add(new NestedModule(thy.toTerm, item.name.local, view))

            thy_draft.rdf_triple(Ontology.binary(to.path, Ontology.ULO.instance_of, from.path))
          }
        }

        // datatypes
        for (datatype <- segment.datatypes) {
          try {
            val content = thy_draft.content

            val item = thy_draft.make_datatype_item(datatype)
            thy_draft.declare_item(item, segment.document_tags, segment.meta_data)

            val datatype_name = item.name.local
            val datatype_thy = Theory.empty(thy.path.doc, thy.name / datatype_name, None)

            // type variables
            val env =
              datatype.typargs.foldLeft(Env.empty) {
                case (env, (a, _)) =>
                  val c = Constant(datatype_thy.toTerm, LocalName(a), Nil, Some(Bootstrap.Type()), None, None)
                  datatype_thy.add(c)
                  env + (a -> c.toTerm)
              }

            // sort constraints
            content.declare_import_sorts(datatype_thy, datatype.typargs)

            // head
            {
              val head = content.import_type(datatype.typ, env = env)
              val name = LocalName(Datatypes.HEAD)
              datatype_thy.add(Constant(datatype_thy.toTerm, name, Nil, None, Some(head), None))
            }

            // constructors
            val constructors =
              for ((term, typ) <- datatype.constructors)
                yield {
                  (content.import_term(term, env = env),
                    content.import_type(typ, env = env))
                }
            for { ((df, tp), i) <- constructors.zipWithIndex } {
              val name = LocalName(Datatypes.Constructors(i + 1))
              datatype_thy.add(Constant(datatype_thy.toTerm, name, Nil, Some(tp), Some(df), None))
            }

            controller.add(new NestedModule(thy.toTerm, datatype_name, datatype_thy))
          }
          catch {
            case isabelle.ERROR(msg) => isabelle.error(msg + "\nin datatype " + isabelle.quote(datatype.name))
          }
        }

        // spec rules
        for (spec_rule <- segment.spec_rules) {
          try {
            val content = thy_draft.content

            val item = thy_draft.make_spec_item(spec_rule)
            thy_draft.declare_item(item, segment.document_tags, segment.meta_data)

            val spec_name = item.name.local
            val spec_thy = Theory.empty(thy.path.doc, thy.name / spec_name, None)

            // type variables
            val type_env = content.declare_import_types(spec_thy, spec_rule.typargs)

            // term variables
            val term_env =
              spec_rule.args.foldLeft(type_env) {
                case (env, (x, ty)) =>
                  val tp = content.import_type(ty, type_env)
                  val c = Constant(spec_thy.toTerm, LocalName(x), Nil, Some(tp), None, None)
                  spec_thy.add(c)
                  env + (x -> c.toTerm)
              }

            // sort constraints
            content.declare_import_sorts(spec_thy, spec_rule.typargs)

            // spec terms
            val spec_terms =
              for ((term, typ) <- spec_rule.terms)
              yield {
                (content.import_term(term, env = term_env),
                 content.import_type(typ, env = type_env))
              }
            for { ((df, tp), i) <- spec_terms.zipWithIndex } {
              val name = LocalName(Spec_Rules.Terms(i + 1))
              spec_thy.add(Constant(spec_thy.toTerm, name, Nil, Some(tp), Some(df), None))
            }

            // spec rules
            for { (rule, i) <- spec_rule.rules.map(content.import_term(_, env = term_env)).zipWithIndex } {
              val name = LocalName(Spec_Rules.Rules(i + 1))
              spec_thy.add(Constant(spec_thy.toTerm, name, Nil, Some(Bootstrap.Ded(rule)), None, None))
            }

            controller.add(new NestedModule(thy.toTerm, spec_name, spec_thy))
          }
          catch {
            case isabelle.ERROR(msg) => isabelle.error(msg + "\nin spec rule " + isabelle.quote(spec_rule.name))
          }
        }
      }

      // information about recursion: after all types have been exported
      for {
        segment <- thy_export.segments
        spec_rule <- segment.spec_rules
        (type_name, predicate) <-
          spec_rule.rough_classification match {
            case isabelle.Export_Theory.Equational(isabelle.Export_Theory.Primrec(List(t))) =>
              Some((t, Ontology.ULO.inductive_on))
            case isabelle.Export_Theory.Equational(isabelle.Export_Theory.Primcorec(List(t))) =>
              Some((t, Ontology.ULO.coinductive_for))
            case _ => None
          }
        const_name <-
          spec_rule.terms.map(p => p._1.head) match {
            case List(isabelle.Term.Const(c, _)) => Some(thy_draft.content.get_const(c))
            case _ => None
          }
      } {
        thy_draft.rdf_triple(
          Ontology.binary(const_name.global, predicate,
            thy_draft.content.get_type(type_name).global))
      }

      // primitive defs: after all axioms have been exported
      for (constdef <- thy_export.constdefs) {
        val const_name = thy_draft.content.get_const(constdef.name)
        val axiom_name = thy_draft.content.get_axiom(constdef.axiom_name)
        thy_draft.rdf_triple(Ontology.binary(axiom_name.global, Ontology.ULO.defines, const_name.global))
      }
      for (typedef <- thy_export.typedefs) {
        val type_name = thy_draft.content.get_type(typedef.name)
        val axiom_name = thy_draft.content.get_axiom(typedef.axiom_name)
        thy_draft.rdf_triple(Ontology.binary(axiom_name.global, Ontology.ULO.defines, type_name.global))
      }

      // RDF document
      {
        val path = thy_archive.archive_rdf_path.ext("xz")
        isabelle.Isabelle_System.make_directory(path.dir)
        isabelle.File.write_xz(path,
          isabelle.XML.header +
          isabelle.XML.string_of_tree(thy_draft.rdf_document))
      }

      thy_draft.end_theory()

      MMT_Importer.importDocument(thy_archive.archive, doc)
    }

    Isabelle.import_session(import_theory)
  }


  /* Isabelle tool wrapper */

  val isabelle_tool =
    isabelle.Isabelle_Tool("mmt_import", "import Isabelle sessions into MMT",
      isabelle.Scala_Project.here,
      args => {
        var archive_dirs: List[isabelle.Path] = Nil
        var chapter_archive_map: Map[String, String] = Map.empty
        var chapter_archive_default = ""
        var base_sessions: List[String] = Nil
        var select_dirs: List[isabelle.Path] = Nil
        var requirements = false
        var exclude_session_groups: List[String] = Nil
        var all_sessions = false
        var logic = isabelle.Dump.default_logic
        var dirs: List[isabelle.Path] = Nil
        var session_groups: List[String] = Nil
        var options = isabelle.Options.init()
        var verbose = false
        var exclude_sessions: List[String] = Nil

        val getopts = isabelle.Getopts("""
Usage: isabelle mmt_import [OPTIONS] [SESSIONS ...]

  Options are:
    -A DIR       add archive directory
    -C CH=AR     add mapping of chapter CH to archive AR, or default "_=AR"
    -B NAME      include session NAME and all descendants
    -D DIR       include session directory and select its sessions
    -R           operate on requirements of selected sessions
    -X NAME      exclude sessions from group NAME and all descendants
    -a           select all sessions
    -b NAME      base logic image (default """ + isabelle.quote(isabelle.Dump.default_logic) + """)
    -d DIR       include session directory
    -g NAME      select session group NAME
    -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)
    -v           verbose mode
    -x NAME      exclude session NAME and all descendants

  Import specified sessions into MMT archive directories.
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
        "R" -> (_ => requirements = true),
        "X:" -> (arg => exclude_session_groups = exclude_session_groups ::: List(arg)),
        "a" -> (_ => all_sessions = true),
        "b:" -> (arg => logic = arg),
        "d:" -> (arg => { dirs = dirs ::: List(isabelle.Path.explode(arg)) }),
        "g:" -> (arg => session_groups = session_groups ::: List(arg)),
        "o:" -> (arg => { options += arg }),
        "v" -> (_ => verbose = true),
        "x:" -> (arg =>
          if (arg == isabelle.Thy_Header.PURE) isabelle.error("Cannot exclude " + isabelle.quote(arg))
          else exclude_sessions = exclude_sessions ::: List(arg)))

        val sessions = getopts(args)

        val selection =
          isabelle.Sessions.Selection(
            requirements = requirements,
            all_sessions = all_sessions,
            base_sessions = base_sessions,
            exclude_session_groups = exclude_session_groups,
            exclude_sessions = exclude_sessions,
            session_groups = session_groups,
            sessions = isabelle.Thy_Header.PURE :: sessions)

        val progress =
          new isabelle.Console_Progress(verbose = verbose) {
            override def theory(theory: isabelle.Progress.Theory): Unit =
              if (verbose) echo("Processing " + theory.print_theory + theory.print_percentage)
          }

        def chapter_archive(ch: String): Option[String] =
          chapter_archive_map.get(ch) orElse isabelle.proper_string(chapter_archive_default)

        val state = new State(options)

        val context = isabelle.Dump.Context(
          options
            .real.update("headless_consolidate_delay", options.real("mmt_consolidate_delay"))
            .real.update("headless_prune_delay", options.real("mmt_prune_delay"))
            .real.update("headless_check_delay", options.real("mmt_check_delay"))
            .real.update("headless_watchdog_timeout", options.real("mmt_watchdog_timeout"))
            .real.update("headless_commit_cleanup_delay", options.real("mmt_commit_cleanup_delay"))
            .real.update("headless_load_limit", options.real("mmt_load_limit"))
            .bool.update("export_standard_proofs", options.bool("mmt_proof_terms")),
          aspects = isabelle.Dump.known_aspects, pure_base = true,
          progress = progress, dirs = dirs, select_dirs = select_dirs, selection = selection)

        context.build_logic(logic)

        val start_date = isabelle.Date.now()
        if (verbose) progress.echo("Started at " + isabelle.Build_Log.print_date(start_date) + "\n")
        try {
          for (session <- context.sessions(logic)) {
            importer(state, session, archive_dirs, chapter_archive)
          }
          context.check_errors
        }
        finally {
          val end_date = isabelle.Date.now()
          progress.echo("Finished import of\n" + state.report_imported)
          if (verbose) progress.echo("\nFinished at " + isabelle.Build_Log.print_date(end_date))
          progress.echo((end_date.time - start_date.time).message_hms + " elapsed time")
        }
      }
    )



  /** Isabelle session **/

  class Isabelle(
    state: State,
    session: isabelle.Dump.Session,
    archives: List[Archive],
    chapter_archive: String => Option[String]
  ) {
    private val options = session.context.options
    private val progress = session.context.progress
    private val resources = session.resources


    /* Isabelle + AFP library info */

    private val isabelle_sessions: Set[String] =
      isabelle.Sessions.load_structure(options, select_dirs = List(isabelle.Path.explode("$ISABELLE_HOME"))).
        selection(isabelle.Sessions.Selection.empty).imports_graph.keys.toSet

    private val optional_afp: Option[isabelle.AFP] = {
      if (isabelle.Isabelle_System.getenv("AFP_BASE").isEmpty) None
      else Some(isabelle.AFP.init(options))
    }


    /* theories */

    def import_name(s: String): isabelle.Document.Node.Name =
      resources.import_name(isabelle.Sessions.DRAFT, "", s)

    def theory_qualifier(name: isabelle.Document.Node.Name): String =
      resources.session_base.theory_qualifier(name)

    sealed case class Theory_Archive(
      archive: Archive, source_path: isabelle.Path, rdf_path: isabelle.Path, doc_path: DPath
    ) {
      def archive_root: isabelle.Path =
        isabelle.File.path(archive.root.toJava)

      def archive_source_uri: URI = archive.narrationBase / source_path.implode
      def archive_source_path: isabelle.Path = archive_root + source_path

      def archive_rdf_path: isabelle.Path = archive_root + rdf_path
    }

    def theory_archive(name: isabelle.Document.Node.Name): Theory_Archive = {
      def err(msg: String): Nothing =
        isabelle.error(msg + " (theory " + isabelle.quote(name.toString) + ")")

      val session_name = theory_qualifier(name)
      val session_info =
        session.context.deps.sessions_structure.get(session_name) getOrElse
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
          isabelle.Path.basic("source") +
          isabelle.Path.explode(subdir.getOrElse("")) +
          isabelle.Path.basic(session_name) +
          isabelle.Path.basic(name.theory).ext("theory")

      val rdf_path =
        isabelle.Path.basic("rdf") +
          isabelle.Path.explode(subdir.getOrElse("")) +
          isabelle.Path.basic(session_name) +
          isabelle.Path.basic(name.theory).ext("rdf")

      val doc_path =
        DPath(archive.narrationBase / subdir.toList / session_name / name.theory_base_name)

      Theory_Archive(archive, source_path, rdf_path, doc_path)
    }

    def make_theory(theory: String): Theory = {
      val name = import_name(theory)
      assert(name.theory == theory)

      val archive = theory_archive(name).archive
      val module = DPath(archive.narrationBase) ? theory
      Theory.empty(module.doc, module.name, None)
    }


    /* import session theories */

    session.used_theories.foreach(theory_archive)

    def import_session(import_theory: Theory_Export => Unit): Unit = {
      if (session.context.process_theory(pure_name.theory)) {
        import_theory(pure_theory_export)
      }
      if (session.used_theories.nonEmpty) {
        session.process(
          unicode_symbols = true,
          process_theory = (args: isabelle.Dump.Args) => {
            val snapshot = args.snapshot
            val rendering = new isabelle.Rendering(snapshot, options, args.session)
            import_theory(read_theory_export(rendering))
          })
      }
    }


    /* Pure theory */

    def pure_name: isabelle.Document.Node.Name = import_name(isabelle.Thy_Header.PURE)

    lazy val pure_path: MPath = make_theory(isabelle.Thy_Header.PURE).path

    lazy val pure_theory: isabelle.Export_Theory.Theory =
      isabelle.Export_Theory.read_pure_theory(state.store, cache = state.cache)

    def pure_theory_export: Theory_Export = {
      val segment =
        Theory_Segment(
          classes = pure_theory.classes,
          types =
            for {
              decl <- pure_theory.types
              if decl.entity.name != isabelle.Pure_Thy.DUMMY &&
                decl.entity.name != isabelle.Pure_Thy.FUN &&
                decl.entity.name != isabelle.Pure_Thy.PROP
            } yield decl,
          consts = pure_theory.consts,
          axioms = pure_theory.axioms,
          thms = pure_theory.thms,
          locales = pure_theory.locales,
          locale_dependencies = pure_theory.locale_dependencies)
      Theory_Export(pure_name, segments = List(segment),
        constdefs = pure_theory.constdefs, typedefs = pure_theory.typedefs)
    }

    private def pure_entity(entities: List[isabelle.Export_Theory.Entity], name: String): GlobalName =
      entities.collectFirst(
        { case entity if entity.name == name =>
            Item.Name(
              theory_path = pure_path,
              entity_kind = entity.kind.toString,
              entity_name = entity.name).global
        }).getOrElse(isabelle.error("Unknown entity " + isabelle.quote(name)))

    def pure_type(name: String): GlobalName = pure_entity(pure_theory.types.map(_.entity), name)
    def pure_const(name: String): GlobalName = pure_entity(pure_theory.consts.map(_.entity), name)

    object Class {
      def apply(): Term = lf.Arrow(Bootstrap.Type(), Bootstrap.Prop())
    }

    object All {
      lazy val path: GlobalName = pure_const(isabelle.Pure_Thy.ALL)
      def apply(x: String, tp: Term, body: Term): Term =
        lf.Apply(OMS(path), lf.Lambda(LocalName(x), tp, body))
    }

    object Imp {
      lazy val path: GlobalName = pure_const(isabelle.Pure_Thy.IMP)
      def apply(t: Term, u: Term): Term = lf.ApplySpine(OMS(path), t, u)
    }

    object Eq {
      lazy val path: GlobalName = pure_const(isabelle.Pure_Thy.EQ)
      def apply(tp: Term, t: Term, u: Term): Term = lf.ApplySpine(OMS(path), tp, t, u)
    }


    /* user theories */

    private def commands_range(
      snapshot: isabelle.Document.Snapshot,
      first: isabelle.Command,
      last: isabelle.Command
    ): isabelle.Text.Range = {
      val start = snapshot.node.command_start(first).get
      val stop = snapshot.node.command_start(last).get + last.span.length
      isabelle.Text.Range(start, stop)
    }

    private def element_meta_data(
      rendering: isabelle.Rendering,
      element: isabelle.Thy_Element.Element_Command
    ): isabelle.Properties.T = {
      val element_range = commands_range(rendering.snapshot, element.head, element.last)
      isabelle.RDF.meta_data(rendering.meta_data(element_range))
    }

    private def session_meta_data(name: String): isabelle.Properties.T = {
      if (isabelle_sessions(name)) List(isabelle.RDF.Property.license -> "BSD")
      else {
        (for { afp <- optional_afp; entry <- afp.sessions_map.get(name) }
          yield entry.rdf_meta_data) getOrElse Nil
      }
    }

    private def rdf_author_info(entry: isabelle.Properties.Entry): Option[isabelle.Properties.Entry] = {
      val (a, b) = entry
      if (a == isabelle.RDF.Property.creator || a == isabelle.RDF.Property.contributor) {
        Some(a -> isabelle.AFP.trim_mail(b))
      }
      else if (a == isabelle.RDF.Property.license) Some(entry)
      else None
    }

    def read_theory_export(rendering: isabelle.Rendering): Theory_Export = {
      val snapshot = rendering.snapshot
      val node_name = snapshot.node_name
      val theory_name = node_name.theory

      val theory =
        isabelle.Export_Theory.read_theory(isabelle.Export.Provider.snapshot(snapshot),
          isabelle.Sessions.DRAFT, theory_name)

      val syntax = resources.session_base.node_syntax(snapshot.version.nodes, node_name)

      def document_command(element: isabelle.Thy_Element.Element_Command): Boolean =
        isabelle.Document_Structure.is_document_command(syntax.keywords, element.head)

      def diag_command(element: isabelle.Thy_Element.Element_Command): Boolean =
        isabelle.Document_Structure.is_diag_command(syntax.keywords, element.head)

      val node_timing =
        isabelle.Document_Status.Overall_Timing.make(
          snapshot.state, snapshot.version, snapshot.node.commands)

      val node_elements =
        isabelle.Thy_Element.parse_elements(syntax.keywords, snapshot.node.commands.toList)

      val theory_session_meta_data =
        session_meta_data(theory_qualifier(node_name)).flatMap(rdf_author_info)

      val theory_meta_data =
        node_elements.find(element => element.head.span.name == isabelle.Thy_Header.THEORY) match {
          case Some(element) => element_meta_data(rendering, element)
          case None => Nil
        }

      val segments = {
        val messages_enabled = options.bool("mmt_messages")

        val relevant_elements =
          node_elements.filter(element =>
              document_command(element) ||
              messages_enabled && diag_command(element) ||
              element.head.span.is_kind(syntax.keywords, isabelle.Keyword.theory, false))

        val relevant_ids =
          (for { element <- relevant_elements.iterator; cmd <- element.outline_iterator }
            yield cmd.id).toSet

        val node_command_ids = snapshot.command_id_map

        var heading_count = 0
        var proof_count = 0

        for (element <- relevant_elements)
        yield {
          val element_timing =
            isabelle.Document_Status.Overall_Timing.make(
              snapshot.state, snapshot.version, element.iterator.toList)

          val element_range = commands_range(rendering.snapshot, element.head, element.last)

          val document_tags = rendering.document_tags(element_range)

          val meta_data = element_meta_data(rendering, element)

          val heading =
            if (isabelle.Document_Structure.is_heading_command(element.head)) {
              heading_count += 1
              Some(heading_count)
            }
            else None

          val proof =
            element.proof_start match {
              case Some(first) =>
                proof_count += 1
                val proof_range = commands_range(snapshot, first, element.last)
                val proof_timing =
                  isabelle.Document_Status.Overall_Timing.make(
                    snapshot.state, snapshot.version, element.iterator.toList.tail)
                Some(Proof_Text(proof_count, proof_range, proof_timing))
              case None => None
            }

          val messages =
            if (messages_enabled) {
              isabelle.Rendering.output_messages(snapshot.command_results(element_range))
                .map(isabelle.Protocol.message_text(_))
            }
            else Nil

          def defined_id(print: String, id: Option[Long]): Boolean = {
            def for_msg: String =
              " for " + print + " in theory " + isabelle.quote(theory_name)

            val entity_id =
              id match {
                case Some(i) => i
                case None => isabelle.error("Missing command id" + for_msg)
              }
            val entity_command =
              node_command_ids.get(entity_id) match {
                case Some(cmd) if relevant_ids(cmd.id) => cmd
                case _ =>
                  val msg = "No command with suitable id" + for_msg
                  snapshot.state.lookup_id(entity_id) match {
                    case None => isabelle.error(msg)
                    case Some(st) =>
                      val command = st.command
                      val line_pos =
                        snapshot.find_command_position(command.id, 0) match {
                          case None => ""
                          case Some(node_pos) => " (line " + node_pos.line1 + ")"
                        }
                      isabelle.error(msg + " -- it refers to command " +
                        isabelle.Symbol.cartouche_decoded(st.command.source) + " in " +
                        isabelle.quote(st.command.node_name.node) + line_pos)
                  }
              }
            element.outline_iterator.exists(cmd => cmd.id == entity_command.id)
          }
          def defined(entity: isabelle.Export_Theory.Entity): Boolean =
            defined_id(entity.toString, entity.id)

          Theory_Segment(
            element = element,
            element_timing = element_timing,
            command_kind = syntax.keywords.kinds.get(element.head.span.name),
            document_tags = document_tags,
            document_command = document_command(element),
            meta_data = meta_data,
            heading = heading,
            proof = proof,
            messages = messages,
            classes = for (decl <- theory.classes if defined(decl.entity)) yield decl,
            types = for (decl <- theory.types if defined(decl.entity)) yield decl,
            consts = for (decl <- theory.consts if defined(decl.entity)) yield decl,
            axioms = for (decl <- theory.axioms if defined(decl.entity)) yield decl,
            thms = for (decl <- theory.thms if defined(decl.entity)) yield decl,
            locales = for (decl <- theory.locales if defined(decl.entity)) yield decl,
            locale_dependencies =
              for (decl <- theory.locale_dependencies if defined(decl.entity)) yield decl,
            datatypes = theory.datatypes.filter(datatype => defined_id(datatype.name, datatype.id)),
            spec_rules =
              theory.spec_rules.filter(spec_rule =>
                spec_rule.name.nonEmpty && defined_id(spec_rule.name, spec_rule.id)))
        }
      }

      Theory_Export(node_name,
        node_source = Source(snapshot.node.source),
        node_timing = node_timing,
        node_meta_data = isabelle.Library.distinct(theory_session_meta_data ::: theory_meta_data),
        parents = theory.parents,
        segments = segments,
        constdefs = theory.constdefs,
        typedefs = theory.typedefs)
    }


    /* management of imported content */

    def begin_theory(thy_export: Theory_Export, thy_source: Option[URI]): Theory_Draft =
      new Theory_Draft(thy_export, thy_source)

    class Theory_Draft private[Isabelle](thy_export: Theory_Export, thy_source: Option[URI]) {
      private val node_name = thy_export.node_name
      private val node_source = thy_export.node_source

      val thy: Theory = make_theory(node_name.theory)
      for (uri <- thy_source) SourceRef.update(thy, SourceRef(uri, SourceRegion.none))

      private val _state =
        isabelle.Synchronized[(Content, List[isabelle.RDF.Triple])](
          Content.merge(thy_export.parents.map(state.theory_content)), Nil)

      def content: Content = _state.value._1
      def end_content: Content = {
        val (content, triples) =_state.value
        content + (node_name.theory -> Triples_Stats.make(triples))
      }

      def rdf_document: isabelle.XML.Elem = Ontology.rdf_document(_state.value._2.reverse)

      def rdf_triple(triple: isabelle.RDF.Triple): Unit =
        _state.change({ case (content, triples) => (content, triple :: triples) })

      def declare_item(item: Item, tags: List[String], props: isabelle.Properties.T): Unit = {
        _state.change(
          { case (content, triples) =>
              val content1 = content.declare(node_name.theory, item.name)
              val name = Ontology.binary(item.name.global, Ontology.ULO.name, item.entity_xname)
              val specs =
                List(
                  Ontology.binary(thy.path, Ontology.ULO.specifies, item.name.global),
                  Ontology.binary(item.name.global, Ontology.ULO.specified_in, thy.path))
              val source_ref =
                item.source_ref.map(sref =>
                    Ontology.binary(item.name.global, Ontology.ULO.sourceref, sref.toURI))
              val important =
                tags.reverse.collectFirst({
                  case isabelle.Markup.Document_Tag.IMPORTANT => true
                  case isabelle.Markup.Document_Tag.UNIMPORTANT => false
                }).toList.map(b =>
                  Ontology.unary(item.name.global, if (b) Ontology.ULO.important else Ontology.ULO.unimportant))
              val properties = props.map({ case (a, b) => Ontology.binary(item.name.global, a, b) })

              val triples1 = name :: specs ::: source_ref.toList ::: important ::: properties.reverse ::: triples
              (content1, triples1)
          })
      }

      def make_item(entity: isabelle.Export_Theory.Entity,
        syntax: isabelle.Export_Theory.Syntax = isabelle.Export_Theory.No_Syntax,
        type_scheme: (List[String], isabelle.Term.Typ) = dummy_type_scheme
      ): Item = {
        Item(
          thy.path,
          entity.kind.toString,
          entity.name,
          entity_xname = entity.xname,
          entity_pos = entity.pos,
          syntax = syntax,
          type_scheme = type_scheme,
          theory_source = thy_source,
          node_source = node_source)
      }

      def declare_entity(
        entity: isabelle.Export_Theory.Entity,
        tags: List[String],
        props: isabelle.Properties.T
      ): Item = {
        val item = make_item(entity)
        declare_item(item, tags, props)
        item
      }

      def make_basic_item(name: String, kind: String, pos: isabelle.Position.T): Item =
        Item(thy.path, kind, name, entity_pos = pos, theory_source = thy_source, node_source = node_source)

      def make_datatype_item(datatype: isabelle.Export_Theory.Datatype): Item = {
        val kind = if (datatype.co) Datatypes.Kind.CODATATYPE else Datatypes.Kind.DATATYPE
        make_basic_item(datatype.name, kind.toString, datatype.pos)
      }

      def make_spec_item(spec_rule: isabelle.Export_Theory.Spec_Rule): Item = {
        val kind =
          spec_rule.rough_classification match {
            case isabelle.Export_Theory.Equational(isabelle.Export_Theory.Unknown_Recursion) =>
              Spec_Rules.Kind.DEFINITION
            case isabelle.Export_Theory.Equational(_) =>
              Spec_Rules.Kind.RECURSIVE_DEFINITION
            case isabelle.Export_Theory.Inductive =>
              Spec_Rules.Kind.INDUCTIVE_DEFINITION
            case isabelle.Export_Theory.Co_Inductive =>
              Spec_Rules.Kind.COINDUCTIVE_DEFINITION
            case isabelle.Export_Theory.Unknown =>
              Spec_Rules.Kind.SPECIFICATION
          }
        make_basic_item(spec_rule.name, kind.toString, spec_rule.pos)
      }

      def end_theory(): Unit = state.end_theory(node_name.theory, end_content)
    }
  }
}
