package info.kwarc.mmt.isabelle

import scala.math.Ordering
import scala.collection.SortedMap

import info.kwarc.mmt.lf
import info.kwarc.mmt.api._
import archives._
import symbols._
import modules._
import documents._
import objects._
import opaque._
import utils._
import parser._


object Importer
{
  /* names */

  /*common namespace for all theories in all sessions in all Isabelle archives*/
  val library_base: DPath = DPath(URI("https", "isabelle.in.tum.de") / "Isabelle")

  def declared_theory(node_name: isabelle.Document.Node.Name): DeclaredTheory =
  {
    val mod = library_base ? node_name.theory
    Theory.empty(mod.doc, mod.name, Some(mod))
  }

  val suppress_types: Set[String] = Set(isabelle.Pure_Thy.DUMMY, isabelle.Pure_Thy.FUN)


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
    def get(key: Item.Key): Item = rep.get(key) getOrElse isabelle.error("Undeclared " + key.toString)
    def get_type(name: String): Item = get(Item.Key(isabelle.Export_Theory.Kind.TYPE, name))
    def get_const(name: String): Item = get(Item.Key(isabelle.Export_Theory.Kind.CONST, name))

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
}

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


    /* theory exports (foundational order) */

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


    /* imported items (foundational order) */

    for ((thy_name, theory) <- thy_exports) {
      // items
      var items = Isabelle.begin_theory(theory)

      def declare_item(entity: isabelle.Export_Theory.Entity, type_scheme: (List[String], isabelle.Term.Typ))
        : Importer.Item =
      {
        val item = Importer.Item(thy_name, entity, type_scheme)
        items = items.declare(item)
        item
      }

      // document
      val doc = new Document(DPath(bt.base / theory.name), root = true)
      controller.add(doc)

      val thy = Importer.declared_theory(thy_name)
      controller.add(thy)
      controller.add(MRef(doc.path, thy.path))

      // theory source
      if (thy_name != Isabelle.pure_name) {
        val thy_text = isabelle.File.read(thy_name.path)
        val thy_text_output = isabelle.Symbol.decode(thy_text.replace(' ', '\u00a0'))
        controller.add(new OpaqueText(thy.asDocument.path, OpaqueText.defaultFormat, StringFragment(thy_text_output)))
      }

      def decl_error(msg: String, entity: isabelle.Export_Theory.Entity): Nothing =
        isabelle.error(msg + "\nin declaration of " + entity + isabelle.Position.here(entity.pos))

      // types
      for (decl <- theory.types if !Importer.suppress_types(decl.entity.name)) {
        try {
          val item = declare_item(decl.entity, Importer.dummy_type_scheme)
          val tp = Isabelle.Type(decl.args.length)
          val df = decl.abbrev.map(rhs => Isabelle.Type.abs(decl.args, Isabelle.import_type(items, rhs)))
          controller.add(item.constant(Some(tp), df))
        }
        catch { case isabelle.ERROR(msg) => decl_error(msg, decl.entity) }
      }

      // consts
      for (decl <- theory.consts) {
        try {
          val item = declare_item(decl.entity, (decl.typargs, decl.typ))
          val tp = Isabelle.Type.all(decl.typargs, Isabelle.import_type(items, decl.typ))
          val df = decl.abbrev.map(rhs => Isabelle.Type.abs(decl.typargs, Isabelle.import_term(items, rhs)))
          controller.add(item.constant(Some(tp), df))
        }
        catch { case isabelle.ERROR(msg) => decl_error(msg, decl.entity) }
      }

      // facts
      for (decl_multi <- theory.facts; decl <- decl_multi.split) {
        try {
          val item = declare_item(decl.entity, Importer.dummy_type_scheme)
          val tp = Isabelle.import_prop(items, decl.prop)
          controller.add(item.constant(Some(tp), None))
        }
        catch { case isabelle.ERROR(msg) => decl_error(msg, decl.entity) }
      }

      Isabelle.end_theory(theory, items)
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


  /* session */

  private var _session: Option[isabelle.Thy_Resources.Session] = None

  def session: isabelle.Thy_Resources.Session =
    _session.getOrElse(isabelle.error("No Isabelle/PIDE session"))

  def resources: isabelle.Thy_Resources = session.resources

  def import_name(s: String): isabelle.Document.Node.Name =
    resources.import_name(isabelle.Sessions.DRAFT, "", s)

  def PURE: String = isabelle.Thy_Header.PURE
  def pure_name: isabelle.Document.Node.Name = import_name(PURE)

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


  /* Pure theory */

  lazy val pure_theory: isabelle.Export_Theory.Theory =
    isabelle.Export_Theory.read_pure_theory(store, cache = Some(cache))

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

  def read_theory(provider: isabelle.Export.Provider, name: isabelle.Document.Node.Name)
    : isabelle.Export_Theory.Theory =
  {
    isabelle.Export_Theory.read_theory(
      provider, isabelle.Sessions.DRAFT, name.theory, cache = Some(cache))
  }

  def use_theories(theories: List[String]): isabelle.Thy_Resources.Theories_Result =
    session.use_theories(theories, progress = progress)


  /* imported theory items */

  private val imported = isabelle.Synchronized(Map.empty[String, Importer.Items])

  def the_theory(name: String): Importer.Items =
    imported.value.getOrElse(name, isabelle.error("Unknown theory " + isabelle.quote(name)))

  def begin_theory(theory: isabelle.Export_Theory.Theory): Importer.Items =
    Importer.Items.merge(theory.parents.map(the_theory(_)))

  def end_theory(theory: isabelle.Export_Theory.Theory, items: Importer.Items): Unit =
    imported.change(map => map + (theory.name -> items))

  def import_type(items: Importer.Items, typ: isabelle.Term.Typ): Term =
  {
    try {
      typ match {
        case isabelle.Term.Type(isabelle.Pure_Thy.FUN, List(a, b)) =>
          lf.Arrow(import_type(items, a), import_type(items, b))
        case isabelle.Term.Type(name, args) =>
          val op = OMS(items.get_type(name).global_name)
          if (args.isEmpty) op else OMA(lf.Apply.term, op :: args.map(import_type(items, _)))
        case isabelle.Term.TFree(a, _) => OMV(a)
        case isabelle.Term.TVar(xi, _) => isabelle.error("Illegal schematic type variable " + xi.toString)
      }
    }
    catch { case isabelle.ERROR(msg) => isabelle.error(msg + "\nin type " + typ) }
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

  def import_prop(items: Importer.Items, prop: isabelle.Export_Theory.Prop): Term =
  {
    val as = prop.typargs.map(_._1)  // FIXME handle sort constraints
    val xs = prop.args.map({ case (x, ty) => OMV(x) % import_type(items, ty) })
    val t = import_term(items, prop.term)
    Type.all(as, if (xs.isEmpty) t else lf.Pi(xs, t))
  }
}
