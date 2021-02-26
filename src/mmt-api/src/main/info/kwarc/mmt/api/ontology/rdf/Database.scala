package info.kwarc.mmt.api.ontology.rdf

import info.kwarc.mmt.api.archives.{Archive, relational}
import info.kwarc.mmt.api.{DPath, DeclarationComponent, ElaborationOf, GeneratedMRef, Original, Path, StructuralElement}
import info.kwarc.mmt.api.documents.{Document, InterpretationInstruction, NRef}
import info.kwarc.mmt.api.frontend.{Controller, Extension}
import info.kwarc.mmt.api.modules.{Link, Module, Theory, View}
import info.kwarc.mmt.api.objects.{OMPMOD, TheoryExp}
import info.kwarc.mmt.api.ontology.{Choice, Declares, HasType, Individual, IsConstant, IsDocument, IsUntypedConstant, Reflexive, Relation, RelationExp, RelationalElement, Sequence, ToObject, ToSubject, Transitive}
import info.kwarc.mmt.api.ontology.rdf.ULO.ULOElem
import info.kwarc.mmt.api.opaque.OpaqueElement
import info.kwarc.mmt.api.parser.{FileInArchiveSource, SourceInfo, SourceRef}
import info.kwarc.mmt.api.patterns.{Instance, Pattern}
import info.kwarc.mmt.api.symbols.{Constant, DerivedDeclaration, NestedModule, ObjContainer, RuleConstant, Structure}
import info.kwarc.mmt.api.utils.{File, FilePath, URI}
import org.apache.log4j.{BasicConfigurator, LogManager}
import org.eclipse.rdf4j.model.vocabulary.RDF
import org.eclipse.rdf4j.model.{Resource, Statement}
import org.eclipse.rdf4j.query.QueryResults
import org.eclipse.rdf4j.repository.RepositoryResult
import org.eclipse.rdf4j.repository.sail.SailRepository
import org.eclipse.rdf4j.rio.helpers.ParseErrorLogger
import org.eclipse.rdf4j.rio.{RDFFormat, RDFParseException, Rio}
import org.eclipse.rdf4j.sail.memory.MemoryStore
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries
import org.eclipse.rdf4j.sparqlbuilder.core.{PropertyPaths, QueryElement, SparqlBuilder, Variable}
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.{GraphPattern, GraphPatterns}
import org.eclipse.rdf4j.sparqlbuilder.rdf.RdfPredicate

import java.io.FileOutputStream
import scala.collection.mutable
import scala.collection.mutable.HashSet
import scala.concurrent.Future

object Database {
  def get(controller : Controller) = controller.extman.get(classOf[Database]).head
}

class Database extends Extension {
  override def logPrefix: String = "database"
  BasicConfigurator.configure()
  import scala.jdk.CollectionConverters._
  LogManager.getRootLogger.setLevel(org.apache.log4j.Level.ERROR)
  import org.eclipse.rdf4j.model.util.Values.iri

  object Threadstack {
    class Thread(fun : Unit => Any) {
      def execute = {
        val ret = fun(())
        this.synchronized{value = Some(ret)}
      }
      private var value : Option[Any] = None
      def get = {
        while (this.synchronized{value.isEmpty}) {
          Thread.sleep(100)
        }
        value.get
      }
    }
    import scala.concurrent.ExecutionContext.Implicits._
    private val threads : mutable.Queue[Thread] = mutable.Queue.empty

    def busy = threads.synchronized{threads.nonEmpty}
    def queue[A](f : => A) : Unit = threads.synchronized{threads.enqueue(new Thread({_ => f}))}
    def await[A](f : => A) = {
      val t = new Thread({_ => f})
      if (threads.synchronized(threads.nonEmpty)) log("Database busy. Waiting...")
      threads.synchronized{threads.enqueue(t)}
      t.get.asInstanceOf[A]
    }

    Future {
      while (true) {
        if (threads.synchronized {threads.isEmpty}) Thread.sleep(100)
        else {
          val f = threads.synchronized{threads.dequeue()}
          f.execute
        }
      }
    }
  }

  private lazy val repo = new SailRepository(new MemoryStore())
  private lazy val connection = repo.getConnection
  private val memory_uri = DPath(ULO.mmt_uri.uri colon "memory")

  override def start(args: List[String]): Unit = {
    super.start(args)
    connection.add(ULO.model)
    /*
    controller.backend.getArchives.foreach(_.id.split('/').toList match {
      case List(id) =>
        add(ULO.library(DPath(mmt_uri colon id)),mmt_uri)
      case List(a,b) =>
        val group = DPath(mmt_uri colon a)
        val arch = DPath(mmt_uri colon (a + "." + b))
        add(ULO.library_group(group),mmt_uri)
        add(ULO.library(arch),mmt_uri)
        add(ULO.contains(group,arch),mmt_uri)
    })
     */
  }

  def relpath(archive : Archive,f : File) = {
    val sub = (archive / relational).relativize(f).toFilePath
    val furi = if (sub.segments.head.contains("..")) {
      var cap = false
      val name = sub.stripExtension.name.flatMap {
        case '$' => cap = true
          ""
        case o if cap => cap = false
          o.toUpper.toString
        case o =>
          o.toString
      }
      Path.parse(sub.head.replace("..", "://") + sub.tail.init.mkString("/","/","") + "?" + name)
    } else sub.setExtension("omdoc").segments.foldLeft(archive.ulo_uri)((u, s) => u / s)
    furi
  }

  def add(archive : Archive): Unit = Threadstack.queue {
    log("Adding archive " + archive.id)
    archive.id.split('/').toList match {
      case List(id) =>
        add(ULO.library(archive.ulo_uri),ULO.mmt_uri)
      case List(a,b) =>
        val group = DPath(ULO.mmt_uri.uri colon a)
        add(ULO.library_group(group),ULO.mmt_uri)
        add(ULO.library(archive.ulo_uri),ULO.mmt_uri)
        add(ULO.contains(group,archive.ulo_uri),ULO.mmt_uri)
    }
    if ((archive/relational).isDirectory) (archive / relational).descendants.filter(_.getExtension.contains("rdf")).foreach {f =>
        try {connection.add(f.toJava, ULO.URLEscape(archive.ulo_uri), ULO.URLEscape(relpath(archive,f))) } catch {
          case _ : RDFParseException =>
        }
    }
  }

  def add(e : StructuralElement,si : SourceInfo) : Unit = Threadstack.queue {} /* si match {
    case FileInArchiveSource(a, f) =>
      import info.kwarc.mmt.api.archives._
      val uri = DPath(mmt_uri colon a.id.replace('/', '.'))
      val segments = if ((a / content) <= f) SourceRef.get(e) match {
        case Some(sr) if sr.container.toString.startsWith(a.narrationBase.toString) =>
          print("")
          sr.container.toString.drop(a.narrationBase.toString.length+1).split('/').toList
        case None =>
          return ()
        case _ =>
          ???
      } else {
        a.root.relativize(f).segments.tail
      }
      val file = segments.foldLeft(uri)((p, d) => p / d)
      add(ULO.contains(uri, file), uri.uri)
      add(ULO.declares(file, e.path), uri.uri)
      addElement(e)(uri.uri)
    case _ =>
      print("")
  } */

  def add(e : StructuralElement,context : Path*) : Unit = Threadstack.queue {
    remove(e.path)
    if (context.isEmpty) addElement(e)(memory_uri,e.path)
    else addElement(e)(context :_*)
    e.getOrigin match {
      case Original =>
      case ElaborationOf(source) =>
        add(ULO.generated_by(e.path,source))
      case _ =>
        print("")
    }
  }

  case class GetResult(it : RepositoryResult[Statement]) {
    import scala.jdk.CollectionConverters._

    def toIterator = it.asScala
    def foreach(f : Statement => Unit) = toIterator.foreach(f)

  }

  def get(s : Option[Path] = None,p : Option[ULOElem] = None, o : Option[Path] = None,inferred : Boolean = true)(context : Path*) = Threadstack.await {
    GetResult(connection.getStatements(s.map(ULO.URLEscape(_)).orNull,p.map(_.toIri).orNull,o.map(ULO.URLEscape(_)).orNull,inferred,context.map(ULO.URLEscape(_)):_*))
  }
  def getInds(cls : ULO.Class)(context : Path*) = Threadstack.await {
    GetResult(connection.getStatements(null,RDF.TYPE,cls.toIri,true,context.map(ULO.URLEscape(_)):_*))
  }

  def remove(p : Path) = Threadstack.queue { try {
    get()(p).foreach(connection.remove(_))
  } catch {
    case t : Throwable =>
      throw t
  } }

  def removeAll = Threadstack.queue {
    get()().foreach(connection.remove(_))
    connection.add(ULO.model)
  }


  def add(e : StructuralElement,a : Archive,f : FilePath) : Unit = Threadstack.queue {
    val uri = DPath(ULO.mmt_uri.uri colon a.id.replace('/', '.'))
    val file = f.segments.foldLeft(uri)((p, d) => p / d)
    add(ULO.contains(uri, file), uri)
    add(ULO.declares(file, e.path), uri)
    add(e,uri,file,e.path)
    val outFile = a / relational / f.setExtension("rdf")
    outFile.up.mkdirs()
    val stream = new FileOutputStream(outFile)
    val writer = Rio.createWriter(RDFFormat.RDFXML, stream)
    try {
      writer.startRDF()
      get(inferred = false)(e.path).foreach(writer.handleStatement)
      writer.endRDF()
    } finally {stream.close()}
  }

  def add(rel : RelationalElement, context : Path*) : Unit = Threadstack.queue {
    rel.toULO match {
      case Some(v) => add(v,context :_*)
      case _ =>
        rel match {
          case Individual(path, IsUntypedConstant) => add(Individual(path,IsConstant),context:_*)
          case _ =>
            print("")
        }
    }
  }

  private def add(s : ULOStatement,context : Path*) : Unit = s.triples.foreach{case (s,p,o) => try {
    connection.add(s,p,o,context.map(u => ULO.URLEscape(u)):_*)
  } catch {
    case t : Throwable =>
      throw t
  }}

  private def addElement(e: StructuralElement)(implicit context : Path*): Unit = {
    implicit val elem : StructuralElement = e
    val path = e.path
    SourceRef.get(e) match {
      case Some(sr) =>
        add(ULO.sourceref(path,sr),context:_*)
      case _ =>
    }
    e match {
      case d: Document =>
        add(ULO.document(d.path),context:_*)
        //TODO should use getLocalItems but then it wouldn't work for documents created from folders
        d.getDeclarations.foreach {
            case inner: Document =>
              add(inner,inner.path +: context :_*)
              add(ULO.declares(path,inner.path),context :_*)
            case oe: OpaqueElement =>
            case nr: NRef =>
              add(ULO.declares(path,nr.target),context :_*)
              if (nr.getOrigin == GeneratedMRef) controller.getO(nr.target) match {
                case Some(ne : StructuralElement) =>
                  add(ne,ne.path +: context :_*)
                case _ =>
              }
            case _ =>
        }
      case n: NRef =>
      case oe: OpaqueElement =>
      case t: Theory =>
        add(ULO.theory(path),context :_*)
        t.meta foreach {p => add(ULO.HasMeta(t.path, p),context :_*)}
      case v: View =>
        add(ULO.ULOView(v.path,v.from.toMPath,v.to.toMPath),context:_*)
      case _ =>
    }
    e match {
      case t: Module =>
        t.getDeclarations foreach {d => {
          remove(d.path)
          add(ULO.declares(path,d.path),context:_*)
          d match {
            case c: Constant =>
              add(ULO.constant(path),c.path +: context :_*)
              add(ULO.name(path,c.name),c.path +: context :_*)
              c.alias foreach {a =>
                add(ULO.name(path,a),c.path +: context :_*)
              }
              // extract dependencies - this may make the rel files a lot bigger
              c.getComponents foreach {
                case DeclarationComponent(dim, oc: ObjContainer[_]) => doDependencies(c.path $ dim, oc)(c.path +: context :_*)
                case _ =>
              }
            case s: Structure =>
              val from = s.from match {
                case OMPMOD(p,_) => p
                case f => TheoryExp.simplify(s.from).toMPath
              }
              if (s.isInclude) {
                if (!s.isGenerated) {
                  // we do not export transitive includes
                  add(ULO.ULOInclude(from,t.path,Some(s.path)),context :_*)
                }
              } else {
                add(ULO.ULOStructure(s.path,from,TheoryExp.simplify(s.to).toMPath),context :_*)
              }
            case rc: RuleConstant =>
              add(ULO.rule_constant(path),context :_*)
            case dd: DerivedDeclaration =>
              dd.feature match {
                case Pattern.feature => add(ULO.pattern(path),context :_*)
                case Instance.feature =>
                  add(ULO.instance(path),context :_*)
                  dd.tpC.get match {
                    case Some(Instance.Type(p,_)) => add(ULO.instance_of(path,p),context :_*)
                    case _ =>
                  }
                case _ =>
                  add(ULO.derived_declaration(path),context :_*)
              }
            case nm: NestedModule =>
              add(ULO.nested_module(nm.module.path),context :_*)
              add(ULO.declares(nm.home.toMPath,path),context :_*)
              add(nm.module,nm.module.path +: context :_*)
          }
        }}
      case _ =>
    }
    e match {
      case l: Link if l.isImplicit =>
        add(ULO.implicit_morphism(path),context :_*)
      case _ =>
    }
  }
  /** extract all dependencies of object containers */
  private def doDependencies(path: Path, oc: ObjContainer[_])(context : Path*) {
    oc.dependsOn foreach {p =>
      val r = ULO.depends_on(path,p)
      add(r,context :_*)
    }
  }

  def query(start : Path, q : RelationExp) = Threadstack.await {
    val qv = SparqlBuilder.`var`("qv")
    val p = makeQuery(q)
    val query = Queries.SELECT(qv).where(GraphPatterns.tp(qv,p,ULO.URLEscape(start)))
    import scala.jdk.CollectionConverters._
    connection.prepareTupleQuery(query.getQueryString).evaluate().iterator().asScala.toList.map {
      _.getBinding("qv").getValue
    }
  }

  private def makeQuery(q : RelationExp) : RdfPredicate = q match {
    case ToSubject(dep) =>
      dep.toULO match {
        case Some(ulo) =>
          () => "<" + ulo.toIri + ">"
        case _ =>
          ???
      }
    case ToObject(dep) =>
      dep.toULO match {
        case Some(ulo) =>
          () => "^<" + ulo.toIri + ">"
        case _ =>
          ???
      }
    case Choice(qs@_*) if qs.contains(Reflexive) =>
      val nqs = qs.filterNot(_ == Reflexive)
      () => "(" + makeQuery(Choice(nqs:_*)).getQueryString + ")?"
    case Choice(qs@_*) =>
      () => "(" + qs.map(makeQuery(_).getQueryString).mkString("|") + ")"
    case Transitive(q) =>
      () => "(" + makeQuery(q).getQueryString + ")+"
    case _ =>
      ???
  }
/*
  private def makeQuery(start : Path, q : RelationExp) : Query = q match {
    case ToObject(d) => objects(start, d).foreach(add)   //all paths related to start via d
    case ToSubject(d) => subjects(d, start).foreach(add) //all paths inversely related to start via d
    //only start itself
    case Reflexive => add(start)
    //the set of paths related to start via arbitrarily many q-steps (depth-first, children before parent)
    case Transitive(qn) =>
      var added = HashSet.empty[Path]
      def step(p : Path) {
        if (! added.contains(p)) {
          //println("Added path "+p.toString()+" as a path related to the starting path "+start.toString()+" with search query "+q.toString())
          added += p
          val next = makeQuery(p, qn)(step)
          add(p) //add parent only after children
        }
      }
      val next = makeQuery(start, qn)(step)
    //the set of paths related to start by any of the relations in qs (in the order listed)
    case Choice(qs @ _*) => qs.foreach(q => makeQuery(start, q))
    //the set of paths related to start by making steps according to qs
    case Sequence(qs @ _*) => qs.toList match {
      case Nil => add(start)
      case hd :: tl => makeQuery(start, hd) {p => makeQuery(p, Sequence(tl : _*))}
    }
    //only start itself iff it has the right type
    case HasType(mustOpt,mustNot) =>
      val tpO = getType(start)
      val inMust = mustOpt match {
        case None => true
        case Some(must) => tpO exists {must contains _}
      }
      lazy val notInMustNot = tpO.exists(tp => ! (mustNot contains tp))
      if (inMust && notInMustNot)
        add(start)
  }

 */

}