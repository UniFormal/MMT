package info.kwarc.mmt.api.ontology.rdf

import info.kwarc.mmt.api.archives.{Archive, relational}
import info.kwarc.mmt.api.{DPath, DeclarationComponent, ElaborationOf, GeneratedMRef, Original, Path, StructuralElement}
import info.kwarc.mmt.api.documents.{Document, InterpretationInstruction, NRef}
import info.kwarc.mmt.api.frontend.{Controller, Extension}
import info.kwarc.mmt.api.modules.{Link, Module, Theory, View}
import info.kwarc.mmt.api.objects.{OMPMOD, TheoryExp}
import info.kwarc.mmt.api.ontology.{Choice, Declares, HasType, Individual, IsConstant, IsDocument, IsUntypedConstant, Reflexive, Relation, RelationExp, RelationalElement, Sequence, ToObject, ToSubject, Transitive}
import info.kwarc.mmt.api.ontology.rdf.ULO.{ULOElem, mmt_uri}
import info.kwarc.mmt.api.opaque.OpaqueElement
import info.kwarc.mmt.api.parser.{FileInArchiveSource, SourceInfo, SourceRef}
import info.kwarc.mmt.api.patterns.{Instance, Pattern}
import info.kwarc.mmt.api.symbols.{Constant, DerivedDeclaration, NestedModule, ObjContainer, RuleConstant, Structure}
import info.kwarc.mmt.api.utils.{File, FilePath, URI}
import org.apache.log4j.{BasicConfigurator, LogManager}
import org.eclipse.rdf4j.model.vocabulary.RDF
import org.eclipse.rdf4j.model.{IRI, Resource, Statement, Value}
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
  val memory_uri = DPath(ULO.mmt_uri.uri colon "memory")
}

class Database extends Extension {
  override def logPrefix: String = "database"
  BasicConfigurator.configure()
  LogManager.getRootLogger.setLevel(org.apache.log4j.Level.ERROR)

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

  override def start(args: List[String]): Unit = {
    super.start(args)
    connection.add(ULO.model)
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
        addStatement(ULO.library(archive.ulo_uri))(Some(ULO.mmt_uri))
      case List(a, _) =>
        val group = DPath(ULO.mmt_uri.uri colon a)
        addStatement(ULO.library_group(group))(Some(ULO.mmt_uri))
        addStatement(ULO.library(archive.ulo_uri))(Some(ULO.mmt_uri))
        addStatement(ULO.contains(group, archive.ulo_uri))(Some(ULO.mmt_uri))
    }
    if ((archive / relational).isDirectory) (archive / relational).descendants.filter(_.getExtension.contains("rdf")).foreach { f =>
      val path = ULO.URLEscape(relpath(archive, f))
      val apath = ULO.URLEscape(archive.ulo_uri)
      connection.add(apath, ULO.contains.toIri, path, apath)
      try {
        connection.add(f.toJava, apath)
      } catch {
        case _: RDFParseException =>
      }
    }
  }

  def add(e : StructuralElement,si : SourceInfo) : Unit = Threadstack.queue {}

  def add(e : StructuralElement)(implicit context : Option[Path] = None) : Unit = Threadstack.queue {
    remove(e.path)
    val statements = getStatements(e)
    statements.foreach(addStatement(_))
    e.getOrigin match {
      case Original =>
      case ElaborationOf(source) =>
        addStatement(ULO.generated_by(e.path,source))
      case _ =>
        print("")
    }
  }

  case class GetResult(it : RepositoryResult[Statement]) {
    import scala.jdk.CollectionConverters._

    def toIterator = it.asScala
    def foreach(f : Statement => Unit) = toIterator.foreach(f)

  }

  def get(s : Option[Path] = None,p : Option[ULOElem] = None, o : Option[Path] = None,inferred : Boolean = true,context : Option[Path] = None)
  = Threadstack.await {
    GetResult(connection.getStatements(s.map(ULO.URLEscape(_)).orNull,p.map(_.toIri).orNull,o.map(ULO.URLEscape(_)).orNull,inferred,context.map(ULO.URLEscape(_)).toList:_*))
  }
  def getInds(cls : ULO.Class, context : Option[Path] = None)
  = Threadstack.await {
    GetResult(connection.getStatements(null,RDF.TYPE,cls.toIri,true,context.map(ULO.URLEscape(_)).toList:_*))
  }

  private def remove(p : Path) = try { // TODO
    val deletes = query(p,Transitive(Declares | ULO.contains.toBinary | Reflexive))
    deletes.foreach(s => get(s = Some(s)).foreach(connection.remove(_)))
  } catch {
    case t : Throwable =>
      throw t
  }

  def removeAll = Threadstack.queue {
    get().foreach(connection.remove(_))
    connection.add(ULO.model)
  }


  def add(e : StructuralElement,a : Archive,f : FilePath) : Unit = Threadstack.queue {
    remove(e.path)
    var statements = getStatements(e)
    e.getOrigin match {
      case Original =>
      case ElaborationOf(source) =>
        statements ::= ULO.generated_by(e.path,source)
      case _ =>
        print("")
    }

    val file = f.segments.foldLeft(a.ulo_uri)((p, d) => p / d)
    statements ::= ULO.contains(a.ulo_uri, file)
    statements ::= ULO.declares(file, e.path)

    val outFile = a / relational / f.setExtension("rdf")
    outFile.up.mkdirs()
    val stream = new FileOutputStream(outFile)
    val writer = Rio.createWriter(RDFFormat.RDFXML, stream)
    try {
      writer.startRDF()
      statements.foreach{s =>
        addStatement(s)(Some(a.ulo_uri))
        s.triples.foreach{case (s,p,o) =>
          writer.handleStatement(new Statement {
            override def getSubject: Resource = s
            override def getPredicate: IRI = p
            override def getObject: Value = o
            override def getContext: Resource = ULO.URLEscape(a.ulo_uri)
          })
        }
      }
      writer.endRDF()
    } finally {stream.close()}
  }

  def addRel(rel : RelationalElement)(implicit context : Option[Path]) : Unit = Threadstack.queue {
    rel.toULO match {
      case Some(v) => addStatement(v)
      case _ =>
        rel match {
          case Individual(path, IsUntypedConstant) => addRel(Individual(path,IsConstant))
          case _ =>
            print("")
        }
    }
  }

  private def addStatement(s : ULOStatement)(implicit context : Option[Path]) : Unit = s.triples.foreach{case (s,p,o) => try {
    context match {
      case Some(path) => connection.add(s,p,o,ULO.URLEscape(path))
      case None => connection.add(s,p,o,ULO.URLEscape(Database.memory_uri))
    }
  } catch {
    case t : Throwable =>
      throw t
  }}

  private def getStatements(e: StructuralElement) : List[ULOStatement] = {
    implicit val elem : StructuralElement = e
    val path = e.path
    var ret : List[ULOStatement] = Nil
    SourceRef.get(e) match {
      case Some(sr) =>
        ret ::= ULO.sourceref(path,sr)
      case _ =>
    }
    e match {
      case d: Document =>
        ret ::= ULO.document(d.path)
        //TODO should use getLocalItems but then it wouldn't work for documents created from folders
        d.getDeclarations.foreach {
            case inner: Document =>
              ret :::= ULO.declares(path,inner.path) :: getStatements(inner)
            case oe: OpaqueElement =>
            case nr: NRef =>
              ret ::= ULO.declares(path,nr.target)
              if (nr.getOrigin == GeneratedMRef) controller.getO(nr.target) match {
                case Some(ne : StructuralElement) =>
                  ret :::= getStatements(ne)
                case _ =>
              }
            case _ =>
        }
      case n: NRef =>
      case oe: OpaqueElement =>
      case t: Theory =>
        ret ::= ULO.theory(path)
        t.meta foreach {p => ret ::= ULO.HasMeta(t.path, p)}
      case v: View =>
        ret ::= ULO.ULOView(v.path,v.from.toMPath,v.to.toMPath)
      case _ =>
    }
    e match {
      case t: Module =>
        t.getDeclarations foreach {d => {
          remove(d.path)
          ret ::= ULO.declares(path,d.path)
          d match {
            case c: Constant =>
              ret ::= ULO.constant(path)
              ret ::= ULO.name(path,c.name)
              c.alias foreach {a =>
                ret ::= ULO.name(path,a)
              }
              // extract dependencies - this may make the rel files a lot bigger
              c.getComponents foreach {
                case DeclarationComponent(dim, oc: ObjContainer[_]) => ret :::= doDependencies(c.path $ dim, oc)
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
                  ret ::= ULO.ULOInclude(from,t.path,Some(s.path))
                }
              } else {
                ret ::= ULO.ULOStructure(s.path,from,TheoryExp.simplify(s.to).toMPath)
              }
            case rc: RuleConstant =>
              ret ::= ULO.rule_constant(path)
            case dd: DerivedDeclaration =>
              dd.feature match {
                case Pattern.feature => ret ::= ULO.pattern(path)
                case Instance.feature =>
                  ret ::= ULO.instance(path)
                  dd.tpC.get match {
                    case Some(Instance.Type(p,_)) => ret ::= ULO.instance_of(path,p)
                    case _ =>
                  }
                case _ =>
                  ret ::= ULO.derived_declaration(path)
              }
            case nm: NestedModule =>
              ret ::= ULO.nested_module(nm.module.path)
              ret ::= ULO.declares(nm.home.toMPath,path)
              ret :::= getStatements(nm.module)
          }
        }}
      case _ =>
    }
    e match {
      case l: Link if l.isImplicit =>
        ret ::= ULO.implicit_morphism(path)
      case _ =>
    }
    ret
  }
  /** extract all dependencies of object containers */
  private def doDependencies(path: Path, oc: ObjContainer[_]) = {
    oc.dependsOn map {p => ULO.depends_on(path,p) }
  }.toList

  def query(start : Path, q : RelationExp) = Threadstack.await {
    val qv = SparqlBuilder.`var`("qv")
    val p = makeQuery(q)
    val query = Queries.SELECT(qv).where(GraphPatterns.tp(qv,p,ULO.URLEscape(start)))
    import scala.jdk.CollectionConverters._
    val values = connection.prepareTupleQuery(query.getQueryString).evaluate().iterator().asScala.toList.map {
      _.getBinding("qv").getValue
    }
    values.map(v => ULO.URLEscape.unapply(v.toString))
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

}