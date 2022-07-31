package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api.archives.{Archive, relational}
import info.kwarc.mmt.api.documents.{Document, NRef}
import info.kwarc.mmt.api.frontend.Extension
import info.kwarc.mmt.api.modules.{Link, Theory, View}
import info.kwarc.mmt.api.objects.{OMPMOD, TheoryExp}
import info.kwarc.mmt.api.opaque.OpaqueElement
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.patterns.{Instance, Pattern}
import info.kwarc.mmt.api.symbols.{Constant, DerivedDeclaration, NestedModule, ObjContainer, RuleConstant, Structure}
import info.kwarc.mmt.api.utils.{File, FilePath, URI}
import info.kwarc.mmt.api.{ComplexStep, DPath, DeclarationComponent, ElaborationOf, GeneratedMRef, GlobalName, MPath, Original, Path, StructuralElement}
import org.eclipse.rdf4j.model.vocabulary.{DC, OWL, RDF, RDFS, XSD}
import org.eclipse.rdf4j.model.{IRI, Resource, Statement, Value}
import org.eclipse.rdf4j.model.util.Values.iri

import java.io.FileOutputStream
import java.net.{URLDecoder, URLEncoder}

/**
  * To encode MMT-URIs as RDF-conform strings
  */
object ULOURLEscape {
  def unapply(s: String): Path = try {
    val ns = URLDecoder.decode(s,"UTF-8")
    Path.parse(ns)
  } catch {
    case t: Throwable =>
      print("")
      throw t
  }
  def toIri(p : Path) = iri(apply(p))
  def apply(p:Path) : String = URLEncoder.encode(p.toPath,"UTF-8").replaceFirst("%3A",":")
}

/**
  * Abstraction for (subject,predicate,object) triples *and* their implied triples
  */
trait ULOStatement {
  def triples : Seq[(Resource,IRI,Value)]
}
case class SimpleStatement(resource : Resource, pred: IRI, value: Value) extends ULOStatement {
  def triples = Seq((resource,pred,value))
}

/**
  * Abstraction for subjects/predicates/objects, including sequences. New ones should basically only be
  * constructed here, because their initialization adds them to the ULO model immediately
  */
trait ULO {
  def toIri : IRI
  protected def doAdd(p : IRI,v:Value): this.type = {
    ULOModel.add(this,p,v)
    this
  }
  import ULO._
  private[ontology] def inverseOf(p: Resource*): this.type = doAdd(OWL.INVERSEOF,sequence(p))
  private[ontology] def domain(p: Resource*): this.type = doAdd(RDFS.DOMAIN,sequence(p))
  private[ontology] def range(p: Resource*): this.type = doAdd(RDFS.RANGE,sequence(p))
  private[ontology] def comment(s: String): this.type = doAdd(RDFS.COMMENT,org.eclipse.rdf4j.model.util.Values.literal(s))
  private[ontology] def _type(p: Resource*): this.type = doAdd(RDF.TYPE,sequence(p))
  private[ontology] def isDisjointUnionOf(p: Resource*): this.type = doAdd(OWL.DISJOINTUNIONOF,sequence(p))
  private[ontology] def isIntersectionOf(p: Resource*): this.type = doAdd(OWL.INTERSECTIONOF,sequence(p))
  private[ontology] def subclassOf(p: Resource*): this.type = doAdd(RDFS.SUBCLASSOF,sequence(p))
  private[ontology] def complementOf(p: Resource*): this.type = doAdd(OWL.COMPLEMENTOF,sequence(p))
}
object ULO {
  private var index = 0
  private[ontology] def sequence(rls:Seq[Resource]) : Value = rls match {
    case Seq() => RDF.NIL
    case Seq(a) => a
    case _ =>
      def inner(ls:Seq[Resource], i : Int) : Value = ls match {
        case Seq() => RDF.NIL
        case _ =>
          val name = ulo ## ("helper-" + index.toString + "-" + i)
          ULOModel.add(name,RDF.FIRST,ls.head)
          ULOModel.add(name,RDF.REST,inner(ls.tail,i+1))
          name
      }
      val r = inner(rls,0)
      index += 1
      r
  }
  val namespace = "http://mathhub.info/ulo"
  val ulo = new ULO {
    override def toIri: IRI = iri(namespace)
    def ##(s : String) = iri(namespace + "#" + s)
  }
  implicit def iriToIri(i : ULO) : Resource = i.toIri
}

import ULO._

class ULOElem(val name: String) extends ULO {
  override def toIri: IRI = ulo ## name
  ULOModel.toULO(toIri.toString) = this
}
class ObjectProperty(name : String, binary:Option[Binary] = None) extends ULOElem(name) {
  _type(OWL.OBJECTPROPERTY)

  private[ontology] def transitive(): this.type = _type(OWL.TRANSITIVEPROPERTY)
  private[ontology] def symmetric(): this.type = _type(OWL.SYMMETRICPROPERTY)
  private[ontology] def asymmetric(): this.type = _type(OWL.ASYMMETRICPROPERTY)
  private[ontology] def subpropertyOf(p: Resource*): this.type = doAdd(RDFS.SUBPROPERTYOF,sequence(p))
  private[ontology] def disjointWith(p: Resource*): this.type = doAdd(OWL.PROPERTYDISJOINTWITH,sequence(p))

  def apply(s: Path, o: Path) = SimpleStatement(ULOURLEscape.toIri(s), this.toIri, ULOURLEscape.toIri(o))
  def toBinary = binary.getOrElse(CustomBinary(name,"",""))
}
class DatatypeProperty(name : String, binary:Option[Binary] = None) extends ULOElem(name) {
  _type(OWL.DATATYPEPROPERTY)

  private[ontology] def subpropertyOf(p: Resource*): this.type = doAdd(RDFS.SUBPROPERTYOF, sequence(p))
  private[ontology] def transitive(): this.type = _type(OWL.TRANSITIVEPROPERTY)
  private[ontology] def symmetric(): this.type = _type(OWL.SYMMETRICPROPERTY)
  private[ontology] def asymmetric(): this.type = _type(OWL.ASYMMETRICPROPERTY)

  def apply(s: Path, o: Any) = SimpleStatement(ULOURLEscape.toIri(s), this.toIri,o match {
    case p : Path => ULOURLEscape.toIri(p)
    case _ => iri(o.toString)
  })
  def toBinary = binary.getOrElse(CustomBinary(name, "", ""))
}
class ULOClass(name : String, unary:Option[Unary] = None) extends ULOElem(name) {
  _type(OWL.CLASS)

  private[ontology] def disjointWith(p: Resource*): this.type = doAdd(OWL.DISJOINTWITH, sequence(p))

  def apply(p: Path) = SimpleStatement(ULOURLEscape.toIri(p), RDF.TYPE, this)
  def toUnary = unary.getOrElse(CustomUnary(name))
}

/**
  * The Upper Library Ontology as an RDF model
  */
object ULOModel {
  import org.eclipse.rdf4j.model.impl.TreeModel
  val model = new TreeModel()

  private def error = ???

  model.setNamespace("ulo",namespace)

  private[ontology] def add(r : Resource, i: IRI, v: Value) = model.add(r,i,v,ulo)
  add(ulo,DC.RIGHTS,org.eclipse.rdf4j.model.util.Values.literal(
    "This ontology is licensed under the CC-BY-SA license."
  ))


  // General

  val ncname = XSD.NCNAME
  val thing = OWL.THING
  val organizational = new DatatypeProperty("organizational")

  // Physical Storage

  val physical = new ULOClass("physical")
    .comment("An organizational unit for the physical organization of mathematical knowledge into documents or document collections.")
  val file = new ULOClass("file")
    .subclassOf(physical)
    .comment("A document in a file system.")
  val folder = new ULOClass("folder")
    .subclassOf(physical)
    .comment("A grouping of files and other folder, i.e. above the document level.")
  val library = new ULOClass("library")
    .subclassOf(physical)
    .comment("A grouping of mathematical documents. Usually in the form of a repository.")
  val library_group = new ULOClass("library-group")
    .subclassOf(physical)
    .comment("A group of libraries, usually on a repository server like GitHub.")
  val para = new ULOClass("para")
    .subclassOf(physical)
    .comment("A document paragraph with mathematical meaning.")
  val phrase = new ULOClass("phrase")
    .subclassOf(physical)
    .comment("Phrasal structures in mathematical texts and formulae, these include symbols, declarations, and quantifications.")
  val section = new ULOClass("section")
    .subclassOf(physical)
    .comment("A physical grouping inside a document. These can be nested.")

  val definition = new ULOClass("definition")
    .subclassOf(para)
    .comment("A logical paragraph that defines a new concept.")
  val example = new ULOClass("example")
    .subclassOf(para)
    .comment("A logical paragraph that introduces a mathematical example.")
  val proof = new ULOClass("proof")
    .subclassOf(para)
    .comment("A logical paragraph that serves as a justification of a proposition.")
  // Note - this means that proofs are purely "physical" things, not terms
  // as in MMT
  val proposition = new ULOClass("proposition")
    .subclassOf(para)
    .comment("A statement of a mathematical object or some relation between some.")


  // Logical Elements

  val logical = new ULOClass("logical")
    .comment("A logical classification of mathematical knowledge items.")

  val primitive = new ULOClass("primitive")
    .subclassOf(logical)
    .comment("This knowledge item does not have a definition in terms of (more) primitive items.")
  val derived = new ULOClass("derived")
    .subclassOf(logical)
    .disjointWith(primitive)
    .complementOf(primitive)
  add(thing, OWL.DISJOINTUNIONOF, sequence(Seq(derived, primitive)))

  val theory = new ULOClass("theory")
    .subclassOf(logical)
    .comment("A semantically meaningful block of declarations that can be referred to globally. Examples include MMT theories, Mizar articles, Isabelle locales and Coq sections.")

  val declaration = new ULOClass("declaration")
    .subclassOf(logical)
    //  .disjointWith(theory) <- this excludes nested modules
    .comment("Declarations are named objects. They can also have a type and a definiens.")
  val statement = new ULOClass("statement")
    .subclassOf(declaration)
    .comment("Statements are declarations of objects that can in principle have proofs.")
  val axiom = new ULOClass("axiom")
    .isIntersectionOf(primitive, statement)
    .subclassOf(statement)
    .comment("Logically (using the Curry-Howard isomorphism), an axiom is a primitive statement, i.e. a declaration without a definiens.")
  // ^ This implies that "primitive"/"derived" do not conform to MMTs
  // conception of the terms - e.g. a derived declaration can not ever elaborate
  // into axioms, in that sense
  val function = new ULOClass("function")
    .subclassOf(declaration)
    .comment("Functions that construct objects, possibly from other objects, for example in first-order logic the successor function.")
  // Notably, according to https://kwarc.info/people/mkohlhase/papers/cicm19-ulo.pdf,
  // constants are also ontologically functions.
  val _type = new ULOClass("type")
    .subclassOf(declaration)
    .comment("Types divide their universe into named subsets.")
  val predicate = new ULOClass("predicate")
    .subclassOf(declaration)
    .comment("A predicate is a mathematical object that evaluates to true/false when applied to enough arguments.")

  val rule = new ULOClass("rule")
    .subclassOf(statement)
    .comment("Rules are statements that can be used for computation, e.g. theorems that can be used for simplification.")
  val theorem = new ULOClass("theorem")
    .subclassOf(statement)
    .isIntersectionOf(derived, statement)
    .comment("Logically (using the Curry-Howard isomorphism), a theorem is a derived statement, i.e. a declaration with a definiens (this is the proof of the theorem given in the type)")
  val typedec = new ULOClass("typedec")
    .subclassOf(declaration)
    .comment("A logical paragraph that introduces a type.")
  val universe = new ULOClass("universe")
    .subclassOf(declaration)
    .comment("A universe declaration, used e.g. in strong logics like Coq.")


  // Relations between physical and logical

  val specifies = new ObjectProperty("specifies")
    .domain(physical)
    .domain(logical)
    .comment("The physical organizational item S specifies a knowledge item O, i.e. S is represented in O.")
  val specified_in = new ObjectProperty("specified_in")
    .inverseOf(specifies)
    .domain(logical)
    .range(physical)


  // Cross References and inter-statements

  val crossrefs = new ObjectProperty("crossrefs")
  val aligned_with = new ObjectProperty("aligned-with")
    .subpropertyOf(crossrefs)
    .symmetric()
  val alternative_for = new ObjectProperty("alternative-for")
    .subpropertyOf(crossrefs)
  val inspired_by = new ObjectProperty("inspired-by")
    .subpropertyOf(crossrefs)
  val same_as = new ObjectProperty("same-as")
    .subpropertyOf(crossrefs)
    .symmetric()
  val see_also = new ObjectProperty("see-also")
    .subpropertyOf(crossrefs)
  val similar_to = new ObjectProperty("similar-to")
    .subpropertyOf(crossrefs)
    .symmetric()

  val inter_statement = new ObjectProperty("inter-statement")
  val constructs = new ObjectProperty("constructs")
    .subpropertyOf(inter_statement)
    .comment("S is a constructor for an inductive type or predicate O")
  val example_for = new ObjectProperty("example-for")
    .subpropertyOf(inter_statement)
  val contains = new ObjectProperty("contains")
  val counter_example_for = new ObjectProperty("counter-example-for")
    .subpropertyOf(inter_statement)
    .disjointWith(example_for)
  val defines = new ObjectProperty("defines")
    .subpropertyOf(inter_statement)
    .domain(thing)
    .range(function)
    .comment("A definition defines various objects.")
  val generated_by = new ObjectProperty("generated-by")
    .subpropertyOf(inter_statement)
    .domain(function)
    .range(function)
  val inductive_on = new ObjectProperty("inductive-on")
    .subpropertyOf(inter_statement)
  val justifies = new ObjectProperty("justifies")
    .subpropertyOf(inter_statement)
    .domain(proof)


  // Linguistic relations (-nyms)

  val nyms = new ObjectProperty("nyms")
  val antonym = new ObjectProperty("antonym")
    .subpropertyOf(nyms)
  val hyponym = new ObjectProperty("hyponym")
    .subpropertyOf(nyms)
  val hypernym = new ObjectProperty("hypernym")
    .subpropertyOf(nyms)
    .inverseOf(hyponym)


  // Others

  val size_properties = new DatatypeProperty("size-properties")
    ._type(OWL.FUNCTIONALPROPERTY)
    .domain(thing)
  val formalizes = new ObjectProperty("formalizes")
  val uses = new ObjectProperty("uses",Some(DependsOn))
    .transitive()
    .domain(statement)
    .range(function)
    .range(_type)
  val implementation_uses = new ObjectProperty("implementation-uses")
    .subpropertyOf(uses)
  val uses_implementation = new ObjectProperty("uses-implementation")
    .subpropertyOf(uses)
  val uses_interface = new ObjectProperty("uses-interface")
    .subpropertyOf(uses)
  val implementation_uses_implementation_of = new ObjectProperty("implementation-uses-implementation-of")
    .subpropertyOf(implementation_uses)
    .subpropertyOf(uses_implementation)
  val implementation_uses_interface_of = new ObjectProperty("implementation-uses-interface-of")
    .subpropertyOf(implementation_uses)
    .subpropertyOf(uses_interface)
  val instance_of = new ObjectProperty("instance-of")
    .comment("S is an instance of O iff it is a model of O, iniherits from O, interprets O, etc.")
  val interface_uses = new ObjectProperty("interface-uses")
    .subpropertyOf(uses)
  val interface_uses_implementation_of = new ObjectProperty("interface-uses-implementation-of")
    .subpropertyOf(interface_uses)
    .subpropertyOf(uses_implementation)
  val interface_uses_interface_of = new ObjectProperty("interface-uses-interface-of")
    .subpropertyOf(interface_uses)
    .subpropertyOf(uses_interface)
    .comment("A proof can justify a theorem or a definition.")
  val superseded_by = new ObjectProperty("superseded_by")
    .transitive()
    .comment("S (a deprecated knowledge item) is superseded by another.")

  val action_times = new DatatypeProperty("action-times")
  val automatically_proved = new DatatypeProperty("automatically-proved")
    .subpropertyOf(organizational)
    .range(XSD.STRING)
    .comment("S is automatically proven by a theorem prover, O is an explanatory string.")
  val check_time = new DatatypeProperty("check-time")
    .subpropertyOf(size_properties)
    .domain(function)
    .domain(_type)
    .range(XSD.NON_NEGATIVE_INTEGER)
    .comment("time (a natural number giving a time in milliseconds) it took to check the declaration that introduced the subject.")
  val deprecated = new DatatypeProperty("deprecated")
    .subpropertyOf(organizational)
    .range(XSD.STRING)
    .comment("S is deprecated (do not use any longer), O is an explanatory string.")
  val docref = new DatatypeProperty("docref")
    .comment("A URI reference to a place where this knowledge item is documented (usually in some read-only rich text format).")
  val experimental = new DatatypeProperty("experimental")
    .subpropertyOf(organizational)
    .range(XSD.STRING)
  val external_size = new DatatypeProperty("external-size")
    .subpropertyOf(size_properties)
    .domain(thing)
    .range(XSD.NON_NEGATIVE_INTEGER)
    .comment("The number of characters (not counting whitespace or comments) in the source code of the subject. This number can be approximate.")
  val important = new DatatypeProperty("important")
    .subpropertyOf(organizational)
    .comment("S is important (to someone); O is an explanatory string.")
  val internal_size = new DatatypeProperty("internal-size")
    .subpropertyOf(size_properties)
    .domain(thing)
    .range(XSD.NON_NEGATIVE_INTEGER)
    .comment("The number of bytes in the internal representation of the subject including any inferred objects and generated proofs. This number can be approximate.")
  val last_checked_at = new DatatypeProperty("last-checked-at")
    .subpropertyOf(action_times)
    .domain(function)
    .domain(_type)
    .range(XSD.DATETIMESTAMP)
    .comment("The time stamp of when the subject was last checked.")
  val name = new DatatypeProperty("name")
    .range(XSD.STRING)
    .comment("The name of a knowledge item given by the user.")
  val paratype = new DatatypeProperty("paratype")
    .range(ncname)
  val revision = new DatatypeProperty("revision")
    .domain(library)
  val sourceref = new DatatypeProperty("sourceref")
    .domain(thing)
    .range(XSD.ANYURI)
    .comment("The URI of the physical location (e.g. file/URI, line, column) of the source code that introduced the subject.")
  val unimportant = new DatatypeProperty("unimportant")
    .subpropertyOf(organizational)
    .comment("S is deemed unimportant (by someone); O is an explanatory string.")
  val mutual_block = new ULOClass("mutual-block")
    .subclassOf(theory)
    .comment("A theory where the declarations may mutually refer to each other; examples include mutually recursive functions and types.")


  // MMT specific concepts, not already included in the ULO

  val document = new ULOClass("document",Some(IsDocument))
    .subclassOf(logical)
  val module = new ULOClass("module")
  theory.subclassOf(module)
  val declares = specifies
  val theory_morphism = new ULOClass("theory-morphism")
    .subclassOf(logical)
  val domain = new ObjectProperty("domain")
    .subclassOf(inter_statement)
  val codomain = new ObjectProperty("codomain")
    .subclassOf(inter_statement)
  val implicit_morphism = new ULOClass("implicit-morphism")
    .subclassOf(theory_morphism)
  val meta_theory = new ULOClass("meta-theory")
    .subclassOf(implicit_morphism)
  val view = new ULOClass("view")
    .subclassOf(theory_morphism)
    .subclassOf(module)
  val structure = new ULOClass("structure")
    .subclassOf(theory_morphism)
  val include = new ULOClass("include")
    .subclassOf(structure)
  val constant = new ULOClass("constant")
    .subclassOf(declaration)
  val rule_constant = new ULOClass("rule-constant")
    .subclassOf(constant)
  val depends_on = uses
  val derived_declaration = new ULOClass("derived-declaraion")
    .isIntersectionOf(derived, declaration)
  val pattern = new ULOClass("pattern")
    .subclassOf(derived_declaration)
  val instance = new ULOClass("instance")
    .subclassOf(derived_declaration)
  val nested_module = new ULOClass("nested-module")
    .subclassOf(module)
    .subclassOf(declaration)


  // Helpers for morphisms etc.
  // TODO should be largely replaced by a (Unary,Binary)-pair - the morphism itself is unary and implies the binary
  // relation between domain/codomain

  trait Morphism extends ULOStatement {
    val _domain : MPath
    val _codomain : MPath
    val name : Resource
    val relation : ULOClass
    override def triples: Seq[(Resource, IRI, Value)] =
      Seq(
        (name,RDF.TYPE,relation),
        (name,domain.toIri,ULOURLEscape.toIri(_domain)),
        (name,codomain.toIri,ULOURLEscape.toIri(_codomain)))
  }
  case class HasMeta(_codomain : MPath,_domain : MPath) extends Morphism {
    val name = iri(ULOURLEscape(_codomain) + "#meta-theory")
    val relation = meta_theory
  }
  case class View(path : MPath, _domain : MPath,_codomain : MPath) extends Morphism {
    val name = ULOURLEscape.toIri(path)
    val relation = view
  }
  class ULOStructure(val path : GlobalName, val _domain : MPath, val _codomain : MPath) extends Morphism {
    val name = ULOURLEscape.toIri(path)
    val relation = structure
    override def equals(obj: Any): Boolean = obj match {
      case i : ULOStructure => i._domain == _domain && i._codomain == _codomain && i.path == path
      case _ => false
    }
    override def hashCode(): Int = path.hashCode() + _domain.hashCode() + _codomain.hashCode()
  }
  object Structure{
    def apply(path : GlobalName, _domain : MPath, _codomain : MPath) = new ULOStructure(path,_domain,_codomain)
  }
  object Include {
    def apply( _domain : MPath, _codomain : MPath, _path : Option[GlobalName] = None) = new ULOInclude(_domain,_codomain,_path)
  }
  class ULOInclude(_domain : MPath, _codomain : MPath, _path : Option[GlobalName] = None)
    extends ULOStructure(_path.getOrElse(_codomain ? ComplexStep(_domain)),_domain,_codomain) {
    override val relation = include
    override def equals(obj: Any): Boolean = obj match {
      case i : ULOInclude => i._domain == _domain && i._codomain == _codomain
      case _ => false
    }
    override def hashCode(): Int = _domain.hashCode() + _codomain.hashCode()
  }

  val toULO = scala.collection.mutable.Map.empty[String, ULOElem]

}

/**
  * Used to mark the source of a relational element, so that they are stored in source-specific rdf files
  */
sealed class SourceInfo
case class FileSource(file: File) extends SourceInfo
case class ArchiveFileSource(archive: Archive, file: File) extends SourceInfo
case object StringSource extends SourceInfo


/**
  * The following should replace the relational store, some parts of the library, and the "backend" of the query-mechanism.
  */

object RDFDatabase {
  /**
    * We use URIs to split the database into individual storages (per archive/source file).
    * The following uris are used for in-memory content or "global" (controller/backend) information:
    */
  val mmt_uri = URI.scheme("mmt") // <- mmt-backend knowledge (e.g. archives, groups etc)
  val memory_uri = DPath(mmt_uri colon "memory") // <- in-memory content
  def archive_uri(id : String) = DPath(mmt_uri colon id.replace('/','.'))
}

class RDFDatabase extends Extension {
  import org.eclipse.rdf4j.repository.sail.SailRepository
  import org.eclipse.rdf4j.sail.memory.MemoryStore
  import org.eclipse.rdf4j.repository.RepositoryResult
  import org.eclipse.rdf4j.rio.{RDFFormat, RDFParseException, Rio}
  import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder
  import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries
  import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns
  import org.eclipse.rdf4j.sparqlbuilder.rdf.RdfPredicate
  import RDFDatabase._

  private lazy val repo = new SailRepository(new MemoryStore)
  private lazy val connection = repo.getConnection
  override def logPrefix: String = "rdfdatabase"

  // TODO corresponding converters between TBox/ABox and ULOElements should probably happen in the Tbox/ABox classes
  private def toUlo(b: Binary): Option[ULOElem] = ???
  private def toUlo(b: RelationalElement): Option[ULOStatement] = ???

  private def add(s: ULOStatement, source_uri:URI) = s.triples.foreach{
    case (s,p,o) => connection.add(s,p,o,iri(source_uri.toString))
  }

  override def start(args: List[String]): Unit = {
    super.start(args)
    connection.add(ULOModel.model)
  }

  override def clear = Threads.queue {
    super.clear
    get(context = Some(memory_uri)).foreach(connection.remove(_))
  }

  // All modification and querying of the database should be handled thread-safe. Hence, we use
  // a queue of executables that is processed in order.

  private object Threads {
    val steps = 10
    private class Thread(f : Unit => Any) {
      private var value : Option[Any] = None
      def execute() = {
        val ret = f(())
        this.synchronized{ value = Some(ret)}
      }
      def get() : Any = {
        while (this.synchronized{value.isEmpty}) { Thread.sleep(steps)}
        value.get
      }
    }
    import scala.concurrent.ExecutionContext.Implicits._
    private val threads = scala.collection.mutable.Queue.empty[Thread]
    def busy = threads.synchronized{threads.nonEmpty}
    def queue[A](f : => A) : Unit = threads.synchronized{threads.enqueue(new Thread({_ => f}))}
    def run[A](f: => A) : A = {
      val th = new Thread({_ => f})
      if (busy) log("Database busy. Waiting...")
      threads.synchronized{threads.enqueue(th)}
      th.get().asInstanceOf
    }
    scala.concurrent.Future {
      while (true) {
        if (threads.synchronized{threads.isEmpty}) Thread.sleep(steps) else {
          val f = threads.synchronized{threads.dequeue()}
          f.execute()
        }
      }
    }
  }


  // adding a new archive

  def addArchive(a : Archive) = {
    Threads.queue {
      val segs = a.id.split('/').toList
      val id = archive_uri(a.id)//DPath(mmt_uri colon segs.mkString("."))
      add(ULOModel.library(id), mmt_uri)
      if (segs.length > 1) {
        val inits = segs.inits.toList.init.reverse.map(l => DPath(mmt_uri colon l.mkString("."))) // List(1,2,3) -> 1, 1.2, 1.2.3
        inits.init.zip(inits.tail).foreach { case (p,c) =>
          add(ULOModel.library_group(p),mmt_uri)
          add(ULOModel.contains(p,c),mmt_uri)
        }
      }
    }
    readRelational(a)
  }

  private def relpath(archive: Archive, f: File) = {
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
      Path.parse(sub.head.replace("..", "://") + sub.tail.init.mkString("/", "/", "") + "?" + name)
    } else sub.setExtension("omdoc").segments.foldLeft(archive_uri(archive.id))((u, s) => u / s)
    furi
  }


  // read all relational of an archive

  def readRelational(a : Archive) = {
    val reldir = a / relational
    if (reldir.exists() && reldir.isDirectory) Threads.queue {
      reldir.descendants.filter(_.getExtension.contains("rdf")).foreach { f =>
        val path = ULOURLEscape.toIri(relpath(a, f))
        val apath = ULOURLEscape.toIri(archive_uri(a.id))
        connection.add(apath, ULOModel.contains.toIri, path, apath)
        try {
          connection.add(f.toJava, apath)
        } catch {
          case _: RDFParseException =>
        }
      }
    }
  }

  private def remove(p: Path) = try {
    val deletes = query(p, Transitive(Declares | ULOModel.contains.toBinary | Reflexive))
    deletes.foreach(s => get(subject = Some(s)).foreach(connection.remove(_)))
  } catch {
    case t: Throwable =>
      throw t
  }

  def removeAll = Threads.queue {
    get().foreach(connection.remove(_))
    connection.add(ULOModel.model)
  }

  // adding new structural / relational elements

  def add(e: StructuralElement)(implicit rel_uri: Option[Path] = None): Unit = Threads.run {
    remove(e.path)
    val statements = getStatements(e)
    statements.foreach(addStatement(_))
    e.getOrigin match {
      case Original =>
      case ElaborationOf(source) =>
        addStatement(ULOModel.generated_by(e.path, source))
      case _ =>
        print("")
    }
  }

  // adding new structural / relational elements and writing them to an .rdf file

  def add(e : StructuralElement,a : Archive,f : FilePath) : Unit = Threads.queue {
    remove(e.path)
    var statements = getStatements(e)
    e.getOrigin match {
      case Original =>
      case ElaborationOf(source) =>
        statements ::= ULOModel.generated_by(e.path,source)
      case _ =>
        print("")
    }
    val auri = archive_uri(a.id)

    val file = f.segments.foldLeft(auri)((p, d) => p / d)
    statements ::= ULOModel.contains(auri, file)
    statements ::= ULOModel.declares(file, e.path)

    val outFile = a / relational / f.setExtension("rdf")
    outFile.up.mkdirs()
    val stream = new FileOutputStream(outFile)
    val writer = Rio.createWriter(RDFFormat.RDFXML, stream)
    try {
      writer.startRDF()
      statements.foreach{s =>
        addStatement(s)(Some(auri))
        s.triples.foreach{case (s,p,o) =>
          writer.handleStatement(new Statement {
            override def getSubject: Resource = s
            override def getPredicate: IRI = p
            override def getObject: Value = o
            override def getContext: Resource = ULOURLEscape.toIri(auri)
          })
        }
      }
      writer.endRDF()
    } finally {stream.close()}
  }

  def addRel(rel : RelationalElement)(implicit rel_uri : Option[Path]) : Unit = Threads.queue {
    toUlo(rel) match {
      case Some(v) => addStatement(v)
      case _ =>
        rel match {
          case Individual(path, IsUntypedConstant) => addRel(Individual(path,IsConstant))
          case _ =>
            print("")
        }
    }
  }

  private def addStatement(s : ULOStatement)(implicit rel_uri : Option[Path]) : Unit = s.triples.foreach{case (s,p,o) => try {
    rel_uri match {
      case Some(path) => connection.add(s,p,o,ULOURLEscape.toIri(path))
      case None => connection.add(s,p,o,ULOURLEscape.toIri(memory_uri))
    }
  } catch {
    case t : Throwable =>
      throw t
  }}


  // extracts ULOStatements from structural elements

  private def getStatements(e: StructuralElement) : List[ULOStatement] = {
    implicit val elem : StructuralElement = e
    val path = e.path
    var ret : List[ULOStatement] = Nil
    SourceRef.get(e) match {
      case Some(sr) =>
        ret ::= ULOModel.sourceref(path,sr)
      case _ =>
    }
    e match {
      case d: Document =>
        ret ::= ULOModel.document(d.path)
        //TODO should use getLocalItems but then it wouldn't work for documents created from folders
        d.getDeclarations.foreach {
          case inner: Document =>
            ret :::= ULOModel.declares(path,inner.path) :: getStatements(inner)
          case oe: OpaqueElement =>
          case nr: NRef =>
            ret ::= ULOModel.declares(path,nr.target)
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
        ret ::= ULOModel.theory(path)
        t.meta foreach {p => ret ::= ULOModel.HasMeta(t.path, p)}
      case v: View =>
        ret ::= ULOModel.View(v.path,v.from.toMPath,v.to.toMPath)
      case _ =>
    }
    e match {
      case t: info.kwarc.mmt.api.modules.Module =>
        t.getDeclarations foreach {d => {
          remove(d.path)
          ret ::= ULOModel.declares(path,d.path)
          d match {
            case c: Constant =>
              ret ::= ULOModel.constant(path)
              ret ::= ULOModel.name(path,c.name)
              c.alias foreach {a =>
                ret ::= ULOModel.name(path,a)
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
                  ret ::= ULOModel.Include(from,t.path,Some(s.path))
                }
              } else {
                ret ::= ULOModel.Structure(s.path,from,TheoryExp.simplify(s.to).toMPath)
              }
            case rc: RuleConstant =>
              ret ::= ULOModel.rule_constant(path)
            case dd: DerivedDeclaration =>
              dd.feature match {
                case Pattern.feature => ret ::= ULOModel.pattern(path)
                case Instance.feature =>
                  ret ::= ULOModel.instance(path)
                  dd.tpC.get match {
                    case Some(Instance.Type(p,_)) => ret ::= ULOModel.instance_of(path,p)
                    case _ =>
                  }
                case _ =>
                  ret ::= ULOModel.derived_declaration(path)
              }
            case nm: NestedModule =>
              ret ::= ULOModel.nested_module(nm.module.path)
              ret ::= ULOModel.declares(nm.home.toMPath,path)
              ret :::= getStatements(nm.module)
          }
        }}
      case _ =>
    }
    e match {
      case l: Link if l.isImplicit =>
        ret ::= ULOModel.implicit_morphism(path)
      case _ =>
    }
    ret
  }
  /** extract all dependencies of object containers */
  private def doDependencies(path: Path, oc: ObjContainer[_]) = {
    oc.dependsOn map {p => ULOModel.depends_on(path,p) }
  }.toList



  // Querying

  // Wrapper for query results
  case class GetResult(it: RepositoryResult[Statement]) {

    import scala.jdk.CollectionConverters._

    def toIterator = it.asScala

    def foreach(f: Statement => Unit) = toIterator.foreach(f)

    def toRelational: List[RelationalElement] = toIterator.toList.map { statement =>
      statement.getPredicate match {
        case RDF.TYPE =>
          Individual(
            ULOURLEscape.unapply(statement.getSubject.toString),
            ULOModel.toULO(ULOURLEscape.unapply(statement.getObject.toString).toString) match {
              case cls: ULOClass => cls.toUnary
              case _ =>
                ???
            })
        case pred =>
          Relation(
            ULOModel.toULO(ULOURLEscape.unapply(pred.toString).toString) match {
              case op: ObjectProperty => op.toBinary
              case dp: DatatypeProperty => dp.toBinary
              case _ =>
                ???
            },
            ULOURLEscape.unapply(statement.getSubject.toString),
            ULOURLEscape.unapply(statement.getObject.toString),
          )
      }
    }

  }


  def get(subject: Option[Path] = None, predicate: Option[ULOElem] = None, obj: Option[Path] = None, inferred: Boolean = true, context: Option[Path] = None)
  = Threads.run {
    GetResult(connection.getStatements(
      subject.map(ULOURLEscape.toIri).orNull,
      predicate.map(_.toIri).orNull,
      obj.map(ULOURLEscape.toIri).orNull,
      inferred, context.map(ULOURLEscape.toIri).toList: _*))
  }

  def query(start: Path, q: RelationExp) = Threads.run {
    val qv = SparqlBuilder.`var`("qv")
    val p = makeQuery(q)
    val query = Queries.SELECT(qv).where(GraphPatterns.tp(qv, p, ULOURLEscape.toIri(start)))
    import scala.jdk.CollectionConverters._
    val values = connection.prepareTupleQuery(query.getQueryString).evaluate().iterator().asScala.toList.map {
      _.getBinding("qv").getValue
    }
    values.map(v => ULOURLEscape.unapply(v.toString))
  }

  def getInds(cls: Option[ULOClass] = None): List[Individual]
  = Threads.run {
    import scala.jdk.CollectionConverters._
    val qv = SparqlBuilder.`var`("qv")
    val query = cls match {
      case Some(c) =>
        Queries.SELECT(qv).where(GraphPatterns.tp(qv, RDF.TYPE, c.toIri))
      case None =>
        val qo = SparqlBuilder.`var`("qo")
        Queries.SELECT(qv, qo).where(GraphPatterns.and(
          GraphPatterns.tp(qv, RDF.TYPE, qo),
          GraphPatterns.tp(qo, RDF.TYPE, OWL.CLASS)
        ))
    }
    val values = connection.prepareTupleQuery(query.getQueryString).evaluate().iterator().asScala.toList.map { s =>
      cls match {
        case Some(c) => Individual(ULOURLEscape.unapply(s.getBinding("qv").getValue.toString), c.toUnary)
        case None if s.hasBinding("qo") =>
          Individual(ULOURLEscape.unapply(s.getBinding("qv").getValue.toString), ULOModel.toULO(s.getBinding("qo").getValue.toString).asInstanceOf[ULOClass].toUnary)
        case _ =>
          ???
      }
    }
    values
  }


  private def makeQuery(q: RelationExp): RdfPredicate = q match {
    case ToSubject(dep) =>
      toUlo(dep) match {
        case Some(ulo) =>
          () => "<" + ulo.toIri + ">"
        case _ =>
          ???
      }
    case ToObject(dep) =>
      toUlo(dep) match {
        case Some(ulo) =>
          () => "^<" + ulo.toIri + ">"
        case _ =>
          ???
      }
    case Choice(qs@_*) if qs.contains(Reflexive) =>
      val nqs = qs.filterNot(_ == Reflexive)
      () => "(" + makeQuery(Choice(nqs: _*)).getQueryString + ")?"
    case Choice(qs@_*) =>
      () => "(" + qs.map(makeQuery(_).getQueryString).mkString("|") + ")"
    case Transitive(q) =>
      () => "(" + makeQuery(q).getQueryString + ")+"
    case _ =>
      ???
  }


}