package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api.archives.{Archive, relational}
import info.kwarc.mmt.api.frontend.{Controller, Extension}
import info.kwarc.mmt.api.utils.{File, FilePath, JSON, JSONArray, JSONObject, JSONString, URI}
import info.kwarc.mmt.api.{ComplexStep, DPath, GlobalName, MPath, NamespaceMap, Path, frontend}
import org.eclipse.rdf4j.model.{IRI, Resource, Value}
import org.eclipse.rdf4j.model.util.{RDFCollections, Values}
import org.eclipse.rdf4j.model.util.Values.{bnode, iri}
import org.eclipse.rdf4j.model.vocabulary.{DC, OWL, RDF, RDFS, XSD}
import org.eclipse.rdf4j.query.TupleQueryResult
import org.eclipse.rdf4j.repository.RepositoryResult
import org.eclipse.rdf4j.repository.sail.SailRepositoryConnection
import org.eclipse.rdf4j.rio.Rio
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns
import org.eclipse.rdf4j.sparqlbuilder.rdf.RdfPredicate

import java.io.FileOutputStream


object RDFImplicits {
  import java.net.{URLDecoder, URLEncoder}
  val path_namespace = "mmt://path#"
  implicit def pathToIri(p: Path): IRI = iri(
    p.toPath(false)
      .replace("[","%5B")
      .replace("]","%5D")
      .replace(" ","%20")
      .replace(">","%3E")
      .replace("<", "%3C")
      .replace("|","%7C")
      .replace("\\","%5C")
      .replace("{","%7B")
      .replace("}","%7D")
      .replace("#","%23")
      .replace("^", "%5E") + "#"
    //p.toPath(false) + "#"
    //path_namespace + URLEncoder.encode(p.toPath(false),"UTF-8").replaceFirst("%3A",":")
  )
  def iriToPath(i: IRI) = Path.parse(i.getNamespace.init
    .replace("%5B","[")
    .replace("%5D","]")
    .replace("%20"," ")
    .replace("%3E",">")
    .replace("%3C","<")
    .replace("%7C","|")
    .replace("%5C","\\")
    .replace("%7B","{")
    .replace("%7D","}")
    .replace("%23","#")
    .replace("%5E","^"))//URLDecoder.decode(i.getLocalName,"UTF-8"))
  implicit def asResource(e: ULOTrait): Resource = e.toIri
  def isPath(i:Value) = i.isIRI && i.asInstanceOf[IRI].getLocalName.isEmpty//.getNamespace == path_namespace

  implicit def URIToIRI(uri:URI): IRI = iri(uri.toString)
}
import RDFImplicits._

trait ULOStatement {
  def triples : Seq[(Resource,IRI,Value)]
  def statements = triples.map { t =>
    new org.eclipse.rdf4j.model.Statement {
      override def getSubject: Resource = t._1
      override def getPredicate: IRI = t._2
      override def getObject: Value = t._3
      override def getContext: Resource = null
    }
  }
}
case class SimpleStatement(subject : Resource, pred: IRI, `object`: Value) extends ULOStatement {
  lazy val triples = Seq((subject,pred,`object`))
}

trait ULOTrait {

  ULO.toULO(toIri.toString) = this

  import ULO._
  def toIri : IRI
  protected def doAdd(p : IRI,v:Value): this.type = {
    ULO.add(this,p,v)
    this
  }
  private[ontology] def inverseOf(p: Resource*): this.type = doAdd(OWL.INVERSEOF,sequence(p))
  private[ontology] def domain(p: Resource*): this.type = doAdd(RDFS.DOMAIN,sequence(p))
  private[ontology] def range(p: Resource*): this.type = doAdd(RDFS.RANGE,sequence(p))
  private[ontology] def comment(s: String): this.type = doAdd(RDFS.COMMENT,org.eclipse.rdf4j.model.util.Values.literal(s))
  private[ontology] def `type`(p: Resource*): this.type = doAdd(RDF.TYPE,sequence(p))
  private[ontology] def isDisjointUnionOf(p: Resource*): this.type = doAdd(OWL.DISJOINTUNIONOF,sequence(p))
  private[ontology] def isIntersectionOf(p: Resource*): this.type = doAdd(OWL.INTERSECTIONOF,sequence(p))
  private[ontology] def subclassOf(p: Resource*): this.type = doAdd(RDFS.SUBCLASSOF,sequence(p))
  private[ontology] def complementOf(p: Resource*): this.type = doAdd(OWL.COMPLEMENTOF,sequence(p))
}

class ULOSubject(val name:String) extends ULOTrait {
  override def toIri: IRI = ULO.ulo ## name
}
trait ULOPredicate extends ULOSubject {
  private[ontology] def transitive(): this.type = `type`(OWL.TRANSITIVEPROPERTY)
  private[ontology] def symmetric(): this.type = `type`(OWL.SYMMETRICPROPERTY)
  private[ontology] def asymmetric(): this.type = `type`(OWL.ASYMMETRICPROPERTY)
  private[ontology] def subpropertyOf(p: Resource*): this.type = doAdd(RDFS.SUBPROPERTYOF,ULO.sequence(p))
  private[ontology] def disjointWith(p: Resource*): this.type = doAdd(OWL.PROPERTYDISJOINTWITH,ULO.sequence(p))
}

class ObjectProperty(name : String, binary:Option[Binary] = None) extends ULOSubject(name) with ULOPredicate {
  `type`(OWL.OBJECTPROPERTY)
  def apply(s: IRI, o: IRI) = SimpleStatement(s, this.toIri, o)
  def toBinary = binary.getOrElse(CustomBinary(name, "", ""))
}
class DatatypeProperty(name : String, binary:Option[Binary] = None) extends ULOSubject(name) with ULOPredicate {
  `type`(OWL.DATATYPEPROPERTY)
  def apply(s: IRI, o: Any) = SimpleStatement(s, this.toIri, o match {
    case p: Path => RDFImplicits.pathToIri(p)
    case str : String => Values.literal(str)
    case d:Long => Values.literal(d)
    case d:Int => Values.literal(d)
    case _ => iri(o.toString)
  })
  def toBinary = binary.getOrElse(CustomBinary(name, "", ""))
}
class ULOClass(name : String, unary:Option[Unary] = None) extends ULOSubject(name) {
  `type`(OWL.CLASS)
  def toUnary = unary.getOrElse(CustomUnary(name))
  private[ontology] def disjointWith(p: Resource*): this.type = doAdd(OWL.DISJOINTWITH, ULO.sequence(p))
  def apply(p: IRI) = SimpleStatement(p, RDF.TYPE, this)
}

object ULO {

  val toULO = scala.collection.mutable.Map.empty[String, ULOTrait]

  import org.eclipse.rdf4j.model.impl.LinkedHashModelFactory

  val namespace = "http://mathhub.info/ulo"
  val model = (new LinkedHashModelFactory).createEmptyModel()
  model.setNamespace("ulo",namespace)
  def sequence(rls:Seq[Resource]) : Value = if (rls.lengthCompare(1) == 0) { rls.head } else {
    import scala.jdk.CollectionConverters._
    val head = bnode()
    RDFCollections.asRDF(rls.asJava,head,model)
    head
  }

  val ulo = new ULOTrait {
    override def toIri: IRI = iri(namespace)

    def ##(s: String) = iri(namespace + "#" + s)
  }
  private[ontology] def add(r: Resource, i: IRI, v: Value) = model.add(r, i, v, ulo)

  add(ulo, DC.RIGHTS, org.eclipse.rdf4j.model.util.Values.literal(
    "This ontology is licensed under the CC-BY-SA license."
  ))

  val ncname = XSD.NCNAME
  val thing = OWL.THING


  // -------------------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
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

  val theory = new ULOClass("theory",Some(IsTheory))
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
  val function = new ULOClass("function",Some(IsDataConstructor))
    .subclassOf(declaration)
    .comment("Functions that construct objects, possibly from other objects, for example in first-order logic the successor function.")
  // Notably, according to https://kwarc.info/people/mkohlhase/papers/cicm19-ulo.pdf,
  // constants are also ontologically functions.
  val `type` = new ULOClass("type")
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

  val specifies = new ObjectProperty("specifies",Some(Declares))
    .domain(physical)
    .domain(logical)
    .comment("The physical organizational item S specifies a knowledge item O, i.e. S is represented in O.")
  val specified_in = new ObjectProperty("specified_in")
    .inverseOf(specifies)
    .domain(logical)
    .range(physical)


  // Cross References and inter-statements

  val crossrefs = new ObjectProperty("crossrefs",Some(RefersTo))
  val aligned_with = new ObjectProperty("aligned-with",Some(IsAlignedWith))
    .subpropertyOf(crossrefs)
    .symmetric()
  val alternative_for = new ObjectProperty("alternative-for")
    .subpropertyOf(crossrefs)
  val inspired_by = new ObjectProperty("inspired-by")
    .subpropertyOf(crossrefs)
  val same_as = new ObjectProperty("same-as",Some(IsAliasFor))
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
    .`type`(OWL.FUNCTIONALPROPERTY)
    .domain(thing)
  val formalizes = new ObjectProperty("formalizes")
  val uses = new ObjectProperty("uses", Some(DependsOn))
    .transitive()
    .domain(statement)
    .range(function)
    .range(`type`)
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
  val instance_of = new ObjectProperty("instance-of",Some(IsInstanceOf))
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
    .domain(`type`)
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
    .domain(`type`)
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
  val archive = library
  val document = new ULOClass("document", Some(IsDocument))
    .subclassOf(logical)
  val module = new ULOClass("module")
  theory.subclassOf(module)
  val declares = specifies
  val theory_morphism = new ULOClass("theory-morphism")
    .subclassOf(logical)
  val has_morphism_from = new ObjectProperty("has-morphism-from")
  val has_morphism_to = new ObjectProperty("has-morphism-to")
  val domain = new ObjectProperty("domain",Some(HasDomain))
    .subclassOf(inter_statement)
  val codomain = new ObjectProperty("codomain",Some(HasCodomain))
    .subclassOf(inter_statement)
  val implicit_morphism = new ULOClass("implicit-morphism")
    .subclassOf(theory_morphism)
  val has_implicit_morphism_from = new ObjectProperty("has-implicit-morphism-from")
    .subpropertyOf(has_morphism_from)
  val has_implicit_morphism_to = new ObjectProperty("has-implicit-morphism-to")
    .subpropertyOf(has_morphism_to)
  val has_meta_theory = new ObjectProperty("has-meta-theory",Some(HasMeta))
    .subclassOf(has_implicit_morphism_from)
  val is_meta_theory_of = new ObjectProperty("is-meta-theory-of")
    .subclassOf(has_implicit_morphism_to)
    .inverseOf(has_meta_theory)
  val view = new ULOClass("view",Some(IsView))
    .subclassOf(theory_morphism)
    .subclassOf(module)
  val has_view_from = new ObjectProperty("has-view-from",Some(HasViewFrom))
    .subpropertyOf(has_morphism_from)
  val has_view_to = new ObjectProperty("has-view-to")
    .subpropertyOf(has_morphism_to)
  val structure = new ULOClass("structure",Some(IsStructure))
    .subclassOf(theory_morphism)
  val has_structure_from = new ObjectProperty("has-structure-from")
    .subpropertyOf(has_morphism_from)
  val has_structure_to = new ObjectProperty("has-structure-to")
    .subpropertyOf(has_morphism_to)
  val include = new ULOClass("include")
    .subclassOf(structure)
  val includes = new ObjectProperty("include",Some(Includes))
    .subpropertyOf(has_implicit_morphism_from)
    .subpropertyOf(has_structure_from)
  val is_included_in = new ObjectProperty("is-included-in")
    .subpropertyOf(has_implicit_morphism_to)
    .subpropertyOf(has_structure_to)
  val constant = new ULOClass("constant",Some(IsConstant))
    .subclassOf(declaration)
  val rule_constant = new ULOClass("rule-constant",Some(IsRule))
    .subclassOf(constant)
  val depends_on = uses
  val derived_declaration = new ULOClass("derived-declaration",Some(IsDerivedDeclaration))
    .isIntersectionOf(derived, declaration)
  val pattern = new ULOClass("pattern",Some(IsPattern))
    .subclassOf(derived_declaration)
  val instance = new ULOClass("instance",Some(IsInstance))
    .subclassOf(derived_declaration)
  val nested_module = new ULOClass("nested-module")
    .subclassOf(module)
    .subclassOf(declaration)

  // Helpers for morphisms etc.
  // TODO should be largely replaced by a (Unary,Binary)-pair - the morphism itself is unary and implies the binary
  // relation between domain/codomain
  trait Morphism extends ULOStatement {
    val _domain: MPath
    val _codomain: MPath
    val name: Option[Resource] = None
    val relation: Option[ULOClass] = None
    val from_to: ObjectProperty
    val to_from: ObjectProperty

    override lazy val triples: Seq[(Resource, IRI, IRI)] = {
      val base = Seq(
        (pathToIri(_domain),from_to.toIri,pathToIri(_codomain)),
        (pathToIri(_codomain),to_from.toIri,pathToIri(_domain)))
      if (name.isDefined && relation.isDefined) {
        base :+ (name.get, RDF.TYPE, relation.get.toIri) :+
          (name.get, ULO.domain.toIri, pathToIri(_domain)) :+
          (name.get, ULO.codomain.toIri, pathToIri(_codomain))
      } else base
    }
  }

  case class HasMetaM(_codomain: MPath, _domain: MPath) extends Morphism {
    val from_to = is_meta_theory_of
    val to_from = has_meta_theory
  }

  case class View(path: MPath, _domain: MPath, _codomain: MPath) extends Morphism {
    override val name = Some(path)
    override val relation = Some(view)
    val from_to = has_view_to
    val to_from = has_view_from
  }

  case class ULOStructure(val path: GlobalName, val _domain: MPath, val _codomain: MPath) extends Morphism {
    override val name = Some(path)
    override val relation = Some(structure)
    val from_to = has_structure_to
    val to_from = has_structure_from

    override def equals(obj: Any): Boolean = obj match {
      case i: ULOStructure => i._domain == _domain && i._codomain == _codomain && i.path == path
      case _ => false
    }

    override def hashCode(): Int = path.hashCode() + _domain.hashCode() + _codomain.hashCode()
  }

  object Structure {
    def apply(path: GlobalName, _domain: MPath, _codomain: MPath) = new ULOStructure(path, _domain, _codomain)
  }

  object Include {
    def apply(_domain: MPath, _codomain: MPath, _path: Option[GlobalName] = None) = new ULOInclude(_domain, _codomain, _path)
  }

  class ULOInclude(_domain: MPath, _codomain: MPath, _path: Option[GlobalName] = None)
    extends ULOStructure(_path.getOrElse(_codomain ? ComplexStep(_domain)), _domain, _codomain) {
    override val relation = Some(include)
    override val from_to = is_included_in
    override val to_from = includes

    override def equals(obj: Any): Boolean = obj match {
      case i: ULOInclude => i._domain == _domain && i._codomain == _codomain
      case _ => false
    }

    override def hashCode(): Int = _domain.hashCode() + _codomain.hashCode()
  }

  // sTeX Relations

  val has_notation_for = new ObjectProperty("has-notation-for")
  val has_language = new DatatypeProperty("has-language")
  val has_language_module = new ObjectProperty("has-language-module")
  val is_language_module = new ULOClass("language-module")
  val variable = new ULOClass("variable")
    .subclassOf(declaration)
  val mathstructure = new ULOClass("mathstructure")
    .subclassOf(theory)
  val mathstructure_of = new ObjectProperty("mathstructure_of")
  val problem = new ULOClass("problem")
  val cognitiveDimension = new DatatypeProperty("cognitive-dimension")
  val precondition = new DatatypeProperty("precondition")
  val objective = new DatatypeProperty("objective")
}

object RDFStore {
  val memory = URI.scheme("mmt") colon "memory"
  def archive(id:String) = iri(memory.toString + "/archive#" + id)
  def archiveId(iri: IRI) = if (iri.getNamespace == memory.toString + "/archive") {
    Some(iri.getLocalName)
  } else None
  import org.eclipse.rdf4j.rio.RDFFormat
  val fileFormat =  ("brf",RDFFormat.BINARY) // ("rdf",RDFFormat.RDFXML) //
}

trait SubGraph {
  def add(s: ULOStatement): Unit
  def close: Unit
  def write(f: File): Unit

  def getAll: Set[ULOStatement]

}
class RDFStore(protected val report : frontend.Report) extends RDFRelStoreLike {
  import RDFStore._
  import org.eclipse.rdf4j.repository.sail.SailRepository
  import org.eclipse.rdf4j.sail.memory.MemoryStore

  protected lazy val repo = new SailRepository(new MemoryStore)

  def start: Unit = {
    val conn = repo.getConnection
    conn.add(ULO.model)
    conn.close()
  }
  protected def add(s : ULOStatement,graph:URI = memory)(conn:SailRepositoryConnection) = s.triples.foreach{
    case (s,p,o) => conn.add(s,p,o,graph)
  }
  def clearGraph(uri:URI) = {
    val conn = repo.getConnection
    conn.clear(uri)
    conn.close()
  }

  def newGraph(uri:URI): SubGraph = new ISubGraph(repo.getConnection,uri)
  class ISubGraph(conn: SailRepositoryConnection,uri:URI) extends SubGraph {
    import scala.jdk.CollectionConverters._
    def add(s : ULOStatement) = RDFStore.this.add(s,uri)(conn)
    def getAllI = conn.getStatements(null,null,null,true,uri).asScala.toSet
    def getAll = getAllI.map{s =>
      SimpleStatement(s.getSubject,s.getPredicate,s.getObject)
    }
    conn.clear(uri)
    def close = conn.close()
    def write(f: File) = {
      f.up.mkdirs()
      val out = new FileOutputStream(f)
      val writer = Rio.createWriter(RDFStore.fileFormat._2, out)
      try {
        writer.startRDF()
        getAllI.foreach(writer.handleStatement)
        writer.endRDF()
      } finally { out.close() }
    }
  }

  override def readArchive(a: Archive, in: FilePath, controller: Controller, kd: String): Unit = {
    addArchive(a)
    val conn = repo.getConnection
    if ((a / relational).exists) a.traverse(relational, in, Archive.traverseIf(fileFormat._1)) { case info.kwarc.mmt.api.archives.Current(inFile, _) =>
      val iri = relpath(a, inFile)
      conn.add(inFile.toJava, fileFormat._2, iri)
    }
    conn.close()
  }

  private def addArchive(a : Archive): Unit = {
    val conn = repo.getConnection
    add(ULO.archive(archive(a.id)))(conn)
    val segs = a.id.split('/')
    if (segs.length > 1) {
      val inits = segs.inits.toList.init.reverse.map(s => archive(s.mkString("/")))
      inits.init.zip(inits.tail).foreach {
        case (p,c) =>
          add(ULO.library_group(p))(conn)
          add(ULO.contains(p,c))(conn)
      }
    }
    //readRelational(a,conn)
    conn.close()
  }
  /*
  def loadAll(controller:Controller) = controller.backend.getArchives.foreach(addArchive)
  private def readRelational(a: Archive,conn:SailRepositoryConnection): Unit = {
    val reldir = a / relational
    if (reldir.exists() && reldir.isDirectory) {
      reldir.descendants.filter(_.getExtension.contains(fileFormat._1)).foreach { f =>
        val iri = relpath(a,f)
        conn.add(f.toJava,fileFormat._2,iri)
      }
    }
  }

   */

  private[ontology] def relpath(archive: Archive, f: File) = {
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
      URI(sub.head.replace("..", "://") + sub.tail.init.mkString("/", "/", "") + "?" + name)
    } else sub.setExtension("omdoc").segments.foldLeft(archive.narrationBase)((u, s) => u / s)
    furi
  }
  // Querying

  // Wrapper for query results
  case class QueryResult(rdf4j: TupleQueryResult) {
    import scala.jdk.CollectionConverters._
    /*
    def toIterator = it.asScala
    def foreach(f: org.eclipse.rdf4j.model.Statement => Unit) = toIterator.foreach(f)
    def toRelational: List[RelationalElement] = toIterator.toList.map { statement =>
      statement.getPredicate match {
        case RDF.TYPE if isPath(statement.getSubject) =>
          Individual(
            iriToPath(statement.getSubject.asInstanceOf[IRI]),
            ULO.toULO(statement.getObject.toString) match {
              case cls: ULOClass => cls.toUnary
              case _ =>
                ???
            })
        case pred if isPath(statement.getSubject) && isPath(statement.getObject) =>
          Relation(
            ULO.toULO(pred.toString) match {
              case op: ObjectProperty => op.toBinary
              case dp: DatatypeProperty => dp.toBinary
              case _ =>
                ???
            },
            iriToPath(statement.getSubject.asInstanceOf[IRI]),
            iriToPath(statement.getObject.asInstanceOf[IRI]),
          )
      }
    }
     */
    def getPaths = {
      val name = rdf4j.getBindingNames.get(0)
      val ret = rdf4j.asScala.map(_.getValue(name)).toList collect {
        case iri: IRI if isPath(iri) => iriToPath(iri)
      }
      rdf4j.close()
      ret
    }
    def getJson = {
      def stringify(v : Value) = JSONString(if (v.isIRI) {
        val iri = v.asInstanceOf[IRI]
        if (isPath(iri)) iriToPath(iri).toString
        else iri.stringValue()
      } else v.stringValue())
      val vars = JSONArray(rdf4j.getBindingNames.asScala.toList.map(JSONString) :_*)
      val bindings = JSONArray(rdf4j.asScala.toList.map{ bs =>
        JSONObject(bs.asScala.toList.map{b =>
          val value = b.getValue
          val tp : List[(String,JSON)] = if (value.isIRI) List(("type",JSONString("uri")),("value",stringify(value)))
            else if (value.isBNode) List(("type",JSONString("bnode")),("value",JSONString(value.stringValue())))
            else if (value.isLiteral) {
              val lit = value.asInstanceOf[org.eclipse.rdf4j.model.Literal]
              if (lit.getLanguage.isEmpty) List(("type",JSONString("literal")),("value",JSONString(value.stringValue())))
              else List(("type",JSONString("literal")),("xml:lang",JSONString(lit.getLanguage.get())))
            }
          else { // triple
            val tr = value.asInstanceOf[org.eclipse.rdf4j.model.Triple]
            List(("type",JSONString("triple")),("value",JSONObject(
              ("subject",stringify(tr.getSubject)),
              ("predicate",JSONString(tr.getPredicate.stringValue())),
              ("object",stringify(tr.getObject))
            )))
          }
          (b.getName, JSONObject(tp:_*))
        } :_*)
      } :_*)
      rdf4j.close()
      JSONObject(("head",JSONObject(("vars",vars))),("results",JSONObject(("bindings",bindings))))
    }
  }

  def query(q: SparqlQuery) = {
    val conn = repo.getConnection
    val res = conn.prepareTupleQuery(q.queryString).evaluate()
   QueryResult(res)
  }
  def query(str : String) = {
    val conn = repo.getConnection
    QueryResult(conn.prepareTupleQuery(str).evaluate())
  }

  start
}


trait RDFRelStoreLike extends RelStoreLike { this : RDFStore =>
  def +=(r: RelationalElement) = {
    val conn = repo.getConnection
    add(r.toULO)(conn)
    conn.close()
  }

  def clear = {
    val conn = repo.getConnection
    conn.clear()
    conn.add(ULO.model)
    conn.close()
  }

  def theoryClosure(p: MPath): List[MPath] = {
    val q = Transitive(+HasMeta | +Includes | +DependsOn | Reflexive)
    var ret: List[MPath] = Nil
    query(p, q) { case mp: MPath => ret ::= mp case _ => }
    ret
  }

  override def getInds: Iterator[Individual] = getIndsA().iterator

  def getInds(tp: Unary): Iterator[Path] = getIndsA(Some(tp.toULO)).map(_.path).iterator


  override def getType(p: Path): Option[Unary] = {
    val qv = SparqlBuilder.`var`("qv")
    val query =
      Queries.SELECT(qv).where(GraphPatterns.tp(pathToIri(p), RDF.TYPE, qv))
    val conn = repo.getConnection
    val values = conn.prepareTupleQuery(query.getQueryString).evaluate()
    conn.close()
    if (values.hasNext) {
      val value = values.next().getBinding("qv").getValue
      ULO.toULO.get(value.toString) match {
        case Some(c: ULOClass) => Some(c.toUnary)
        case _ => None
      }
    } else None
  }

  override def hasType(p: Path, tp: Unary): Boolean = {
    val conn = repo.getConnection
    val res = conn.getStatements(pathToIri(p), RDF.TYPE, tp.toULO.toIri)
    conn.close()
    res.hasNext
  }

  override def hasDep(from: Path, to: Path, bin: Binary): Boolean = {
    val conn = repo.getConnection
    conn.getStatements(pathToIri(from), bin.toULO.toIri, pathToIri(to)).hasNext
  }

  override def getDeps: Iterator[Relation] = {
    import scala.jdk.CollectionConverters._
    val conn = repo.getConnection
    var res: List[Relation] = Nil
    conn.getStatements(null, null, null).forEach { tr =>
      val rel = ULO.toULO.get(tr.getPredicate.toString) match {
        case Some(op: ObjectProperty) => Some(op.toBinary)
        case Some(dtp: DatatypeProperty) => Some(dtp.toBinary)
        case _ => None
      }
      rel.foreach { rl =>
        if (tr.getSubject.isIRI && tr.getObject.isIRI)
          res ::= Relation(rl,
            iriToPath(tr.getSubject.asInstanceOf[IRI]),
            iriToPath(tr.getObject.asInstanceOf[IRI])
          )
      }
    }
    conn.close()
    res.iterator
  }

  private def getIndsA(cls: Option[ULOClass] = None): List[Individual]
  = {
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
    val conn = repo.getConnection
    val values = conn.prepareTupleQuery(query.getQueryString).evaluate().iterator().asScala.toList.map { s =>
      cls match {
        case Some(c) => Individual(iriToPath(s.getBinding("qv").getValue.asInstanceOf[IRI]), c.toUnary)
        case None if s.hasBinding("qo") && isPath(s.getBinding("qv").getValue) =>
          Individual(iriToPath(s.getBinding("qv").getValue.asInstanceOf[IRI]), ULO.toULO(s.getBinding("qo").getValue.toString).asInstanceOf[ULOClass].toUnary)
        case _ =>
          ???
      }
    }
    conn.close()
    values
  }

  def query(start: Path, q: RelationExp)(implicit add: Path => Unit) = {
    val qv = SparqlBuilder.`var`("qv")
    val p = makeQuery(q)
    val query = Queries.SELECT(qv).where(GraphPatterns.tp(qv, p, pathToIri(start)))
    import scala.jdk.CollectionConverters._
    repo.getConnection.prepareTupleQuery(query.getQueryString).evaluate().forEach { res =>
      val vl = res.getBinding("qv").getValue
      if (RDFImplicits.isPath(vl))
        add(iriToPath(vl.asInstanceOf[IRI]))
    }
  }

  private def makeQuery(q: RelationExp): RdfPredicate = q match {
    case ToSubject(dep) =>
      () => "<" + dep.toULO.toIri.toString + ">"
    case ToObject(dep) =>
      () => "^<" + dep.toULO.toIri + ">"
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

/** Converts .rel files to the rdf format specified in [[RDFStore.fileFormat]] */
class RelToRDF extends Extension {
  override def start(args: List[String]): Unit = {
    val rdfdb = new RDFStore(controller.report)
    controller.backend.getArchives.foreach {a =>
      val relpath = a / relational
      val nsmap = NamespaceMap(DPath(a.narrationBase))
      if (relpath.isDirectory) relpath.descendants.filter(_.getExtension.contains("rel")).foreach { file =>
        val iri = rdfdb.relpath(a,file)
        println(iri)
        var ret : List[ULOStatement] = Nil
        File.ReadLineWise(file) {line => try {
          val re = controller.relman.parse(line,nsmap)
          ret ::= (re match {
            case Individual(path,IsUntypedConstant) => Individual(path,IsConstant)
            case _ => re
          }).toULO
        } catch {
          case t: Throwable =>
            t.printStackTrace()
        }}
        val outfile = new FileOutputStream(file.setExtension(RDFStore.fileFormat._1))
        val writer = Rio.createWriter(RDFStore.fileFormat._2,outfile)
        try {
          writer.startRDF()
          ret.flatMap(_.statements).foreach(writer.handleStatement)
          writer.endRDF()
        } finally { outfile.close() }
      }
    }
  }
}
sealed trait SparqlQuery {
  private case class QUNION(q1: SparqlQuery, q2: SparqlQuery) extends SparqlQuery {
    def queryString: String = s"{ ${q1.queryString} } UNION { ${q2.queryString} }"
  }
  private case class QAND(q1: SparqlQuery, q2: SparqlQuery) extends SparqlQuery {
    def queryString: String = s"${q1.queryString} ${q2.queryString}"
  }
  def UNION(q: SparqlQuery): SparqlQuery = QUNION(this,q)
  def AND(q: SparqlQuery): SparqlQuery = QAND(this,q)

  def queryString: String
}
object SPARQL {
  sealed trait Object {
    def toObjString : String
  }
  sealed trait Subject extends this.Object

  private case class Trans(p:Predicate) extends Predicate {
    def predString: String = p.predString + "+"
  }
  private case class PredUnion(p:Predicate*) extends Predicate {
    def predString: String = p.map(_.predString).mkString("(","|",")")
    override def |(that:Predicate) : Predicate = that match {
      case PredUnion(q@_*) => PredUnion(p ++ q :_*)
      case _ => PredUnion(p :+ that :_*)
    }
  }
  sealed trait Predicate {
    def predString : String
    def + : Predicate = Trans(this)
    def |(that:Predicate) : Predicate = PredUnion(this,that)
  }
  private case class UloPred(u:ULOPredicate) extends Predicate {
    override def predString: String = "<" + u.toIri.toString + ">"
  }
  implicit def asPred(u:ULOPredicate): Predicate = UloPred(u)

  private case class PathO(p:Path) extends Subject {
    override def toObjString: String = "<" + pathToIri(p).toString + ">"
  }
  case class V(s:String) extends Subject {
    def toObjString = s"?$s"
  }
  implicit def pathtosubject(p:Path): Subject = PathO(p)
  private case class SelectWhere(vars:List[String],where:SparqlQuery) extends SparqlQuery {
    def queryString: String = s"SELECT ${vars.map("?" + _).mkString(" ")} WHERE { ${where.queryString} }"
  }
  case class SELECT(`var` : String*) {
    def WHERE(q : SparqlQuery): SparqlQuery = SelectWhere(`var`.toList,q)
  }
  import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries._
  case class T(subject: Subject,predicate: this.Predicate,`object`:this.Object) extends SparqlQuery {
    def queryString: String = s"${subject.toObjString} ${predicate.predString} ${`object`.toObjString} ."
  }
  case class HASTYPE(subject: Subject,`object`:this.Object) extends SparqlQuery {
    override def queryString: String = s"${subject.toObjString} <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ${`object`.toObjString} ."
  }

}