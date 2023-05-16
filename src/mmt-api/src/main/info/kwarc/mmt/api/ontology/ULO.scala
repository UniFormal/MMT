package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api.Path
import org.eclipse.rdf4j.model.{IRI, Resource, Value}
import org.eclipse.rdf4j.model.impl.LinkedHashModelFactory
import org.eclipse.rdf4j.model.util.{RDFCollections, Values}
import org.eclipse.rdf4j.model.util.Values.{bnode, iri}
import org.eclipse.rdf4j.model.vocabulary.{DC, OWL, RDF, RDFS, XSD}

implicit def PathToIri(p : Path): IRI = iri(p.toPath(true))
implicit def asResource(e: ULOTrait): Resource = e.toIri

trait ULOStatement {
  def triples : Seq[(Resource,IRI,Value)]
}
case class Statement(resource : Resource, pred: IRI, value: Value) extends ULOStatement {
  def triples = Seq((resource,pred,value))
}

trait ULOTrait {

  import ULOModel._
  def toIri : IRI
  protected def doAdd(p : IRI,v:Value): this.type = {
    ULOModel.add(this,p,v)
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
  override def toIri: IRI = ULOModel.ulo ## name
}
trait ULOPredicate extends ULOSubject {
  private[ontology] def transitive(): this.type = `type`(OWL.TRANSITIVEPROPERTY)
  private[ontology] def symmetric(): this.type = `type`(OWL.SYMMETRICPROPERTY)
  private[ontology] def asymmetric(): this.type = `type`(OWL.ASYMMETRICPROPERTY)
  private[ontology] def subpropertyOf(p: Resource*): this.type = doAdd(RDFS.SUBPROPERTYOF,ULOModel.sequence(p))
  private[ontology] def disjointWith(p: Resource*): this.type = doAdd(OWL.PROPERTYDISJOINTWITH,ULOModel.sequence(p))
}

class ObjectProperty(name : String, binary:Option[Binary] = None) extends ULOSubject(name) with ULOPredicate {
  `type`(OWL.OBJECTPROPERTY)
  def apply(s: Path, o: Path) = Statement(s, this.toIri, o)
}
class DatatypeProperty(name : String, binary:Option[Binary] = None) extends ULOSubject(name) with ULOPredicate {
  `type`(OWL.DATATYPEPROPERTY)
  def apply(s: Path, o: Any) = Statement(s, this.toIri, o match {
    case p: Path => p
    case str : String => Values.literal(str)
    case _ => iri(o.toString)
  })
}
class ULOClass(name : String, unary:Option[Unary] = None) extends ULOSubject(name) {
  `type`(OWL.CLASS)
  private[ontology] def disjointWith(p: Resource*): this.type = doAdd(OWL.DISJOINTWITH, ULOModel.sequence(p))
  def apply(p: Path) = Statement(p, RDF.TYPE, this)
}

object ULOModel {
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

  val document = new ULOClass("document", Some(IsDocument))
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


}
