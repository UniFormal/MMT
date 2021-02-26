package info.kwarc.mmt.api.ontology.rdf

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.utils.{Escaping, URI}
import org.eclipse.rdf4j.model.vocabulary.XSD
import org.eclipse.rdf4j.model.{IRI, Resource, Value}

import java.net.URLEncoder
import scala.List

trait ULOStatement {
  def triples : Seq[(Resource,IRI,Value)]
}
case class SimpleStatement(s : Resource, p : IRI, o : Value) extends ULOStatement {
  def triples = Seq((s,p,o))
}

object ULO {
  import org.eclipse.rdf4j.model.impl.TreeModel
  import org.eclipse.rdf4j.model.util.Values.iri
  import org.eclipse.rdf4j.model.vocabulary.{DC, OWL, RDF, RDFS}
  import org.eclipse.rdf4j.model.{IRI, Resource}

  val mmt_uri = DPath(URI.scheme("mmt"))

  val namespace = "https://mathhub.info/ulo"
  val model = new TreeModel()
  model.setNamespace("ulo",namespace)
  private def error = {
    ???
  }
  private def add(r : Resource, i : IRI, v : Value) = model.add(r,i,v,ulo) || error

  implicit def iriToIri(i : ULOTrait) : Resource = i.toIri

  private var index = 0

  private def sequence(rls : Seq[Resource]) : Value = rls match {
    case Seq() => RDF.NIL
    case Seq(a) =>
      a
    case _ =>
      def inner(ls : Seq[Resource],i : Int) : Value = ls match {
        case Seq() => RDF.NIL
        case other =>
          val name = ulo ## ("helper-" + index.toString + "-" + i)
          add(name,RDF.FIRST,other.head)
          add(name,RDF.REST,inner(other.tail,i+1))
          name
      }
      val r = inner(rls,0)
      index += 1
      r
  }

  private val self = this

  trait ULOTrait {
    def toIri : IRI
    private[rdf] def inverseOf(p : Resource*) : this.type  = {
      add(this,OWL.INVERSEOF,sequence(p))
      this
    }
    private[rdf] def domain(p : Resource*) : this.type  = {
      add(this,RDFS.DOMAIN,sequence(p))
      this
    }
    private[rdf] def range(p : Resource*) : this.type  = {
      add(this,RDFS.RANGE,sequence(p))
      this
    }
    private[rdf] def comment(s : String) : this.type  = {
      add(this,RDFS.COMMENT,org.eclipse.rdf4j.model.util.Values.literal(s))
      this
    }
    private[rdf] def _type(p : Resource*) : this.type  = {
      add(this,RDF.TYPE,sequence(p))
      this
    }
    private[rdf] def isDisjointUnionOf(p : Resource*) : this.type  = {
      add(this,OWL.DISJOINTUNIONOF,sequence(p))
      this
    }
    private[rdf] def isIntersectionOf(p : Resource*) : this.type  = {
      add(this,OWL.INTERSECTIONOF,sequence(p))
      this
    }
    private[rdf] def subclassOf(p : Resource*) : this.type  = {
      add(this,RDFS.SUBCLASSOF,sequence(p))
      this
    }
    private[rdf] def complementOf(p : Resource*) : this.type  = {
      add(this,OWL.COMPLEMENTOF,sequence(p))
      this
    }
  }

  class ULOElem(s : String) extends ULOTrait {
    override def toIri: IRI = ulo ## s
  }

  case class ObjectProperty(s : String) extends ULOElem(s) {
    _type(OWL.OBJECTPROPERTY)
    private[rdf] def subpropertyOf(p : Resource*) : this.type = {
      add(this,RDFS.SUBPROPERTYOF,sequence(p))
      this
    }
    private[rdf] def disjointWith(p : Resource*) : this.type  = {
      add(this,OWL.PROPERTYDISJOINTWITH,sequence(p))
      this
    }
    def apply(s : Path,o : Path) = SimpleStatement(pathToString(s),this.toIri,pathToString(o))
  }

  object URLEscape {
    def apply(p : Path) = try {iri(applyI(p))} catch {
      case t : Throwable =>
        print("")
        throw t
    }
    private def applyI(p : Path) : String = p match {
      case DPath(uri) =>
        uri.scheme.map(_ + "://").getOrElse("") +
        uri.authority.getOrElse("") + "/" +
        uri.path.map(URLEscaping.apply).mkString("/")
      case mp : MPath =>
        applyI(mp.doc) + "?" + doName(mp.name)
      case gn : GlobalName =>
        applyI(gn.module) + "%3F" + doName(gn.name)
      case CPath(parent, component) =>
        applyI(parent) + "%3F" + component.toString
      case _ =>
        ???
    }
    def doName(ln : LocalName) = ln.steps.map{
      case ComplexStep(path) => URLEscaping("[" + path.toString + "]")
      case step => URLEscaping(step.toString)
    }.mkString("%2F")
    object URLEscaping {
      def apply(s : String) = URLEncoder.encode(s,"UTF-8")
    }
  }

  private def pathToString(p : Path) = URLEscape(p)

  case class DatatypeProperty(s : String) extends ULOElem(s) {
    _type(OWL.DATATYPEPROPERTY)

    private[rdf] def subpropertyOf(p: Resource*): this.type = {
      add(this, RDFS.SUBPROPERTYOF, sequence(p))
      this
    }

    def apply(s: Path, o: Any) = try {
      SimpleStatement(pathToString(s), this.toIri, o match {
        case p: Path => pathToString(p)
        case sr: SourceRef =>
          iri(pathToString(DPath(sr.container)) + "#" + sr.region.toString)
        case ln: LocalName =>
          org.eclipse.rdf4j.model.util.Values.literal(URLEscape.doName(ln))
        case _ =>
          iri(o.toString)
      })
    } catch {
      case t : Throwable =>
        throw t
    }
  }
  case class Class(s : String) extends ULOElem(s) {
    _type(OWL.CLASS)
    private[rdf] def disjointWith(p : Resource*) : this.type  = {
      add(this,OWL.DISJOINTWITH,sequence(p))
      this
    }
    def apply(p : Path) = SimpleStatement(pathToString(p),RDF.TYPE,this)
  }

  val ulo = new ULOTrait {
    override def toIri: IRI = iri(namespace)
    def ##(s : String) = iri(namespace + "#" + s)
  }
  add(ulo,DC.RIGHTS,org.eclipse.rdf4j.model.util.Values.literal(
    "This ontology is licensed under the CC-BY-SA license."
  ))

  // Object Properties

  val aligned_with = ObjectProperty("aligned-with")
    .subpropertyOf(crossrefs)
  val alternative_for = ObjectProperty("alternative-for")
    .subpropertyOf(crossrefs)
  val antonym = ObjectProperty("antonym")
    .subpropertyOf(nyms)
  val constructs = ObjectProperty("constructs")
    .subpropertyOf(inter_statement)
    .comment("S is a constructor for an inductive type or predicate O")
  lazy val crossrefs = ObjectProperty("crossrefs")
  val example_for = ObjectProperty("example-for")
    .subpropertyOf(inter_statement)
  val contains = ObjectProperty("contains")
  val counter_example_for = ObjectProperty("counter-example-for")
    .subpropertyOf(inter_statement)
    .disjointWith(example_for)
  val defines = ObjectProperty("defines")
    .subpropertyOf(inter_statement)
    .domain(OWL.THING)
    .range(function)
    .comment("A definition defines various objects.")
  val formalizes = ObjectProperty("formalizes")
  val generated_by = ObjectProperty("generated-by")
    .subpropertyOf(inter_statement)
    .domain(function)
    .range(function)
  val hyponym = ObjectProperty("hyponym")
    .subpropertyOf(nyms)
  val hypernym = ObjectProperty("hypernym")
    .subpropertyOf(nyms)
    .inverseOf(hyponym)
  val implementation_uses = ObjectProperty("implementation-uses")
    .subpropertyOf(uses)
  val implementation_uses_implementation_of = ObjectProperty("implementation-uses-implementation-of")
    .subpropertyOf(implementation_uses)
    .subpropertyOf(uses_implementation)
  val implementation_uses_interface_of = ObjectProperty("implementation-uses-interface-of")
    .subpropertyOf(implementation_uses)
    .subpropertyOf(uses_interface)
  val inductive_on = ObjectProperty("inductive-on")
    .subpropertyOf(inter_statement)
  val inspired_by = ObjectProperty("inspired-by")
    .subpropertyOf(crossrefs)
  val instance_of = ObjectProperty("instance-of")
    .comment("S is an instance of O iff it is a model of O, inherits from O, interprets O, etc.")
  val interface_uses = ObjectProperty("interface-uses")
    .subpropertyOf(uses)
  val interface_uses_implementation_of = ObjectProperty("interface-uses-implementation-of")
    .subpropertyOf(interface_uses)
    .subpropertyOf(uses_implementation)
  val interface_uses_interface_of = ObjectProperty("interface-uses-interface-of")
    .subpropertyOf(interface_uses)
    .subpropertyOf(uses_interface)
  lazy val inter_statement = ObjectProperty("inter-statement")
  val justifies = ObjectProperty("justifies")
    .subpropertyOf(inter_statement)
    .comment("A proof can justify a theorem or a definition.")
  lazy val nyms = ObjectProperty("nyms")
  val same_as = ObjectProperty("same-as")
    .subpropertyOf(crossrefs)
  val see_also = ObjectProperty("see-also")
    .subpropertyOf(crossrefs)
  val similar_to = ObjectProperty("similar-to")
    .subpropertyOf(crossrefs)
  val specifies = ObjectProperty("specifies")
    .domain(physical)
    .domain(logical)
    .comment("The physical organizational item S specifies a knowledge item O, i.e. S is represented in O.")
  val specified_in = ObjectProperty("specified_in")
    .inverseOf(specifies)
    .domain(logical)
    .range(physical)
  val superseded_by = ObjectProperty("superseded_by")
    ._type(OWL.TRANSITIVEPROPERTY)
    .comment("S (a deprecated knowledge item) is superseded by another.")
  lazy val uses = ObjectProperty("uses")
    ._type(OWL.TRANSITIVEPROPERTY)
    .domain(statement)
    .range(function)
    .range(_type)
  lazy val uses_implementation = ObjectProperty("uses-implementation")
    .subpropertyOf(uses)
  lazy val uses_interface = ObjectProperty("uses-interface")
    .subpropertyOf(uses)

  // Data Properties

  val action_times = DatatypeProperty("action-times")
  val automatically_proved = DatatypeProperty("automatically-proved")
    .subpropertyOf(organizational)
    .range(XSD.STRING)
    .comment("S is automatically proven by a theorem prover, O is an explanatory string.")
  val check_time = DatatypeProperty("check-time")
    .subpropertyOf(size_properties)
    .domain(function)
    .domain(_type)
    .range(XSD.NON_NEGATIVE_INTEGER)
    .comment("time (a natural number giving a time in milliseconds) it took to check the declaration that introduced the subject.")
  val deprecated = DatatypeProperty("deprecated")
    .subpropertyOf(organizational)
    .range(XSD.STRING)
    .comment("S is deprecated (do not use any longer), O is an explanatory string.")
  val docref = DatatypeProperty("docref")
    .comment("A URI reference to a place where this knowledge item is documented (usually in some read-only rich text format).")
  val experimental = DatatypeProperty("experimental")
    .subpropertyOf(organizational)
    .range(XSD.STRING)
  val external_size = DatatypeProperty("external-size")
    .subpropertyOf(size_properties)
    .domain(OWL.THING)
    .range(XSD.NON_NEGATIVE_INTEGER)
    .comment("The number of characters (not counting whitespace or comments) in the source code of the subject. This number can be approximate.")
  val important = DatatypeProperty("important")
    .subpropertyOf(organizational)
    .comment("S is important (to someone); O is an explanatory string.")
  val internal_size = DatatypeProperty("internal-size")
    .subpropertyOf(size_properties)
    .domain(OWL.THING)
    .range(XSD.NON_NEGATIVE_INTEGER)
    .comment("The number of bytes in the internal representation of the subject including any inferred objects and generated proofs. This number can be approximate.")
  val last_checked_at = DatatypeProperty("last-checked-at")
    .subpropertyOf(action_times)
    .domain(function)
    .domain(_type)
    .range(XSD.DATETIMESTAMP)
    .comment("The time stamp of when the subject was last checked.")
  val name = DatatypeProperty("name")
    .range(XSD.STRING)
    .comment("The name of a knowledge item given by the user.")
  lazy val organizational = DatatypeProperty("organizational")
  val paratype = DatatypeProperty("paratype")
    .range(XSD.NCNAME)
  val revision = DatatypeProperty("revision")
    .domain(library)
  lazy val size_properties = DatatypeProperty("size-properties")
    ._type(OWL.FUNCTIONALPROPERTY)
    .domain(OWL.THING)
  val sourceref = DatatypeProperty("sourceref")
    .domain(OWL.THING)
    .range(XSD.ANYURI)
    .comment("The URI of the physical location (e.g. file/URI, line, column) of the source code that introduced the subject.")
  val unimportant = DatatypeProperty("unimportant")
    .subpropertyOf(organizational)
    .comment("S is deemed unimportant (by someone); O is an explanatory string.")

  // Classes
  add(OWL.THING,OWL.DISJOINTUNIONOF,sequence(Seq(derived,primitive)))

  val axiom = Class("axiom")
    .isIntersectionOf(primitive,statement)
    .subclassOf(statement)
    .comment("Logically (using the Curry-Howard isomorphism), an axiom is a primitive statement, i.e. a declaration without a definiens.")
  lazy val declaration = Class("declaration")
    .subclassOf(logical)
  //  .disjointWith(theory) <- this excludes nested modules
    .comment("Declarations are named objects. They can also have a type and a definiens.")
  val definition = Class("definition")
    .subclassOf(para)
    .comment("A logical paragraph that defines a new concept.")
  lazy val derived = Class("derived")
    .subclassOf(logical)
    .disjointWith(primitive)
    .complementOf(primitive)
  val example = Class("example")
    .subclassOf(para)
    .comment("A logical paragraph that introduces a mathematical example.")
  val file = Class("file")
    .subclassOf(physical)
    .comment("A document in a file system.")
  val folder = Class("folder")
    .subclassOf(physical)
    .comment("A grouping of files and other folder, i.e. above the document level.")
  lazy val function = Class("function")
    .subclassOf(declaration)
    .comment("Functions that construct objects, possibly from other objects, for example in first-order logic the successor function.")
  lazy val library = Class("library")
    .subclassOf(physical)
    .comment("A grouping of mathematical documents. Usually in the form of a repository.")
  val library_group = Class("library-group")
    .subclassOf(physical)
    .comment("A group of libraries, usually on a repository server like GitHub.")
  lazy val logical = Class("logical")
    .comment("A logical classification of mathematical knowledge items.")
  val mutual_block = Class("mutual-block")
    .subclassOf(theory)
    .comment("A theory where the declarations may mutually refer to each other; examples include mutually recursive functions and types.")
  lazy val para = Class("para")
    .subclassOf(physical)
    .comment("A document paragraph with mathematical meaning.")
  val phrase = Class("phrase")
    .subclassOf(physical)
    .comment("Phrasal structures in mathematical texts and formulae, these include symbols, declarations, and quantifications.")
  lazy val physical = Class("physical")
    .comment("An organizational unit for the physical organization of mathematical knowledge into documents or document collections.")
  val predicate = Class("predicate")
    .subclassOf(declaration)
    .comment("A predicate is a mathematical object that evaluates to true/false when applied to enough arguments.")
  lazy val primitive = Class("primitive")
    .subclassOf(logical)
    .comment("This knowledge item does not have a definition in terms of (more) primitive items.")
  val proof = Class("proof")
    .subclassOf(para)
    .comment("A logical paragraph that serves as a justification of a proposition.")
  val proposition = Class("proposition")
    .subclassOf(para)
    .comment("A statement of a mathematical object or some relation between some.")
  val rule = Class("rule")
    .subclassOf(statement)
    .comment("Rules are statements that can be used for computation, e.g. theorems that can be used for simplification.")
  val section = Class("section")
    .subclassOf(physical)
    .comment("A physical grouping inside a document. These can be nested.")
  lazy val statement = Class("statement")
    .subclassOf(declaration)
    .comment("Statements are declarations of objects that can in principle have proofs.")
  val theorem = Class("theorem")
    .subclassOf(statement)
    .isIntersectionOf(derived,statement)
    .comment("Logically (using the Curry-Howard isomorphism), a theorem is a derived statement, i.e. a declaration with a definiens (this is the proof of the theorem given in the type)")
  lazy val theory = Class("theory")
    .subclassOf(logical)
    .comment("A semantically meaningful block of declarations that can be referred to globally. Examples include MMT theories, Mizar articles, Isabelle locales and Coq sections.")
  lazy val _type = Class("type")
    .subclassOf(declaration)
    .comment("Types divide their universe into named subsets.")
  val typedec = Class("typedec")
    .subclassOf(declaration)
    .comment("A logical paragraph that introduces a type.")
  val universe = Class("universe")
    .subclassOf(declaration)
    .comment("A universe declaration, used e.g. in strong logics like Coq.")

  // additions

  val document = Class("document")
    .subclassOf(logical)
  val module = Class("module")
  theory.subclassOf(module)
  val declares = specifies
  val theory_morphism = Class("theory-morphism")
    .subclassOf(logical)
  val domain = ObjectProperty("domain")
    .subclassOf(inter_statement)
  val codomain = ObjectProperty("codomain")
    .subclassOf(inter_statement)
  val implicit_morphism = Class("implicit-morphism")
    .subclassOf(theory_morphism)
  val meta_theory = Class("meta-theory")
    .subclassOf(implicit_morphism)
  val view = Class("view")
    .subclassOf(theory_morphism)
    .subclassOf(module)
  val structure = Class("structure")
    .subclassOf(theory_morphism)
  val include = Class("include")
    .subclassOf(structure)
  val has_structure_from = ObjectProperty("has-structure-from")
  val has_implicit_morphism_from = ObjectProperty("has-implicit-morphism-from")
  val includes = ObjectProperty("includes")
    .subpropertyOf(has_structure_from)
    .subpropertyOf(has_implicit_morphism_from)
  val constant = Class("constant")
    .subclassOf(declaration)
  val rule_constant = Class("rule-constant")
    .subclassOf(constant)
  val depends_on = uses
  val derived_declaration = Class("derived-declaraion")
    .isIntersectionOf(derived,declaration)
  val pattern = Class("pattern")
    .subclassOf(derived_declaration)
  val instance = Class("instance")
    .subclassOf(derived_declaration)
  val nested_module = Class("nested-module")
    .subclassOf(module)
    .subclassOf(declaration)
  val realization = Class("realization")
    .subclassOf(structure)
  val has_view_from = ObjectProperty("has-view-from")
  val has_metatheory = ObjectProperty("has-metatheory")
    .subpropertyOf(has_implicit_morphism_from)
  val realizes = ObjectProperty("realizes")
    .subpropertyOf(has_structure_from)
  val constructor = Class("constructor")
    .subclassOf(constant)
  val judgment_constructor = Class("judgment-constructor")
    .subclassOf(function)
  val high_universe = Class("high-universe")
    .subclassOf(universe)

  abstract class Morphism(morph : Option[Class], domtocod : Option[ObjectProperty], codtodom : Option[ObjectProperty]) extends ULOStatement {
    val _domain : MPath
    val _codomain : MPath
    val name : Resource
    override def triples: Seq[(Resource, IRI, Value)] = {
      val tail = morph.toSeq.flatMap(mo => List((name,RDF.TYPE,mo.toIri),(name,domain.toIri,pathToString(_domain)),(name,codomain.toIri,pathToString(_codomain))))
      val mid = domtocod match {
        case Some(v) => (v(_domain,_codomain)).triples ++ tail
        case _ => tail
      }
      codtodom match {
        case Some(v) => (v(_codomain,_domain)).triples ++ mid
        case _ => mid
      }
    }
  }
  case class HasMeta(_codomain : MPath,_domain : MPath) extends Morphism(Some(meta_theory),None,Some(has_metatheory)) {
    val name = iri(pathToString(_codomain).toString + "#meta-theory")
    val relation = meta_theory
  }
  case class ULOView(path : MPath, _domain : MPath, _codomain : MPath) extends Morphism(Some(view),None,Some(has_view_from)) {
    val name = pathToString(path)
  }
  case class ULOStructure(val path : GlobalName, val _domain : MPath, val _codomain : MPath) extends Morphism(Some(structure),None,Some(has_structure_from)) {
    val name = pathToString(path)
  }
  case class ULOInclude(_domain : MPath, _codomain : MPath, _path : Option[GlobalName] = None) extends Morphism(Some(include),None,Some(includes)) {
    val name = pathToString(_path.getOrElse(_codomain ? ComplexStep(_domain)))
  }
}