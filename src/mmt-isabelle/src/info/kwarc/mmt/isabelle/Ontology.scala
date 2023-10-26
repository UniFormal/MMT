package info.kwarc.mmt.isabelle

import info.kwarc.mmt.api.ContentPath
import java.net.URI


object Ontology {
  /* relational triples */

  def unary(subject: ContentPath, predicate: String): isabelle.RDF.Triple =
    isabelle.RDF.Triple(subject.toString, predicate)

  def binary(subject: ContentPath, predicate: String, `object`: isabelle.XML.Body): isabelle.RDF.Triple =
    isabelle.RDF.Triple(subject.toString, predicate, `object` = `object`)

  def binary(subject: ContentPath, predicate: String, `object`: String): isabelle.RDF.Triple =
    binary(subject, predicate, List(isabelle.XML.Text(`object`)))

  def binary(subject: ContentPath, predicate: String, resource: ContentPath): isabelle.RDF.Triple =
    isabelle.RDF.Triple(subject.toString, predicate, resource = resource.toString)

  def binary(subject: ContentPath, predicate: String, resource: URI): isabelle.RDF.Triple =
    isabelle.RDF.Triple(subject.toString, predicate, resource = resource.toString)


  /* namespace */

  val ulo: isabelle.XML.Namespace = isabelle.XML.Namespace("ulo", "https://mathhub.info/ulo#")

  def rdf_document(triples: List[isabelle.RDF.Triple]): isabelle.XML.Elem =
    isabelle.RDF.document(isabelle.RDF.triples(triples),
      namespaces = isabelle.RDF.default_namespaces ::: List(ulo))

  object ULO {
    /* unaries */

    val `type` = ulo("type") // type constructors
    val function = ulo("function") // term constants
    val statement = ulo("statement") // fact items (thm)
    val universe = ulo("universe") // type classes (primitive entity)

    val name = ulo("name") // external entity name (xname)
    val predicate = ulo("predicate") // term constants with propositional body type

    val theory = ulo("theory") // theory, locale (subsumes type classes as logical specification)

    val section = ulo("section") // document headings: chapter, section, ..., subparagraph

    val para = ulo("para") // theorem-like statements (e.g. "theorem", "lemma", "axiomatization")
    val definition = ulo("definition") // definitional statements (kind "thy_defn", "thy_goal_defn")

    val file = ulo("file")  // unused: could be theory file
    val folder = ulo("folder")  // unused: could be session
    val library = ulo("library")  // unused: could be "Distribution", "AFP", "IsaFoR" etc.

    val primitive = ulo("primitive") // consts or facts from "axiomatization"
    val derived = ulo("derived") // HOL typedefs, proven statements
    val experimental = ulo("experimental") // proof contains "sorry"

    val important = ulo("important")  // command is tagged as "important"
    val unimportant = ulo("unimportant")  // command is tagged as "unimportant"


    /* binaries */

    val paratype = ulo("paratype") // name of theorem-like statements as its type (e.g. "theorem")

    val instance_of = ulo("instance-of") // locale interpretation

    val inductive_on = ulo("inductive-on") // const is specified via primitive recursion on type
    val coinductive_for = ulo("coinductive-for") // const is specified via primitive co-recursion for type

    val specifies = ulo("specifies") // theory/locale declares item
    val specified_in = ulo("specified-in") // inverse of "specifies"

    val uses = ulo("uses") // dependencies of term and proof constants (identified theorems)

    val defines = ulo("defines") // relation of axiom vs. consts
    val justifies = ulo("justifies") // relation of proof vs. facts

    val sourceref = ulo("sourceref") // entity position
    val check_time = ulo("check-time") // elapsed time (ms)
    val external_size = ulo("external-size") // source size (UTF-8 bytes)
  }
}