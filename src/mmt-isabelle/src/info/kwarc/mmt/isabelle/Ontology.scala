package info.kwarc.mmt.isabelle

import info.kwarc.mmt.api.ContentPath
import java.net.URI


object Ontology
{
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

  val ulo: isabelle.XML.Namespace = isabelle.XML.Namespace("ulo", "https://mathhub.info/ulo")

  def rdf_document(triples: List[isabelle.RDF.Triple]): isabelle.XML.Elem =
    isabelle.RDF.document(isabelle.RDF.triples(triples),
      namespaces = isabelle.RDF.default_namespaces ::: List(ulo))

  object ULO
  {
    /* unaries */

    val `type` = ulo("type") // type constructors
    val `object` = ulo("object") // term constants
    val statement = ulo("statement")  // fact items (thm)

    val theory = ulo("theory") // theory, locale

    val section = ulo("section") // document headings: chapter, section, ..., subparagraph

    val para = ulo("para") // theorem-like statements (e.g. "theorem", "lemma", "axiomatization")
    val definition = ulo("definition") // definitional statements (kind "thy_defn", "thy_goal_defn")

    val file = ulo("file")  // unused: could be theory file
    val folder = ulo("folder")  // unused: could be session
    val library = ulo("library")  // unused: could be "Distribution", "AFP", "IsaFoR" etc.

    val primitive = ulo("primitive") // consts or facts from "axiomatization"
    val derived = ulo("derived") // HOL typedefs, proven statements
    val experimental = ulo("experimental") // proof contains "sorry"


    /* binaries */

    val paratype = ulo("paratype") // name of theorem-like statements as its type (e.g. "theorem")

    val instance_of = ulo("instance-of") // locale interpretation

    val specifies = ulo("specifies") // theory/locale declares item
    val specified_in = ulo("specified-in") // inverse of "specifies"

    val uses = ulo("uses") // dependencies of term language (not proofs)

    val source_ref = ulo("source-ref") // entity position
    val check_time = ulo("check-time") // elapsed time (ms)
    val external_size = ulo("external-size") // source size (UTF-8 bytes)
  }
}