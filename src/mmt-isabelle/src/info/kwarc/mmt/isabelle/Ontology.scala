package info.kwarc.mmt.isabelle

import java.net.URI


object Ontology {
  def unary(subject: String, predicate: String): isabelle.RDF.Triple =
    isabelle.RDF.Triple(subject, predicate, Nil)

  def binary(subject: String, predicate: String, `object`: String): isabelle.RDF.Triple =
    isabelle.RDF.Triple(subject, predicate, List(isabelle.XML.Text(`object`)))

  val ulo: isabelle.XML.Namespace = isabelle.XML.Namespace("ulo", "ulo:")

  def rdf_document(triples: List[isabelle.RDF.Triple]): isabelle.XML.Elem =
    isabelle.RDF.document(isabelle.RDF.triples(triples),
      namespaces = isabelle.RDF.default_namespaces ::: List(ulo))

  object ULO
  {
    /* unaries */

    val `type` = ulo("type") // type constructors
    val data = ulo("data") // term constants

    val proposition = ulo("proposition") // FIXME !?
    val statement = ulo("statement")

    val theory = ulo("theory") // theory, locale
    val instance = ulo("instance") // locale interpretation, class instantiation, unbundle (?)

    val section = ulo("section") // document headings: chapter, section, ..., subparagraph
    val file = ulo("file") // auxiliary file (from thy_load command) (?)
    val folder = ulo("folder") // Isabelle session (?)
    val library = ulo("library") // e.g. Isabelle Distribution, AFP, IsaFoR
    val group = ulo("group")

    val primitive = ulo("primitive") // e.g. axioms
    val derived = ulo("derived") // e.g. definitions, theorems

    val theorem = ulo("theorem") // kind "theorem"
    val lemma = ulo("lemma") // kind "lemma"
    val conjecture = ulo("conjecture") // unused
    val corollary = ulo("corollary") // kind "corollary"
    // FIXME val proposition = uri("proposition")  // kind "proposition"

    val simplification_rule = ulo("simplification-rule") // fact within simpset (??)

    val automatically_proved = ulo("automatically_proved") // unused!? could be result of "Judgement Day" Sledgehammer exploration
    val experimental = ulo("experimental") // unused!? could be "theorem A oops"
    val deprecated = ulo("deprecated") // unused!? could be command with "legacy_feature" message


    /* binaries */

    val declares = ulo("declares") // theory/locale declares item
    val uses = ulo("uses")
    val type_depends_on_definition_of = ulo("type-depends-on-definition-of")
    val type_depends_on_type_of = ulo("type-depends-on-type-of")
    val definition_depends_on_type_of = ulo("definition-depends-on-type-of")
    val definition_depends_on_definition_of = ulo("definition-depends-on-definition-of")

    val generated_by = ulo("generated-by")
    val inductive_on = ulo("inductive-on")
    val mutual_with = ulo("mutual-with") // e.g. Spec_Rules peer group


    // manual markers!?
    val same_as = ulo("same-as")
    val similar_to = ulo("similar-to")
    val alternative_for = ulo("alternative-for")
    val see_also = ulo("see-also")
    val antonym_of = ulo("antonym-of")

    val formalizes = ulo("formalizes")
    val aligned_with = ulo("aligned_with")
    val inspired_by = ulo("inspired_by")

    val source_ref = ulo("source-ref")
    val check_time = ulo("check-time")  // elapsed time (ms)
    val external_size = ulo("external-size")  // source size (UTF-8 bytes)
    val internal_size = ulo("internal-size")


    /* examples */

    // axiomatization ... where  -- statement, primitive
    // theorem A sorry  -- statement
    // theorem A <proof>  -- statement, derived
  }
}