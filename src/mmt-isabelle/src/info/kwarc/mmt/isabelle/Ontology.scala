package info.kwarc.mmt.isabelle

import java.net.URI


object Ontology
{
  def unary(subject: URI, predicate: URI): isabelle.RDF.Triple =
    isabelle.RDF.Triple(subject.toString, predicate.toString, Nil)

  def binary(subject: URI, predicate: URI, `object`: URI): isabelle.RDF.Triple =
    isabelle.RDF.Triple(subject.toString, predicate.toString, List(isabelle.XML.Text(`object`.toString)))
}

object ULO
{
  def uri(name: String): URI = new URI("ulo:" + name)


  /* unaries */

  val `type` = uri("type")    // Pure/HOL type
  val data = uri("data")      // term constant

  val proposition = uri("proposition")  // FIXME !?
  val statement = uri("statement")

  val theory = uri("theory")  // theory, locale, class, bundle (?)
  val instance = uri("instance")  // locale interpretation, class instantiation, unbundle (?)

  val section = uri("section")  // document headings: chapter, section, ..., subparagraph
  val file = uri("file")  // auxiliary file (from thy_load command) (?)
  val folder = uri("folder")  // Isabelle session (?)
  val library = uri("library")  // e.g. Isabelle Distribution, AFP, IsaFoR
  val group = uri("group")

  val primitive = uri("primitive")  // e.g. axioms
  val derived = uri("derived")  // e.g. definitions, theorems

  val theorem = uri("theorem")  // kind "theorem"
  val lemma = uri("lemma")  // kind "lemma"
  val conjecture = uri("conjecture")  // unused
  val corollary = uri("corollary")  // kind "corollary"
  // FIXME val proposition = uri("proposition")  // kind "proposition"

  val simplification_rule = uri("simplification-rule")  // fact within simpset (??)

  val automatically_proved = uri("automatically_proved")  // unused!? could be result of "Judgement Day" Sledgehammer exploration
  val experimental = uri("experimental")  // unused!? could be "theorem A oops"
  val deprecated = uri("deprecated")  // unused!? could be command with "legacy_feature" message


  /* binaries */

  val declares = uri("declares")  // theory/locale declares item
  val uses = uri("uses")
  val type_depends_on_definition_of = uri("type-depends-on-definition-of")
  val type_depends_on_type_of = uri("type-depends-on-type-of")
  val definition_depends_on_type_of = uri("definition-depends-on-type-of")
  val definition_depends_on_definition_of = uri("definition-depends-on-definition-of")

  val generated_by = uri("generated-by")
  val inductive_on = uri("inductive-on")
  val mutual_with = uri("mutual-with")  // e.g. Spec_Rules peer group


  // manual markers!?
  val same_as = uri("same-as")
  val similar_to = uri("similar-to")
  val alternative_for = uri("alternative-for")
  val see_also = uri("see-also")
  val antonym_of = uri("antonym-of")

  val formalizes = uri("formalizes")
  val aligned_with = uri("aligned_with")
  val inspired_by = uri("inspired_by")

  val source_ref = uri("source-ref")
  val check_time = uri("check-time")
  val external_size = uri("external-size")  // source size (Isabelle symbols, singleton blanks)
  val internal_size = uri("internal-size")


  /* examples */
  
  // axiomatization ... where  -- statement, primitive
  // theorem A sorry  -- statement
  // theorem A <proof>  -- statement, derived
}