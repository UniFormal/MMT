package kwarc.info.mmt.tptp

import scala.collection.mutable._
import ptp.SimpleTptpParserOutput._

class TptpToMMT {

  val imports = new HashSet[String]()

  def this(theory: String) = {
    this()
    imports.clear
    if (theory.endsWith(".ax") && !TptpUtils.fileVersion(theory).equals("0"))
      imports.add(theory.replaceFirst("\\+\\d+", "+0"))
  }

  def translate(item: TopLevelItem) = {

  }

  def translate(item: IncludeDirective) = {

  }

  def translate(item: AnnotatedFormula) = {

  }

  def translate(item: Formula) = {

  }

  def translate(item: Atomic) = {

  }

  def translate(item: Term) = {

  }

  def translate(item: Negation) = {

  }

  def translate(item: Binary) = {

  }

  def translate(item: Quantified) = {

  }
}
