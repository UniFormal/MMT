package info.kwarc.mmt.tptp

import scala.collection.mutable._
import tptp.SimpleTptpParserOutput._
import tptp.SimpleTptpParserOutput.Formula._
import info.kwarc.mmt.api.libraries.NullChecker
import info.kwarc.mmt.api.frontend.{FileReport, Controller}
import java.io.File
import info.kwarc.mmt.api.modules.PlainImport
import tptp.TptpParserOutput.TptpInput
import info.kwarc.mmt.api.{LocalPath, DPath, MPath, StructuralElement}

class TptpToMMT {

  val imports = new HashSet[String]()

  def this(theory: String) = {
    this ()
    if (theory.endsWith(".ax") && !TptpUtils.fileVersion(theory).equals("0"))
      imports.add(theory.replaceFirst("\\+\\d+", "+0"))
  }

  def translate(item: TopLevelItem): Option[StructuralElement] = {
    if (item.getKind().equals(TptpInput.Kind.Formula))
      translate((AnnotatedFormula) item)
    else if (item.getKind().equals(TptpInput.Kind.Include))
      translate((IncludeDirective) item)
    else
      error(item)
  }

  def translate(item: IncludeDirective): Option[StructuralElement] = {
    val from = item.getFileName().substring(item.getFileName().indexOf("/") + 1);
    val mfrom = MPath(DPath(xml.URI(item.getFileName)), LocalPath(TptpUtils.removeExtension(from)));
//    imports.add(mfrom, );
    PlainImport()
  }

  def translate(item: AnnotatedFormula): Option[StructuralElement] = {

  }

  def translate(item: Formula): Option[StructuralElement] = {

  }

  def translate(item: Atomic): Option[StructuralElement] = {

  }

  def translate(item: Term): Option[StructuralElement] = {

  }

  def translate(item: Negation): Option[StructuralElement] = {

  }

  def translate(item: Binary): Option[StructuralElement] = {

  }

  def translate(item: Quantified): Option[StructuralElement] = {

  }

  def error(item: Any): Option[StructuralElement] = {
    println("error", "Error translating " + item.toString)
    None
  }
}

object TptpToMMT {
  val CONTROLLER = new Controller(NullChecker, new FileReport(new File("tptp.log")))
}