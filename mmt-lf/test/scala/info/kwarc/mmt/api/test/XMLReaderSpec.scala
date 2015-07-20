package scala.info.kwarc.mmt.api.test

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.backend._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.utils._
import org.scalatest._

//@Ignore
class XMLReaderSpec extends FlatSpec with Matchers {
  val controller = new Controller()
  val xmlReader = new XMLReader(controller.report)
  //controller.handleLine("log console")
  //controller.handleLine("log+ reader")

  val docbase = new DPath(URI("http://docs.omdoc.org/test/")) / "examples"
  val modbase = new DPath(URI("http://cds.omdoc.org/")) / "examples"
  val testLocation = "test/resources/XMLReaderTest/"

  // documents
  val literalnatXML = utils.xml.readFile(File(testLocation + "literalnat.omdoc"))
  val literalnatURI = docbase / "literalnat.omdoc"
  //theories
  val IntLiteralsXML = utils.xml.readFile(File(testLocation + "IntLiterals.omdoc"))
  val IntLiteralsURI = modbase ? "IntLiterals"
  val NatLiteralsXML = utils.xml.readFile(File(testLocation + "NatLiterals.omdoc"))
  val NatLiteralsURI = modbase ? "NatLiterals"
  "Reading a valid xml/omdoc document" should "not throw an error" in {
    alert("XMLReader requires quite specific utils.readFile to read XML, often fails otherwise")
    xmlReader.readDocument(literalnatURI, literalnatXML)(controller.add)
  }
  it should "pass found content to the continuation function (add it to controller)" in {
    controller.get(literalnatURI)
  }
  "Reading a valid xml/omdoc theory" should "not throw an error" in {
    xmlReader.readDocument(docbase, IntLiteralsXML)(controller.add)
    xmlReader.readDocument(docbase, NatLiteralsXML)(controller.add)
  }
  it should "pass found content to the continuation function (add it to controller)" in {
    controller.get(IntLiteralsURI)
    controller.get(NatLiteralsURI)
  }
}
