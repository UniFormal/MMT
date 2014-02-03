package scala.info.kwarc.mmt.api.test

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.utils._

import org.scalatest._
import scala.xml._

//@Ignore
class XMLReaderSpec extends FlatSpec with Matchers {
  val controller = new Controller()
  //controller.handleLine("log console")
  //controller.handleLine("log+ reader")
  
  val docbase = new DPath(URI("http://docs.omdoc.org/test/")) / "examples"
  val modbase = new DPath(URI("http://cds.omdoc.org/")) / "examples"
  val testLocation = "src/test/resources/XMLReaderTest/" 
    
  // documents
  val hottXML = utils.xml.readFile(File(testLocation + "hott.omdoc"))
  val hottURI = docbase / "hott.mmt" 
  val literalnatXML = utils.xml.readFile(File(testLocation + "literalnat.omdoc"))
  val literalnatURI = docbase / "literalnat.omdoc" 
  //theories
  val IntLiteralsXML = utils.xml.readFile(File(testLocation + "IntLiterals.omdoc"))
  val IntLiteralsURI = modbase ? "IntLiterals"
  val NatLiteralsXML = utils.xml.readFile(File(testLocation + "NatLiterals.omdoc"))
  val NatLiteralsURI = modbase ? "NatLiterals"
  val HOTTXML = utils.xml.readFile(File(testLocation + "HOTT.omdoc"))
  val HOTTURI = modbase ? "HOTT"
  "Reading a valid xml/omdoc document" should "not throw an error" in {
    alert("XMLReader requires quite specific utils.readFile to read XML, often fails otherwise")
    controller.xmlReader.readDocument(hottURI, hottXML)(controller.add)
    controller.xmlReader.readDocument(literalnatURI, literalnatXML)(controller.add)
  }
  it should "pass found content to the continuation function (add it to controller)" in  {
    controller.get(hottURI)
    controller.get(literalnatURI)
  }
  "Reading a valid xml/omdoc theory" should "not throw an error" in {
    controller.xmlReader.readDocument(docbase, IntLiteralsXML)(controller.add)
    controller.xmlReader.readDocument(docbase, NatLiteralsXML)(controller.add)
    controller.xmlReader.readDocument(docbase, HOTTXML)(controller.add)
  }  
  it should "pass found content to the continuation function (add it to controller)" in  {
    controller.get(IntLiteralsURI)
    controller.get(NatLiteralsURI)
    controller.get(HOTTURI)
  }
}