package info.kwarc.mmt.mizar.reader


import info.kwarc.mmt.mizar.objects._
import scala.xml._

object UtilsReader {

  def parseSymbols(n : Node)  = {
    n.child.map(parseSymbol).map(ParsingController.dictionary.addSymbol)
  }

	def parseSymbol(n : Node) : Symbol = {
		val kind = (n \ "@kind").text
		val nr = (n \ "@nr").text.toInt
		val name = (n \ "@name").text
		new Symbol(kind, nr, name)
	}

	def parseFormats(n : Node) : Unit = {
		n.child.filter(x => x.label == "Format").map(parseFormat).map(
		  ParsingController.dictionary.addFormat
		)
	}

	def parseFormat(n : Node) = {
		val kind = (n \ "@kind").text
		val symbolnr = (n \ "@symbolnr").text.toInt
		val nr = (n \ "@nr").text.toInt
		val argnr = (n \ "@argnr").text.toInt
		val leftargnr = try {
		  (n \ "@leftargnr").text.toInt
		} catch {
		  case _ : Exception => 0
		}
		
		val rightsymbolnr = try {
		  Some((n \ "@rightsymbolnr").text.toInt)
		} catch {
		  case _ : Exception => None
		}
		new Format(kind, nr, symbolnr, argnr, leftargnr, rightsymbolnr)
		
	}
	
	def parseSourceRef(n : Node) : SourceRef = {
	  val line = (n \ "@line").text.toInt
	  val col = (n \ "@col").text.toInt
	  SourceRef(line, col)
	}

}
