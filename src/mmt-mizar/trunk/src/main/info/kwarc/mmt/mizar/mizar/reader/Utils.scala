package info.kwarc.mmt.mizar.mizar.reader


import info.kwarc.mmt.mizar.mizar.objects._
import scala.xml._

object UtilsReader {

	def parseSymbols(n : Node)  = {
		ParsingController.dictionary.addSymbols(n.child.map(parseSymbol).toList)
	}

	def parseSymbol(n : Node) : Symbol = {
		val kind = (n \ "@kind").text
		val nr = (n \ "@nr").text.toInt
		val name = (n \ "@name").text
		new Symbol(kind, nr, name)
	}

	def parseFormats(n : Node) : Unit = {
		n.child.filter(x => x.label == "Format").map(parseFormat)
	}

	def parseFormat(n : Node) = {
		val kind = (n \ "@kind").text
		val symbolnr = (n \ "@symbolnr").text.toInt
		val formatnr = (n \ "@nr").text.toInt
		ParsingController.dictionary.addFormatnr(kind, symbolnr, formatnr)
	}
	
	
	def parseSourceRef(n : Node) : SourceRef = {
	  val line = (n \ "@line").text.toInt
	  val col = (n \ "@col").text.toInt
	  SourceRef(line, col)
	}

}
