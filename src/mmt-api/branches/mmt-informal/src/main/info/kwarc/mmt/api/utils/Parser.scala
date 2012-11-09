package info.kwarc.mmt.api.utils


// TODO delete?
abstract class Token
case object EOF extends Token
case class Keyword(s: String) extends Token
case class Id(s: String) extends Token
case class LineComment(s : String) extends Token

abstract class Language {
   val tokens : List[String]
	val lineComment : List[String]
	val idEnd : String
}

object Twelf extends Language {
    val tokens = List("%sig", "%view", "{", "}", "[", "]", ":=", ":", "=", ".", "%*", "%%", "% ")
	val lineComment = List("%%", "% ")
	val idEnd = "\\s:()[]{}.%"
}

class Source(r : java.io.BufferedReader, lang : Language) {
	var current : String = ""
	var lineNr = 0
	def nextAfter(n: Int) : (Token, Int) = {
		var remainder = current.substring(n)
		var taken = 0
        if (remainder == "") {
     	   val ln = r.readLine
	       if (ln == null) return (EOF, 0) 
	       else {
	          current = current + " " + ln
	          remainder = current.substring(n)
              lineNr += 1
	       }
        }
		//remove whitespace from beginning of remainder, increase taken
		lang.tokens.find(remainder startsWith _) match {
			case Some(t) if lang.lineComment contains t =>
			   (LineComment(remainder.substring(t.length)), taken + remainder.length)
			case Some(t) =>
			   (Keyword(t), t.length)
			case None =>
			   val id = remainder.split(lang.idEnd)(0)
			   (Id(id), id.length)
		}
	}
	def drop(n: Int) {
		current = current.substring(n)
	}
}

class SourceReader(source : Source, pos : Int) {
	def skip(n: Int) : SourceReader = new SourceReader(source, pos + n)
	def next : (Token, Int) = source.nextAfter(pos)
    def cut : SourceReader = {
		source.drop(pos)
		new SourceReader(source, 0)
	}
}

object + {
	def unapply(r: SourceReader) : Option[(SourceReader, Token)] = r.next match {
		case (EOF, _) => None
		case (t, n) => Some((r.skip(n), t))
	}	
}

object Parser {
	def main(args: Array[String]) {
	   val src = new Source(new java.io.BufferedReader(new java.io.FileReader(new java.io.File(args(0)))), Twelf)
	   val rdr = new SourceReader(src, 0)
	   parse(rdr)
	}
	def parse(rdr: SourceReader) {
	   var rest = rdr
	   var continue = true
	   while (continue) {
          rest = rest.cut match {
	  	     case r + Keyword("%sig") + Id(name) + Keyword("=") + Keyword("{") =>
	  	        // start new signature
	  	        r
	  	     case r + EOF =>
	  	        continue = false
	  	        r
          }
	   }
	}
}