package info.kwarc.mmt.imps

import scala.io.Source
import Character.isWhitespace

import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.parser.SourceRegion
import info.kwarc.mmt.api.parser.SourcePosition

import info.kwarc.mmt.api.utils.Unparsed

import info.kwarc.mmt.api._
import frontend._
import utils._

abstract class LispExp
case class Exp(id: String, children: List[(LispExp, SourceRef)]) extends LispExp

// IMPS Special Forms
case class DefString(str : String) extends LispExp

case class Constant(name : String, defstr : String, theory : String, sort : Option[String], usages : Option[List[String]]) extends LispExp

class LispParser
{

    def parse(s: String): LispExp = parse(new Unparsed(s, msg => throw GeneralError(msg)))

    private def parse(u: Unparsed): LispExp =
    {

        var exprs : List[(LispExp, SourceRef)] = List.empty
        var mdl   : String                     = ""

        u.trim

        // Skip to heralding, everything before is ignored!
        var hrld = false
        while (!hrld)
        {
            // Skip to next open brace
            u.takeWhile(_ != '(')

            if (u.takeWhile(_ != ' ') == "(herald")
            {
                hrld = true
                u.next // skip space
                mdl = u.takeWhile(_ != ')')
                mdl.trim
            }
        }

        // Parse as much as possible
        try
        {
            while (true)
            {
                // Skip to next open brace
                u.takeWhile(_ != '(')
                val sourceRef_start : SourcePosition = u.getSourcePosition
                u.next
                val form = u.takeWhile(c => !(isWhitespace(c)))
                var newExpr : Option[LispExp] = None

                form match {
                    case "def-constant" => val constant : LispExp = parseConstant(u)
                                           newExpr = Some(constant)
					case "def-theorem"  => println("Found a theorem!")
					case "def-theory"   => println("Found a theory!")
                    case _              => println("Found unknown form. Skipping!")
                }

                if (!(newExpr.isEmpty))
                {
                    val sourceRef_end    : SourcePosition = u.getSourcePosition
                    val sourceRef_region : SourceRegion   = SourceRegion(sourceRef_start, sourceRef_end)
                    val sourceRef        : SourceRef      = SourceRef(null, sourceRef_region)

                    exprs :+ (newExpr.get, sourceRef)
                }
			}
        }
        catch
        {
            case ex : StringIndexOutOfBoundsException =>
              { println("\nNo more to parse") }
        }

        // Return one expression with all the smaller expressions as children
        return Exp(mdl, exprs)
    }

    // ######## SMALL PARSERS ########

    private def parseConstant (u : Unparsed) : LispExp =
    {
        println("\n~ Found a constant:")
        u.takeWhile(_ != ' ')

        val nm     : String = parseName(u)
        val defStr : String = parseDefinitionString(u)
        val thry   : String = parseTheoryName(u)

        // Parse optional values
        val rest : String = untilEndOfExpr(u)
        var p : Unparsed = new Unparsed(rest, msg => throw GeneralError(msg))
        var q : Unparsed = new Unparsed(rest, msg => throw GeneralError(msg))
        
        val opt_sort   : Option[String]       = parseSortOption(p)
        val opt_usages : Option[List[String]] = parseUsagesOption(q)

        return new Constant(nm, defStr, thry, opt_sort, opt_usages)
    }

    // ######## TINY PARSERS ########

    private def parseName (u : Unparsed) : String =
    {
        u.takeWhile(isWhitespace _)
        var nm = u.takeWhile(c => !(isWhitespace(c)))
        println("    name:   " + nm)
        return ""
    }

    private def parseDefinitionString (u : Unparsed) : String =
    {
        u.takeWhile(_ != '"')
        u.next
        var strng = u.takeWhile(_ != '"')
        println("    defStr: \"" + strng  + '"')
        return ('"' + strng  + '"')
    }

    private def parseTheoryName (u : Unparsed) : String =
    {
        u.takeWhile(_ != '(')
        u.takeWhile(c => !(isWhitespace(c)))

        var thry : String = untilEndOfExpr(u)
        thry = thry.init

        println("    theory: " + thry)
        return thry
    }

    private def parseSortOption (u : Unparsed) : Option[String] =
    {
		var result : Option[String] = None
		
		try
		{
			var bar : String = ""
			while (!(bar == "(sort"))
			{
				println("bar is " + bar)
				u.takeWhile(_ != '(')
				bar = u.takeWhile(c => !(isWhitespace(c)))
			}
			
			var srt : String = untilEndOfExpr(u)
			srt = srt.init
			println("    sort:   " + srt)
			result = Some(srt)
		}
		catch
        {
            case ex : StringIndexOutOfBoundsException => { }
        }
		
		return result
    }

    private def parseUsagesOption (u : Unparsed) : Option[List[String]] =
    {
		var usgs : Option[List[String]] = None
        return usgs
    }

    // ######## HELPER FUNCTIONS ########

    private def untilEndOfExpr (u : Unparsed) : String =
    {
        var str    : String = ""
        var open   : Int    = 1
        var closed : Int    = 0

        // The expression ends as soon as braces are balanced
        // Lisps are really cool for that fact.
        while (open - closed != 0)
        {
            u.next
            val c : Char = u.head
            str += c

            if (c == '(')
              { open += 1 }
            else if (c == ')')
              { closed += 1 }
        }

        return str
    }
    
    private def parseExpr (u : Unparsed) : LispExp =
    {
		// Ident is from open paren until whitespace
		val ident : String = u.takeWhile(c => !(isWhitespace(c)))
		
		return Exp(ident, List.empty)
	}
}

// this might eventually subclass archives.Importer
class IMPSImporter extends ShellExtension("imps")
{
    def helpText = ""

    def run(shell: Shell, args: List[String]) =
    {
        println("Scanning file " + args(0))
        val fileContents = Source.fromFile(args(0)).getLines.mkString

        var lspprsr = new LispParser()
        val foo = lspprsr.parse(fileContents)

        true
    }
}
