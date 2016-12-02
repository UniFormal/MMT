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

case class Exp(children : List[(LispExp, SourceRef)]) extends LispExp
case class Str(str : String) extends LispExp

// IMPS Special Forms
case class Constant(name : String, defstr : String, theory : String, sort : Option[String], usages : Option[List[String]]) extends LispExp

class LispParser
{
    def parse(s: String): LispExp = parse(new Unparsed(s, msg => throw GeneralError(msg)))

    private def parse(u: Unparsed): LispExp =
    {
        var exprs : List[(LispExp, SourceRef)] = List.empty

        // Parse as much as possible
        try
        {
            while (true)
            {
                // Skip to next open brace
                u.takeWhile(_ != '(')
                u.next
                val expression = parseExpAndSourceRef(u)
                exprs = exprs ++ List(expression)
            }
        }
        catch
        {
            case ex : StringIndexOutOfBoundsException =>
            {
				println("Summary (" + exprs.length + " expressions):\n")
				
				for (e <- exprs)
				{
					e._1 match {
						case Exp(cs) => println("Expression with " + cs.length + " children!") 
						                for (k <- cs)
						                {
											k._1 match {
												case Str(s) => println("    child: " + s)
												case _      => println("    complete subexpression")
										    }
										}
						case _       => println("~~")
					}
				}
			}
        }

        println("\n#########################\n")

        // Return one expression with all the smaller expressions as children
        return Exp(exprs)
    }

    private def parseExpAndSourceRef (u : Unparsed) : (LispExp, SourceRef) =
    {
        val sourceRef_start : SourcePosition = u.getSourcePosition
        var children : List[(LispExp, SourceRef)] = List.empty

        var open   : Int = 1
        var closed : Int = 0

        while (open - closed != 0)
        {
			println("iteration, head is " + u.head)
			
            if (u.head == '"')
            {
                val sr_start : SourcePosition = u.getSourcePosition

                u.next
                var str : String = u.takeWhile(_ != '"')

                val sr_end    : SourcePosition = u.getSourcePosition
                val sr_region : SourceRegion   = SourceRegion(sr_start, sr_end)
                val sr        : SourceRef      = SourceRef(null, sr_region)

                children = children ++ List((new Str("\"" + str + "\""), sr))
                u.next
            }
            else if (u.head == '(')
            {
                u.next
                val chld : (LispExp, SourceRef) = parseExpAndSourceRef(u)
                
                children = children ++ List(chld)
                u.next
            }
            else if (u.head == ')')
            {
				closed += 1
			}
            else if (isWhitespace(u.head))
            {
                // Skip through whitespace
                u.next
            }
            else
            {
                val sr_start : SourcePosition = u.getSourcePosition

                val str : String = u.takeWhile(c => !(isWhitespace(c)) && c != ')')

                val sr_end    : SourcePosition = u.getSourcePosition
                val sr_region : SourceRegion   = SourceRegion(sr_start, sr_end)
                val sr        : SourceRef      = SourceRef(null, sr_region)

                children = children ++ List((new Str("\"" + str + "\""), sr))
                //u.next
            }
        }

        val sourceRef_end    : SourcePosition = u.getSourcePosition
        val sourceRef_region : SourceRegion   = SourceRegion(sourceRef_start, sourceRef_end)
        val sourceRef        : SourceRef      = SourceRef(null, sourceRef_region)
        // TODO: Implement correct URI

        return (Exp(children), sourceRef)
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
