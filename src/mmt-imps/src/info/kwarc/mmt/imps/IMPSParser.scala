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

case class Exp(children : List[LispExp], src : SourceRef) extends LispExp
case class Str(str : String) extends LispExp

// small IMPS Special Forms
case class Theory(thy : String)        extends LispExp
case class Sort(srt : String)          extends LispExp
case class Witness(wtnss : String)     extends LispExp
case class Usages(usgs : List[String]) extends LispExp

// larger IMPS Special Forms
case class Heralding(module : String) extends LispExp
case class Constant(name : String, defstr : String, theory : Theory, sort : Option[Sort], usages : Option[List[String]]) extends LispExp
case class AtomicSort(sortName : String, quasiSortString : String, theory : Option[Theory], usages : Option[List[String]], witness : Option[Witness]) extends LispExp

class LispParser
{
    def parse(s: String): LispExp = parse(new Unparsed(s, msg => throw GeneralError(msg)))

    private def parse(u: Unparsed): LispExp =
    {
		val sr_start : SourcePosition = u.getSourcePosition
        var exprs : List[Exp] = List.empty

        // Parse as much as possible
        try
        {
            while (true)
            {
                // Skip to next open brace
                u.takeWhile(_ != '(')
                u.next
                val exp = parseExpAndSourceRef(u)
                exprs = exprs ::: List(exp)
            }
        }
        catch
        {
            case ex : StringIndexOutOfBoundsException =>
            {
				// Some printouts for manual inspection, to be removed later
				println("Summary: " + exprs.length + " expressions parsed")
			}
        }
        
        val sr_end    : SourcePosition = u.getSourcePosition
        val sr_region : SourceRegion   = SourceRegion(sr_start, sr_end)
        val sr        : SourceRef      = SourceRef(null, sr_region)
        
        // Return one expression with all the smaller expressions as children
        return Exp(exprs, sr)
    }

    private def parseExpAndSourceRef (u : Unparsed) : Exp =
    {
        val sourceRef_start : SourcePosition = u.getSourcePosition
        var children : List[Exp] = List.empty

        var open   : Int = 1
        var closed : Int = 0

		// While expression is still ongoing
        while (open - closed != 0)
        {			
            if (u.head == '"')
            {
				// String literal parsing (defStrings etc.)
                val sr_start : SourcePosition = u.getSourcePosition

                u.next
                var str : String = u.takeWhile(_ != '"')

                val sr_end    : SourcePosition = u.getSourcePosition
                val sr_region : SourceRegion   = SourceRegion(sr_start, sr_end)
                val sr        : SourceRef      = SourceRef(null, sr_region)

                children = children ::: List(Exp(List(new Str("\"" + str + "\"")), sr))
                u.next
            }
            else if (u.head == '(')
            {
				// Some children are complete expressions in themselves
				// necessitating a recursive call
                u.next
                val chld : Exp = parseExpAndSourceRef(u)
                children = children ::: List(chld)
                u.next
            }
            else if (u.head == ')')
            {
				// Don't forget to take care of closed brackets
				closed += 1
			}
            else if (isWhitespace(u.head))
            {
                // Skip through whitespace
                u.next
            }
            else
            {
				// Some children, as for example but not limited to
				// names, are just blank strings
                val sr_start : SourcePosition = u.getSourcePosition

                val str : String = u.takeWhile(c => !(isWhitespace(c)) && c != ')')

                val sr_end    : SourcePosition = u.getSourcePosition
                val sr_region : SourceRegion   = SourceRegion(sr_start, sr_end)
                val sr        : SourceRef      = SourceRef(null, sr_region)

                children = children ::: List(Exp(List(new Str(str)), sr))
            }
            // TODO: This doesn't handle lists (e.g. of names, lambdas) correctly, I think
        }

        val sourceRef_end    : SourcePosition = u.getSourcePosition
        val sourceRef_region : SourceRegion   = SourceRegion(sourceRef_start, sourceRef_end)
        val sourceRef        : SourceRef      = SourceRef(null, sourceRef_region)
        // TODO: Implement correct URI

        return Exp(children, sourceRef)
    }
    
    private def parseExpressions (exprs : List[Exp]) : List[LispExp] =
    {
		var parsedExprs : List[LispExp] = List.empty
		
		for (e <- exprs)
		{
			e.children.head match {
				case Str("herald")          => println("herald parsed")
				case Str("def-atomic-sort") => var as : Option[LispExp] = parseAtomicSort(e)
			                                   if (!(as.isEmpty)) {
												   parsedExprs = parsedExprs ::: List(as.get) 
				                                   println("atomic sort parsed")
										       } else { println("atomic sort not parsed") }
				case Str(str)               => println(str)
				case _                      => println("Couldn't parse, adding unaltered Exp")
			}
		}
		
		return parsedExprs
	}
    
    // ######################### Smaller Parsers
    
    private def parseAtomicSort (e : Exp) : Option[LispExp] =
    {
		var name : Option[String] = None
		var qss  : Option[String] = None
		
		var thy     : Option[Theory]       = None
		var usages  : Option[List[String]] = None
		var witness : Option[Witness]      = None
		
		val cs : Int = e.children.length
		if (cs >= 2 && e.children(0) == Str("def-atomic-sort"))
		{
			/* Parse positional arguments */
			e.children(1) match { case Str(x) => name = Some(x) }
			e.children(2) match { case Str(y) => qss  = Some(y) }
			
			/* Parse modifier and keyword arguments */
			var i : Int = 3
			while (cs - i >= 0)
			{
				println("Checking an optional argument!")
				/*
				if (parseTheory(e.children(i)._1)  != None) {thy = parseTheory(e.children(i)._1)}
				if (parseUsages(e.children(i)._1)  != None) {thy = parseUsages(e.children(i)._1)}
				if (parseWitness(e.children(i)._1) != None) {thy = parseWitness(e.children(i)._1)}*/
				i += 1
			}
			
			if (name.isEmpty || qss.isEmpty) { return None }
			else { return Some(AtomicSort(name.get, qss.get, thy, usages, witness)) }
			
		} else { return None }
	}
	
	private def parseConstant (e : Exp) : Option[LispExp] =
	{
		return None
	}
	
	// ######### Tiny parsers
	
	private def parseHeralding (e : Exp) : Option[Heralding] =
	{
		if ((e.children.length == 2) && (e.children(0) == Str("herald"))) {
			e.children(1) match {
				case Str(x) => return Some(Heralding(x))
				case _      => return None
			}
		} else {
			return None
		}
	}
	
	private def parseTheory (e : Exp) : Option[Theory] =
	{
		if ((e.children.length == 2) && (e.children(0) == Str("theory"))) {
			e.children(1) match {
				case Str(x) => return Some(Theory(x))
				case _      => return None
			}
		} else {
			return None
		}
	}
	
	private def parseWitness (e : Exp) : Option[Witness] =
	{
		if ((e.children.length == 2) && (e.children(0) == Str("witness"))) {
			e.children(1) match {
				case Str(x) => return Some(Witness(x))
				case _      => return None
			}
		} else {
			return None
		}
	}
	
	private def parseUsages (e : Exp) : Option[Usages] =
	{
		var usgs : List[String] = List.empty
		
		if ((e.children.length >= 2) && (e.children(0) == Str("usages")))
		{
			var i : Int = 2
			
			while (i <= e.children.length)
			{
				e.children(i) match
				{
					case Str(x) => usgs = usgs ::: List(x)
					case _      => return None
				}
				i += 1;
			}
			return Some(Usages(usgs))
			
		} else {
			return None
		}
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
