package info.kwarc.mmt.imps

/* Java Imports */
import java.io._

/* Scala Imports */
import scala.io.Source
import Character.isWhitespace

/* Scala KWARC Imports */
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.parser.SourceRegion
import info.kwarc.mmt.api.parser.SourcePosition

import info.kwarc.mmt.api.utils.Unparsed
import info.kwarc.mmt.api._
import frontend._
import utils._

/* ######### PARSER ######### */

class LispParser
{
    def parse(s: String): LispExp = parse(new Unparsed(s, msg => throw GeneralError(msg)))

    /* Take an Unparsed object (info.kwarc.mmt.api.utils.Unparsed)
     * and parse the heck out of it */
    private def parse(u : Unparsed) : LispExp =
    {
        /* Expression starts at the very beginning */
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
            // TODO: Re-work this into something else other than
            //       an ugly try-catch block

            case ex : StringIndexOutOfBoundsException =>
            {
                // Some printouts for manual inspection, to be removed later
                println("Summary: " + exprs.length + " expressions parsed")
            }
        }

        /* Expression ends after parsing has finished. */
        val sr_end    : SourcePosition = u.getSourcePosition
        val sr_region : SourceRegion   = SourceRegion(sr_start, sr_end)
        val sr        : SourceRef      = SourceRef(null, sr_region)

        /* Actually parse Exps and filter for successes */
        val parsedExprs : List[LispExp] = exprs.map(parseExpression).filter(y => !(y.isEmpty)).map(z => z.get)

        /* Truncate output.t to 0 length */
        val pw = new PrintWriter("output.t");
        pw.close

        /* Print parsed expressions for diff */
        val fw = new FileWriter("output.t", true)
        for (p <- parsedExprs)
        {
            println("\n" + p.toString)
            fw.write(p.toString + "\n\n")
        }
        fw.close()

        /* Return one expression with all the smaller expressions as children */
        return Exp(parsedExprs, sr)
    }

    /* Create an EXP expression from Unparsed object, until brackets
     * are balanced again. This way, it is extremely easy to parse the
     * correct amount of source. Yay Lisp. */
    private def parseExpAndSourceRef (u : Unparsed) : Exp =
    {
        val sourceRef_start : SourcePosition = u.getSourcePosition
        var children : List[Exp] = List.empty

        var open   : Int = 1 /* we started with an open bracket */
        var closed : Int = 0

        /* While expression is still ongoing, keep parsing */
        while (open - closed != 0)
        {
            // TODO: Casematch here more beautiful?
            if (u.head == '"')
            {
                /* String literal parsing (defStrings etc.) */
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
                /* Some children are complete expressions in themselves
                 * necessitating a recursive call here */
                u.next
                val chld : Exp = parseExpAndSourceRef(u)
                children = children ::: List(chld)
                u.next
            }
            else if (u.head == ')')
            {
                /* Don't forget to take care of closed brackets */
                closed += 1
            }
            else if (isWhitespace(u.head))
            {
                /* Skip through whitespace */
                u.next
            }
            else
            {
                /* Some children, as for example but not limited to
                 * names, are just blank strings */
                val sr_start : SourcePosition = u.getSourcePosition

                val str : String = u.takeWhile(c => !(isWhitespace(c)) && c != ')')

                val sr_end    : SourcePosition = u.getSourcePosition
                val sr_region : SourceRegion   = SourceRegion(sr_start, sr_end)
                val sr        : SourceRef      = SourceRef(null, sr_region)

                // TODO: Is this nesting overkill / overcommplicated?
                children = children ::: List(Exp(List(new Str(str)), sr))
            }
        }

        val sourceRef_end    : SourcePosition = u.getSourcePosition
        val sourceRef_region : SourceRegion   = SourceRegion(sourceRef_start, sourceRef_end)
        val sourceRef        : SourceRef      = SourceRef(null, sourceRef_region)
        // TODO: Implement correct MMT-URI

        return Exp(children, sourceRef)
    }

    /* Parse a single EXP expression into a special form or similar (if possible) */
    private def parseExpression (e : Exp) : Option[LispExp] =
    {
        /* Patter matching down/through to appropriate level */
        e.children.head match
        {
            case Exp(cs,s) => cs.head match
            {
                /* toplevel stuff */
                case Str("herald") => var eprime : Option[LispExp] = parseHeralding(e)
                                      if (!(eprime.isEmpty)) { return eprime }

                case Str("load-section") => var eprime : Option[LispExp] = parseLoadSection(e)
                                            if (!(eprime.isEmpty)) { return eprime }

                /* Actual IMPS special forms */
                case Str("def-atomic-sort") => var as : Option[LispExp] = parseAtomicSort(e)
                                               if (!(as.isEmpty)) { return as }

                case Str("def-constant") => var c : Option[LispExp] = parseConstant(e)
                                            if (!(c.isEmpty)) { return c }
                                            
                case Str("def-quasi-constructor") => var qc : Option[LispExp] = parseQuasiConstructor(e)
                                                     if (!(qc.isEmpty)) { return qc }
                                                     
                case Str("def-cartesian-product") => var cp : Option[LispExp] = parseCartesianProduct(e)
                                                     if (!(cp.isEmpty)) { return cp }
                                                     
                case Str("def-imported-rewrite-rules") => var irr : Option[LispExp] = parseImportedRewriteRules(e)
                                                          if (!(irr.isEmpty)) { return irr } 
                                                          
                case Str("def-schematic-macete") => var sm : Option[LispExp] = parseSchematicMacete(e)
                                                    if (!(sm.isEmpty)) { return sm } 

                /* Catchall case */
                case _                      => println("DBG: unrecognised structure, not parsed!")
            }

            case q  => println("DBG: Couldn't parse:\n~~~")
                       println(q.toString + "\n~~~")
        }

        /* Return None of nothing could be parsed */
        return None
    }

    /* ######### Smaller parsers, mosty IMPS special forms ######### */

    /* Parser for IMPS special form def-constants
     * Documentation: IMPS manual pgs. 168, 169 */
    private def parseConstant (e : Exp) : Option[LispExp] =
    {
        // Required arguments
        var name       : Option[String] = None
        var defstring  : Option[String] = None
        var thy        : Option[Theory] = None

        // Optional arguments
        var usages : Option[Usages] = None
        var sort   : Option[Sort]   = None

        val cs : Int = e.children.length

        /* Three arguments minimum because three req. arguments */
        if (cs >= 3)
        {
            /* Parse positional arguments */
            e.children(1) match { case Exp(List(Str(x)), _) => name      = Some(x) }
            e.children(2) match { case Exp(List(Str(y)), _) => defstring = Some(y) }

            /* Parse keyword arguments, these can come in any order */
            var i : Int = 3
            while (cs - i > 0)
            {
                e.children(i) match {
                    case Exp(ds,src) => ds.head match
                    {
                        case Exp(List(Str("theory")),_) => thy    = argParsers.parseTheory(Exp(ds,src))
                        case Exp(List(Str("usages")),_) => usages = argParsers.parseUsages(Exp(ds,src))
                        case Exp(List(Str("sort")),_)   => sort   = argParsers.parseSort(Exp(ds,src))
                        case _                           => ()
                    }
                    case _ => ()
                }
                i += 1
            }

            /* check for required arguments */
            if (name.isEmpty || defstring.isEmpty || thy.isEmpty) { return None }
            else { return Some(Constant(name.get, defstring.get, thy.get, sort, usages, e.src)) }

        } else { return None }
    }

    /* Parser for IMPS special form def-atomic sort
     * Documentation: IMPS manual pgs. 158, 159 */
    private def parseAtomicSort (e : Exp) : Option[LispExp] =
    {
        // Required arguments
        var name : Option[String] = None
        var qss  : Option[String] = None
        var thy  : Option[Theory] = None

        // Optional arguments
        var usages  : Option[Usages]  = None
        var witness : Option[Witness] = None

        val cs : Int = e.children.length

        /* Three arguments minimum because three req. arguments */
        if (cs >= 3)
        {
            /* Parse positional arguments */
            e.children(1) match { case Exp(List(Str(x)), _) => name = Some(x) }
            e.children(2) match { case Exp(List(Str(y)), _) => qss  = Some(y) }

            /* Parse keyword arguments, these can come in any order */
            var i : Int = 3
            while (cs - i > 0)
            {
                e.children(i) match {
                    case Exp(ds,src) => ds.head match
                    {
                        case Exp(List(Str("theory")),_)  => thy     = argParsers.parseTheory(Exp(ds,src))
                        case Exp(List(Str("usages")),_)  => usages  = argParsers.parseUsages(Exp(ds,src))
                        case Exp(List(Str("witness")),_) => witness = argParsers.parseWitness(Exp(ds,src))
                        case _                           => ()
                    }
                    case _ => ()
                }
                i += 1
            }

            /* check for required arguments */
            if (name.isEmpty || qss.isEmpty || thy.isEmpty) { return None }
            else { return Some(AtomicSort(name.get, qss.get, thy.get, usages, witness, e.src)) }

        } else { return None }
    }
    
    /* Parser for IMPS special form def-quasi-constructor
     * Documentation: IMPS manual pgs. 177, 178 */
    private def parseQuasiConstructor (e : Exp) : Option[LispExp] =
    {
        // Required arguments
        var name   : Option[String]   = None
        var expstr : Option[String]   = None
        var lang   : Option[Language] = None

        // Optional arguments
        var fixed  : Option[FixedTheories] = None

        val cs : Int = e.children.length

        /* Three arguments minimum because three req. arguments */
        if (cs >= 3)
        {
            /* Parse positional arguments */
            e.children(1) match { case Exp(List(Str(x)), _) => name   = Some(x) }
            e.children(2) match { case Exp(List(Str(y)), _) => expstr = Some(y) }

            /* Parse keyword arguments, these can come in any order */
            var i : Int = 3
            while (cs - i > 0)
            {
                e.children(i) match {
                    case Exp(ds,src) => ds.head match
                    {
                        case Exp(List(Str("language")),_)       => lang  = argParsers.parseLanguage(Exp(ds,src))
                        case Exp(List(Str("fixed-theories")),_) => fixed = argParsers.parseFixedTheories(Exp(ds,src))
                        case _                           => ()
                    }
                    case _ => ()
                }
                i += 1
            }

            /* check for required arguments */
            if (name.isEmpty || expstr.isEmpty || lang.isEmpty) { return None }
            else { return Some(QuasiConstructor(name.get, expstr.get, lang.get, fixed, e.src)) }

        } else { return None }
    }
    
    /* Parser for IMPS special form def-imported-rewrite-rules
     * Documentation: IMPS manual pg. 169 */
    private def parseImportedRewriteRules (e : Exp) : Option[LispExp] =
    {
        // Required arguments
        var name   : Option[String]   = None

        // Optional arguments
        var srcTheory   : Option[SourceTheory]   = None
        var srcTheories : Option[SourceTheories] = None

        val cs : Int = e.children.length

        if (cs >= 2)
        {
            /* Parse positional arguments */
            e.children(1) match { case Exp(List(Str(x)), _) => name   = Some(x) }

            /* Parse keyword arguments, these can come in any order */
            var i : Int = 2
            while (cs - i > 0)
            {
                e.children(i) match {
                    case Exp(ds,src) => ds.head match
                    {
                        case Exp(List(Str("src-theory")),_)   => srcTheory   = argParsers.parseSourceTheory(Exp(ds,src))
                        case Exp(List(Str("src-theories")),_) => srcTheories = argParsers.parseSourceTheories(Exp(ds,src))
                        case _                                => ()
                    }
                    case _ => ()
                }
                i += 1
            }

            /* check for required arguments */
            if (name.isEmpty || (srcTheory.isEmpty && srcTheories.isEmpty)) { return None }
            else { return Some(ImportedRewriteRules(name.get, srcTheory, srcTheories, e.src)) }

        } else { return None }
    }
    
    /* Parser for IMPS special form def-cartesian-product
     * Documentation: IMPS manual pg. 166 */
    private def parseCartesianProduct (e : Exp) : Option[LispExp] =
    {
        // Required arguments
        var name      : Option[String]       = None
        var sortNames : Option[List[String]] = None
        var thy       : Option[Theory]       = None

        // Optional arguments
        var accs  : Option[Accessors]   = None
        var const : Option[Constructor] = None

        val cs : Int = e.children.length

        /* Three arguments minimum because three req. arguments */
        if (cs >= 3)
        {
            /* Parse positional arguments */
            e.children(1) match { case Exp(List(Str(x)), _) => name   = Some(x) }
            
            var tmplst : List[String] = List.empty
            
            e.children(2) match {
				case Exp(cs, sc) => for (c <- cs) {
					c match {
						case Exp(List(Str(s)),_) => tmplst = tmplst ::: List(s)
						case _                   => ()
					}
				}
				case _ => println("DBG: MISMATCH") ; return None
			}
			if (tmplst != List.empty) { sortNames = Some(tmplst) } else { println("DBG: EMPTYLISt") ; return None }

            /* Parse keyword arguments, these can come in any order */
            var i : Int = 3
            while (cs - i > 0)
            {
                e.children(i) match {
                    case Exp(ds,src) => ds.head match
                    {
                        case Exp(List(Str("constructor")),_) => const = argParsers.parseConstructor(Exp(ds,src))
                        case Exp(List(Str("accessors")),_)   => accs  = argParsers.parseAccessors(Exp(ds,src))
                        case Exp(List(Str("theory")),_)      => thy   = argParsers.parseTheory(Exp(ds,src))
                        case _                               => ()
                    }
                    case _ => ()
                }
                i += 1
            }

            /* check for required arguments */
            if (name.isEmpty || sortNames.isEmpty || thy.isEmpty) { return None }
            else { return Some(CartesianProduct(name.get, sortNames.get, thy.get, const, accs, e.src)) }

        } else { return None }
    }
    
    /* Parser for IMPS special form def-schematic-macete
     * Documentation: IMPS manual pgs. 180, 181 */
    private def parseSchematicMacete (e : Exp) : Option[LispExp] =
    {
		println(e)
		
        // Required arguments
        var name    : Option[String] = None
        var formula : Option[String] = None
        var thy     : Option[Theory] = None

        // Optional arguments
        var nullarg  : Boolean = false
        var transarg : Boolean = false

        val cs : Int = e.children.length

        /* Three arguments minimum because three req. arguments */
        if (cs >= 3)
        {
            /* Parse positional arguments */
            e.children(1) match { case Exp(List(Str(x)), _) => name    = Some(x) }
            e.children(2) match { case Exp(List(Str(y)), _) => formula = Some(y) }

            /* Parse keyword arguments, these can come in any order */
            var i : Int = 3
            while (cs - i > 0)
            {
                e.children(i) match
                {
                    case Exp(ds,src) => ds.head match
                    {	
                        case Exp(List(Str("theory")),_) => thy = argParsers.parseTheory(Exp(ds,src))
                        case (Str("null"))              => nullarg  = true
                        case (Str("transportable"))     => transarg = true
                        case _                          => ()
                    }
                    case _ => ()
                }
                i += 1
            }

            /* check for required arguments */
            if (name.isEmpty || formula.isEmpty || thy.isEmpty) { return None }
            else { return Some(SchematicMacete(name.get, formula.get, thy.get, nullarg, transarg, e.src)) }

        } else { return None }
    }

    /* ######### Tiny parsers, mostly arguments to def-forms ######### */

    /* Parser for IMPS load-section objects
     * used in: toplevel imports */
    private def parseLoadSection (e : Exp) : Option[LoadSection] =
    {
        if (e.children.length == 2)
        {
            e.children(1) match {
                case Exp(List(Str(x)),_) => return Some(LoadSection(x, e.src))
                case _                   => return None
            }
        } else { return None }
    }
    
    /* Parser for IMPS heralding objects
     * used in: toplevel module declaration */
    private def parseHeralding (e : Exp) : Option[Heralding] =
    {
        if (e.children.length == 2)
        {
            e.children(1) match {
                case Exp(List(Str(x)),_) => return Some(Heralding(x, e.src))
                case _                   => return None
            }
        } else { return None }
    }
}

// this might eventually subclass archives.Importer
class IMPSImporter extends ShellExtension("imps")
{
    def helpText = ""

    def run(shell: Shell, args: List[String]) =
    {
        /* Read first argument as filepath, load and parse file */
        println("Scanning file " + args(0))
        val fileContents = Source.fromFile(args(0)).getLines.mkString

        var lspprsr = new LispParser()
        val foo = lspprsr.parse(fileContents)

        true
    }
}
