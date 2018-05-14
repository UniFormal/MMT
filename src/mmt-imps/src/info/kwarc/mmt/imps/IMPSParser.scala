package info.kwarc.mmt.imps

/* Scala Imports */
import Character.isWhitespace

/* Scala KWARC Imports */
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.parser.SourceRegion
import info.kwarc.mmt.api.parser.SourcePosition

import info.kwarc.mmt.api.utils.Unparsed
import info.kwarc.mmt.api._
import utils._

/* ######### PARSER ######### */

class IMPSParser
{
  def parse(s: String, uri : URI, js : List[JSONObject]) : TExp = parse(new Unparsed(s, msg => throw GeneralError(msg)), uri, js)

  /* Take an Unparsed object (info.kwarc.mmt.api.utils.Unparsed)
   * and parse the heck out of it */
  private def parse(u : Unparsed, uri : URI, js : List[JSONObject]) : TExp =
  {
    /* Expression starts at the very beginning */
    val sr_start : SourcePosition = u.getSourcePosition
    var exprs : List[Exp] = Nil

    // Parse as much as possible
    try
    {
      while (true)
      {
        // Skip to next open brace or next comment
        u.takeWhile(c => (c != '(') && (c != ';'))
        if (u.head == '(')
        {
          u.next()
          val exp = parseExpAndSourceRef(u, uri)
          exprs = exprs :+ exp
        }
        else
        if (u.head == ';')
        {
          val sref_start : SourcePosition = u.getSourcePosition

          u.next()
          var str : String = ""
          while (u.getnext(1).charAt(0) != '\n') { str = str + u.next }

          val sref_end    : SourcePosition = u.getSourcePosition
          val sref_region : SourceRegion   = SourceRegion(sref_start, sref_end)
          val sref        : SourceRef      = SourceRef(uri, sref_region)

          exprs = exprs :+ Exp(List(Comment(str,Some(sref))), Some(sref))
        }
      }
    }
    catch
      {
        // TODO: Re-work this into something else other than
        //       an ugly try-catch block

        case _: StringIndexOutOfBoundsException =>
      }

    /* Expression ends after parsing has finished. */
    val sr_end    : SourcePosition = u.getSourcePosition
    val sr_region : SourceRegion   = SourceRegion(sr_start, sr_end)
    val sr        : SourceRef      = SourceRef(uri, sr_region)

    /* Actually parse Exps and filter for successes */
    val definedExprs : List[TExp]  = exprs.map(e => parseExpression(e,js)).filter(y => y.isDefined).map(z => z.get)

    //println(definedExprs.toString())

    var successes : Int = 0
    var failures  : Int = 0
    var dummies   : Int = 0
    var ignore    : Int = 0

    for (ex <- definedExprs)
    {
      ex match {
        case Dummy(_)        => dummies   += 1
        case ParseFailure(_) => failures  += 1
        case Ignore(_)       => ignore    += 1
        case _               => successes += 1
      }
    }

    // Some printouts for manual inspection, to be removed later
    println("\n#### Summary for " + uri.toString + ":")
    println("#### " + definedExprs.length + " expressions and subexpressions parsed; " + successes + " successes, " + failures + " explicit failures, " + dummies + " dummies and " + ignore + " ignored forms\n")

    // Set to true for pretty helpful debug output
    val debug : Boolean = false
    if (debug)
    {
      for (ex <- definedExprs) { println(">>> " + ex.getClass.toString) }
    }

    /* Print parsed expressions for diff */
    // for (p <- parsedExprs) { println("\n" + p.toString) }

    /* Return one expression with all the smaller expressions as children */
    Exp(definedExprs, Some(sr))
  }

  /* Create an EXP expression from Unparsed object, until brackets
   * are balanced again. This way, it is extremely easy to parse the
   * correct amount of source. Yay Lisp. */
  private def parseExpAndSourceRef (u : Unparsed, uri : URI) : Exp =
  {
    val sourceRef_start : SourcePosition = u.getSourcePosition
    var children : List[Exp] = List.empty

    var closed : Boolean = false

    /* While expression is still ongoing, keep parsing */
    while (!closed)
    {
      // TODO: Casematch here more beautiful?
      if (u.head == '"')
      {
        /* String literal parsing (defStrings etc.) */
        val sr_start : SourcePosition = u.getSourcePosition

        u.next()
        val str : String = u.takeWhile(_ != '"')

        val sr_end    : SourcePosition = u.getSourcePosition
        val sr_region : SourceRegion   = SourceRegion(sr_start, sr_end)
        val sr        : SourceRef      = SourceRef(uri, sr_region)

        children = children :+ Exp(List(Str("\"" + str + "\"")), Some(sr))
        u.next()
      }
      else if (u.head == '(')
      {
        /* Some children are complete expressions in themselves
         * necessitating a recursive call here */
        u.next()
        val chld : Exp = parseExpAndSourceRef(u, uri)
        children = children :+ chld
        u.next()
      }
      else if (u.head == ')')
      {
        /* end parsing when matching closed bracket was found */
        closed = true
      }
      else if (isWhitespace(u.head))
      {
        /* Skip through whitespace */
        u.next()
      }
      else
      {
        /* Some children, as for example but not limited to
         * names, are just blank strings */
        val sr_start : SourcePosition = u.getSourcePosition

        val str : String = u.takeWhile(c => !isWhitespace(c) && c != ')')

        val sr_end    : SourcePosition = u.getSourcePosition
        val sr_region : SourceRegion   = SourceRegion(sr_start, sr_end)
        val sr        : SourceRef      = SourceRef(uri, sr_region)

        // TODO: Is this nesting overkill / overcommplicated?
        children = children :+ Exp(List(Str(str)), Some(sr))
      }
    }

    val sourceRef_end    : SourcePosition = u.getSourcePosition
    val sourceRef_region : SourceRegion   = SourceRegion(sourceRef_start, sourceRef_end)
    val sourceRef        : SourceRef      = SourceRef(uri, sourceRef_region)

    Exp(children, Some(sourceRef))
  }

  /* Parse a single EXP expression into a special form or similar (if possible) */
  private def parseExpression (e : Exp, js : List[JSONObject]) : Option[TExp] =
  {
    //println(" >>> parsing expression: " + e.children.head.toString)

    /* Patter matching down/through to appropriate level */
    e.children.head match
    {
      case Exp(cs,_) => cs.head match
      {
        /* toplevel stuff */
        case Str("herald") => return Some(Ignore("heralding"))

        case Str("load-section") => return Some(Ignore("load-section"))

        case Str("include-files") => println(" > Dropping (include-files ...)") ; return Some(Ignore("include-files"))
          // val th = controller.get(theorypath : MPath) match { case th : DeclaredTheory => th case _ => throw something }
          // th.getDeclarations, th.getConstants, th.getIncludes : List[MPath]
          // th.getDeclarationsGenerated (unwahrscheinlich)
          // th.getDeclarationsPrimitive (<- )

        case Str("view-expr") => return Some(Dummy("view-expr"))

        /* Actual IMPS special forms */

        case Str("def-algebraic-processor") => return Some(Dummy("def-algebraic-processor"))

        case Str("def-atomic-sort") => return impsDefFormParsers.parseAtomicSort(e, js)

        case Str("def-bnf") => ??? // Defunct

        case Str("def-cartesian-product") => ??? // Defunct

        case Str("def-compound-macete") => return Some(Dummy("def-compund-macete"))

        case Str("def-constant") => return impsDefFormParsers.parseConstant(e, js)

        case Str("def-imported-rewrite-rules") => return impsDefFormParsers.parseImportedRewriteRules(e)

        case Str("def-inductor") => return Some(Dummy("def-inductor"))

        case Str("def-language") => return impsDefFormParsers.parseLanguage(e)

        case Str("def-order-processor") => return Some(Dummy("def-order-processor"))

        case Str("def-primitive-recursive-constant") => return Some(Dummy("def-primitive-recursive-constant"))

        case Str("def-quasi-constructor") => return impsDefFormParsers.parseQuasiConstructor(e)

        case Str("def-record-theory") => return Some(Dummy("def-record-theory"))

        case Str("def-recursive-constant") => return impsDefFormParsers.parseRecursiveConstant(e,js)

        case Str("def-renamer") => return Some(Dummy("def-renamer"))

        case Str("def-schematic-macete") => return impsDefFormParsers.parseSchematicMacete(e)

        case Str("def-script") => return Some(Dummy("def-script"))

        case Str("def-section") => return Some(Dummy("def-section"))

        case Str("def-sublanguage") => return Some(Dummy("def-sublanguage"))

        case Str("def-theorem") => return impsDefFormParsers.parseTheorem(e, js)

        case Str("def-theory") => return impsDefFormParsers.parseTheory(e, js)

        case Str("def-theory-ensemble") => return Some(Dummy("def-theory-ensemble"))

        case Str("def-theory-ensemble-instances") => return Some(Dummy("def-theory-ensemble-instances"))

        case Str("def-theory-ensemble-multiple") => return Some(Dummy("def-theory-ensemble-multiple"))

        case Str("def-theory-ensemble-overloadings") => return Some(Dummy("def-theory-ensemble-overloadings"))

        case Str("def-theory-instance") => return Some(Dummy("def-theory-instance"))

        case Str("def-theory-processors") => return Some(Dummy("def-theory-processors"))

        case Str("def-translation") => return Some(impsDefFormParsers.parseTranslation(e,js))

        case Str("def-transported-symbols") => return Some(Dummy("def-transported-symbols"))

        /* Syntax changers */

        case Str("def-overloading") => println(" > Dummy (def-overloading ...)")   ; return Some(Dummy("def-overloading"))

        case Str("def-parse-syntax") => println(" > Dummy (def-parse-syntax ...)") ; return Some(Dummy("def-parse-syntax"))

        case Str("def-print-syntax") => println(" > Dummy (def-print-syntax ...)") ; return Some(Dummy("def-print-syntax"))

        /* Other meta-commands etc. */

        case Str("set")                     => { println(" > Dropping (set ...)")     ; return Some(Ignore("set")) }
        case Str("define")                  => { println(" > Dropping (define ...)")  ; return Some(Ignore("define")) }
        case Str("comment")                 => { println(" > Dropping (comment ...)") ; return Some(Ignore("comment")) }
        case Str("make-tex-correspondence") => { println(" > Dropping (make-tex-correspondence ...)")     ; return Some(Ignore("Tex-Correspondence")) }

        /* Catchall cases */
        case Str(x) => {
          println(" > PARSEFAILURE " + x)
          return Some(ParseFailure(x))
        }
        case foo => println("DBG: faulty structure? " + foo.toString) ; return None
      }

      case Comment(_, _) => return None /* No action for comment lines. */
      case q  => println("DBG: Couldn't parse:\n~~~") ; println(q.toString + "\n~~~") ; return None
    }

    println(" > META-PARSEFAILURE " + e.toString)

    /* Return None if nothing could be parsed */
    None
  }
}
