// TODO: catch certain exceptions earlier, e.g. just ignore a module if possible, but parse the rest, or at least return those already parsed

package info.kwarc.mmt.twelf

import java.io.File
import scala.collection.mutable.{ArraySeq, LinkedHashMap, HashSet, LinkedHashSet, ListBuffer}
import URI._

/** Crawl a file */
object FileCrawler {
  /** Crawl a file
  * @param file the file descriptor (not open)
  * @return a Document object with the information extracted from the file, which includes a LinkedList of errors occurred during parsing
  * @throws ParseError for syntactical errors in the file
  * @throws FileOpenError if the file cannot be opened */
  def apply(file : File) : Document = (new FileCrawler(file)).crawl()
}


/** Algorithms for crawling a file
  * @param file the file descriptor */
class FileCrawler(file : File) {

  // ------------------------------- private members -------------------------------

  /** array containing all the lines in the file */
  private var lines : Array[String] = null

  /** all the lines in a single string, with " " instead of newline. */
  private var flat  : String = ""

  /** array of pairs <position in the flat array, the number of the line that starts at this position>. All indexes are 0-based. */
  private implicit var lineStarts: Array[(Int, Int)] = new Array[(Int, Int)](0)

  /** list of parsing errors in the file */
  private var errors : List[ParseError] = Nil

  /** temporary variable used during parsing: saves the last SemanticCommentBlock */
  private var keepComment : Option[SemanticCommentBlock] = None

  /** Read the file and update lines, flat and lineStarts */
  if (!file.canRead())
    throw FileOpenError("error: file cannot be opened")
  try {
    val source = scala.io.Source.fromFile(file, "utf-8")
    lines = source.getLines().toArray                          // get all lines from the file
    source.asInstanceOf[scala.io.BufferedSource].close       // close the file, since scala.io.Source doesn't close it
  } catch {
    case e : Throwable => throw FileOpenError("error: file cannot be opened or the encoding is not UTF-8")
  }
  var lineNumber = 0
  for (x <- lines) {
    lineStarts = lineStarts :+ (flat.length, lineNumber)
    flat += x + "\n"
    lineNumber += 1
  }


  // ------------------------------- main method -------------------------------


  /** Crawl a file.
    * @return a Document object containing the extracted information, which includes a LinkedList of errors occurred during parsing
    * @throws ParseError for syntactical errors in the file */
  private def crawl() : Document =              // line and column numbers start from 0
  {
    // temporary variable used during parsing: the namespace URI which is in effect at the current place in the file
    var currentNS : Option[URI] = None

    // the optional semantic comment associated with the document
    var associatedComment : Option[SemanticCommentBlock] = None

    // theories and views
    var modules = new ListBuffer[ModuleBlock] ()

    // mapping from namespace prefixes to their URI
    var prefixes = new LinkedHashMap[String,URI] ()

    // list of current namespaces declared in the document
    var declaredNamespaces = new LinkedHashSet[URI] ()

    var i = 0                   // position in the flattened file

    // Read the optional semantic comment about the document
    i = skipws(i)
    if (i < flat.length && flat.startsWith("%*", i)) {
      val (comment, positionAfter) = crawlSemanticCommentBlock(i)
      associatedComment = Some(comment)
      i = positionAfter
    }

    keepComment = None          // reset the last semantic comment stored
    i = skipwscomments(i)       // check whether there is a new semantic comment

    while (i < flat.length) {
      if (flat.startsWith("%namespace", i)) {
        val (name, uri, positionAfterBlock) = crawlNamespaceBlock(i)
        i = positionAfterBlock
        if (name.isEmpty) {      // a new current namespace
          if (currentNS.isEmpty)
            currentNS = Some(uri)
          else
            currentNS = Some(currentNS.get.resolve(uri).normalize)
          declaredNamespaces += currentNS.get
        }
        else                     // an alias namespace declaration
          if (currentNS == None)
            throw ParseError(toPair(i, lineStarts).toString + ": error: current namespace must be defined before the namespace alias declaration")
          else {
            val absoluteRemoteURI = currentNS.get.resolve(uri).normalize
            prefixes += ((name.get, absoluteRemoteURI))
          }
      }
      else if (flat.startsWith("%sig", i)) {
        val oldComment = keepComment
        val (block, positionAfterBlock) = crawlSigBlock(i, currentNS, prefixes)      // *** this overwrites keepComment
        if (oldComment != None)
          block.associatedComment = oldComment
        i = positionAfterBlock
        modules += block
      }
      else if (flat.startsWith("%view", i)) {
        val oldComment = keepComment
        val (block, positionAfterBlock) = crawlViewBlock(i, currentNS, prefixes)      // *** this overwrites keepComment
        if (oldComment != None)
          block.associatedComment = oldComment
        i = positionAfterBlock
        modules = modules += block
      }
      else if (flat.startsWith("%", i) && (i < flat.length && isIdentifierPartCharacter(flat.codePointAt(i + 1)))) { // unknown top-level %-declaration => ignore it
        i = skipUntilDot(i)
      }
      else if (flat.startsWith("%.", i))  // this marks the end of file; ignore everything after
        i = flat.length
      else if (isIdentifierPartCharacter(flat.codePointAt(i))) {    // top-level constant declaration => ignore it
        i = skipUntilDot(i)
      }
      else
        throw ParseError(toPair(i, lineStarts).toString + ": error: unknown entity. Module, comment or namespace declaration expected")

      keepComment = None          // reset the last semantic comment stored
      i = skipwscomments(i)       // check whether there is a new semantic comment
    }
    return new Document(new URI(Catalog.getPath(file)), associatedComment, modules, prefixes, declaredNamespaces, errors)
  }


  // ------------------------------- crawl-forward -------------------------------


  /** Jump over a line.
    * @param start a position within a line
    * @return the index of the beginning of the next line */
  private def skipline(start: Int) : Int = {
    try {
      lineStarts.filter(p => (p._1 > start)).head._1
    } catch {
      case e : NoSuchElementException => flat.length     // there is no next line
    }
  }

  /** Jump over white space
    * @param start the starting position, assumed to be white space
    * @return the index of the first non-whitespace character. If the character at the starting position is not a white space, then it returns start */
  private def skipws(start: Int) : Int =
  {
    var i = start
    while (i < flat.length && Character.isWhitespace(flat.charAt(i)))
       i += 1
    return i
  }

  /** Jump over all comments and white spaces.
    * Side-effect: the last %* ... *% comment is saved in keepComment
    * @param start the starting position
    * @return the first position >= start which is NOT white space or part of a comment
    * @throws ParseError for syntactical errors in the comments encountered */
  private def skipwscomments(start: Int) : Int =
  {
    var i = skipws(start)
    var break = false
    while (i < flat.length && !break) {
        if (flat.startsWith("%{", i))     // %{ comment
        i = crawlCommentThrowBlock(i)
      else if (flat.startsWith("%*", i)) {    // %* comment
        val (comment, positionAfter) = crawlSemanticCommentBlock(i)
        keepComment = Some(comment)
        i = positionAfter
      }
      else if (flat.startsWith("%%", i))   // %% comment
        i = skipline(i)
      else if (flat.startsWith("%", i) && (i < flat.length && Character.isWhitespace(flat.charAt(i + 1))))  // %space comment
        i = skipline(i)
      else if (flat.startsWith("%", i) && (i == flat.length || flat.charAt(i + 1) == '\n' || flat.charAt(i + 1) == '\r' || flat.charAt(i + 1) == '\f')) // %newline comment
        i = skipline(i)
      else break = true
      i = skipws(i)
    }
    return i
  }


  /** Jump over a block surrounded by curly brackets.
    * Skips over comments, strings and everything else.
    * @param start the position of an open {
    * @return the position after the corresponding } (note: it might be a whitespace)
    * @throws ParseError if the curly bracket doesn't close */
  private def closeCurlyBracket(start: Int) : Int = {
    var i = start + 1       // skip over the initial bracket
    while (i < flat.length) {
      var c = flat.codePointAt(i)
      if (c == '"')
        i = crawlString(i)._2
      else if (c == '%') {
        var old = i
        i = skipwscomments(old)
        if (i == old)    // no comment started here
          i += 1
      }
      else if (isIdentifierPartCharacter(c))
        i = crawlIdentifier(i)._2
      else if (c == '{')
        i = closeCurlyBracket(i)
      else if (c == '}')
        return i + 1
      else
        i += Character.charCount(c)
    }
    throw ParseError(toPair(start, lineStarts).toString + ": error: left bracket { does not close")
    return -1
  }


  /** Find the first white space.
    * @param start the position where to start looking
    * @return the position of the first white space */
  private def skipUntilWs(start: Int) : Int =
  {
    var i = start
    while (i < flat.length && !Character.isWhitespace(flat.codePointAt(i)))
      i += Character.charCount(flat.codePointAt(i))
    return i
  }


  /** Find the first dot which is not part of an identifier. Useful for jumping over a declaration.
    * Skips over comments and quote-surrounded strings, so dots inside them do not count.
    * @param start the position where to start looking
    * @return the position after the first dot with the desired properties
    * @throws ParseError if end of file was encountered before finding a dot */
  private def skipUntilDot(start: Int) : Int =
  {
    var j = start
    while (j < flat.length) {
      val c = flat.codePointAt(j)
      if (c == '"')
        j = crawlString(j)._2
      else if (c == '%') {
        var old = j
        j = skipwscomments(old)
        if (j == old)    // if this was not the beginning of a comment
          j += Character.charCount(c)
      }
      else if (isIdentifierPartCharacter(c))
        j = crawlIdentifier(j)._2
      else if (c == '{')
        j = closeCurlyBracket(j)
      else if (c == '.')
        return j + 1
      else
        j += Character.charCount(c);
    }
    // We now reached the end of file without encountering a dot
    throw ParseError("error: dot expected before end of file")
  }


  // ------------------------------- peek-forward -------------------------------


  /** Find the position of the given string, after some white space and (possibly) some comments.
    * @param start the starting position
    * @param whatToExpect the string to find
    * @param error the error that will be sent along with the exception if the string after the white space is not whatToExpect
    * @param acceptComments if true, then all comments are ignored. If false, then only white space is allowed before whatToExpect.
    * @return the position of the first character in the whatToExpect string, if it is right after white space (and possibly comments).
    * @throws ParseError if the string after white space is not whatToExpect. */
  private def expectNext(start: Int, whatToExpect: String, error: String, acceptComments: Boolean = true) : Int =
  {
    var i = 0
    if (acceptComments)
      i = skipwscomments(start)
    else
      i = skipws(start)
    if (i == flat.length || /*flat.indexOf(whatToExpect, i) == -1)*/ flat.substring(i, i + whatToExpect.length) != whatToExpect)
      throw ParseError(error)
    return i
  }


  // ------------------------------- object-level -------------------------------


  /** Check whether a character, given as a unicode (UTF-8) code point is valid within an identifier part
    * @param c a Unicode code point
    * @return true iff c not one of .:()[]{}%" or white space */
  private def isIdentifierPartCharacter(c: Int) : Boolean =
    !Character.isWhitespace(c) && c != '.' && c != ':' && c != '(' && c != ')' && c != '[' && c != ']' &&
              c != '{' && c != '}' && c != '%' && c != '"'


  /** Read a quote-surrounded string
    * @param start the position of the quotes at the beginning of the string
    * @return a pair of the string and the position after the final "
    * @throws ParseError if the string does not close */
  private def crawlString(start: Int) : (String, Int) =
  {
    val endsAt = flat.indexOf('"', start + 1)    // position of the final quotes
    if (endsAt == -1)
      throw ParseError(toPair(start, lineStarts).toString + ": error: the string does not close")
    return (flat.substring(start + 1, endsAt), endsAt + 1)
  }


  /** Read an identifier.
    * @param start the position of the first character of the identifier
    * @return Pair(identifier as a string, position after the last character of the identifier)
    * @throws ParseError if the current position does not start an identifier */
  private def crawlIdentifier(start: Int) : (String, Int) =
  {
    var i = start                     // the current position
    var c = flat.codePointAt(i)       // the current code point
    var previousCharacterCount = 1    // the size of the previous code point

    while (i < flat.length && (c == '.' || isIdentifierPartCharacter(c))) {
       previousCharacterCount = Character.charCount(c)
       i += previousCharacterCount
       c = flat.codePointAt(i)
    }
    if (flat.codePointAt(i - previousCharacterCount) == '.')  // dot shouldn't be the last character in the identifier, so it belongs outside it
      i -= previousCharacterCount

    val myId = flat.substring(start, i)
    if (myId.isEmpty)
      throw ParseError(toPair(start, lineStarts).toString + ": error: identifier expected")
    return (myId, i)
  }


  // ------------------------------- namespace declarations -------------------------------


  /** Read a namespace block
    * @param start the position of the initial %
    * @return (Option[alias], URI, position). If this is a namespace alias declaration, then URI is the relative URI that alias points to. If this is an absolute namespace declaration, then the first return value is None and URI is the absolute URI that was read. In all cases, position is the position after the closing dot
    * @throws ParseError for syntactical errors */
  private def crawlNamespaceBlock(start: Int) : (Option[String], URI, Int) =
  {
    var i = skipws(start + "%namespace".length)    // jump over %namespace

    // An absolute URI which becomes the current namespace URI
    if (flat.charAt(i) == '"') {
      val (string, positionAfterString) = crawlString(i)
      var uri : URI = null
      try {
        uri = new URI(string.trim)
      } catch {
        case exc: java.net.URISyntaxException => throw ParseError(toPair(i, lineStarts).toString + ": error: " + exc.getMessage)
      }
      return (None, uri, 1 + expectNext(positionAfterString, ".", toPair(start, lineStarts).toString + ": error: %namespace statement does not end with a dot", false))

    }
    // A namespace alias declaration
    else {
      val (alias, positionAfter) = crawlIdentifier(i)        // read the identifier (alias)
      i = positionAfter
      i = 1 + expectNext(i, "=", toPair(start, lineStarts).toString + ": error: %namespace statement does not have '='", false)
      i = expectNext(i, "\"", toPair(start, lineStarts).toString + ": error: %namespace statement does not have a definiens", false)

      val (string, positionAfterString) = crawlString(i)         // read the URI
      var uri : URI = null
      try {
        uri = new URI(string.trim)
      } catch {
        case exc: java.net.URISyntaxException => throw ParseError(toPair(i, lineStarts).toString + ": error: " + exc.getMessage)
      }
      return (Some(alias), uri, 1 + expectNext(positionAfterString, ".", toPair(start, lineStarts).toString + ": error: %namespace statement does not end with a dot", false))
    }
  }


  // ------------------------------- comments -------------------------------


  /** Jump over a non-semantic %{ comment }%
    * @param start the position of the initial %
    * @return the position after the final %
    * @throws ParseError if the comment does not close */
  private def crawlCommentThrowBlock(start: Int) : Int =
  {
    var i = start + "%{".length
    i = flat.indexOf("}%", i) //TODO handle nested comments
    if (i == -1)
      throw ParseError(toPair(start, lineStarts).toString + ": error: comment does not close")
      return i + "}%".length
  }


  /** Read a semantic %* comment *%
    * @param start the position of the initial %
    * @return The first return value is the structured comment, saved in a block, whose end position is on the final %. The second return value is the position after the block.
    * @throws ParseError if the comment does not close. Syntactical errors are only printed
    */
  private def crawlSemanticCommentBlock(start: Int) : (SemanticCommentBlock, Int) =
  {
    var endsAt : Int = flat.indexOf("*%", start)    // position of the final *
    if (endsAt == -1)
      throw ParseError(toPair(start, lineStarts).toString + ": error: comment does not close")
    endsAt += 1                                     // position of the final %
    val properties = LinkedHashMap[String, String] ()

    val entireComment = flat.substring(start+2, endsAt-1).trim
    val commentLines = entireComment.split("\n")
    var firstPropertyLine = commentLines.indexWhere(_.trim.startsWith("@"))
    if (firstPropertyLine == -1) firstPropertyLine = commentLines.length

    // Add the short comment
    if (firstPropertyLine >= 1 && commentLines(0).trim.nonEmpty)
      properties += (("short", commentLines(0).trim))

    // Add the long comment
    if (firstPropertyLine >= 2)
      if (commentLines.slice(1, firstPropertyLine).mkString("\n").trim.nonEmpty) {
        // Remove initial and final empty lines
        val longCommentLines = commentLines.slice(1, firstPropertyLine)
        val firstNonEmptyLine = longCommentLines.indexWhere(a => a.trim.nonEmpty)
        val lastNonEmptyLine = longCommentLines.lastIndexWhere(a => a.trim.nonEmpty)
        properties += (("long", longCommentLines.slice(firstNonEmptyLine, lastNonEmptyLine + 1).mkString("\n")))
      }

    // Add the key-value properties
    var propertyLines : Array[String] = null
    try {
        for (line <- commentLines.drop(firstPropertyLine).map(_.trim)) {
            if (!line.startsWith("@"))
                throw ParseError(toPair(start, lineStarts).toString + ": error: key-value properties (starting with '@') must be grouped at the end of the comment")
            val keyValue = line.drop(1).trim
            if (keyValue.isEmpty)
                throw ParseError(toPair(start, lineStarts).toString + ": error: empty key in @-starting property")
            var i = 0
            var c = keyValue.codePointAt(i)
            while (!Character.isWhitespace(c) && i < keyValue.length) {
                c = keyValue.codePointAt(i)
                i += Character.charCount(c);
            }
            properties += ((keyValue.take(i).trim, keyValue.drop(i).trim))
        }
    } catch {
        case e : ParseError => errors = (errors :+ e)   // add to the list of errors returned
    }
    return (new SemanticCommentBlock(entireComment, properties,
                                          new Position(toPair(start, lineStarts), toPair(endsAt, lineStarts))),
                 endsAt + 1)
  }


  // ------------------------------- symbol-level -------------------------------


  /** Reads a constant declaration.
    * @param start the position of the first character in the constant identifier
    * @param parentURI the URI of the parent module
    * @return Pair(a CstDecl containing the information from the constant declaration, position after the block)
    * @throws ParseError for syntactical errors */
  private def crawlCstDecl(start: Int, parentURI: URI) : (CstDeclBlock, Int) =
  {
    var i = start
    if (flat.startsWith("%abbrev", i)) {
      i += "%abbrev".length
      i = skipwscomments(i)
    }
    val (cstName, positionAfter) = crawlIdentifier(i)  // read constant name
    i = positionAfter
    val endsAt = skipUntilDot(i) - 1
    val position = new Position(toPair(start, lineStarts), toPair(endsAt, lineStarts))
    val url = new URI(Catalog.getPath(file) + "#" + position)
    return (CstDeclBlock(parentURI ? cstName, url, cstName, position), endsAt + 1)
  }


  /** Reads a structure declaration.
    * @param start the position of the initial % from %struct
    * @param parentURI the URI of the parent module
    * @param currentNS the current namespace
    * @return Pair(a StrDecl containing the information from the structure declaration, position after the block)
    * @throws ParseError for syntactical errors */
  private def crawlStrDecl(start: Int, parentURI: URI, currentNS: Option[URI], prefixes: LinkedHashMap[String,URI]) : (StrDeclBlock, Int) =
  {
    var i = start
    val children = new ListBuffer[AssignmentBlock] ()
    var domain : Option[URI] = None
    if (i + "%struct".length + 2 >= flat.length)
      throw ParseError(toPair(i, lineStarts).toString + ": error: %struct statement does not end")
    i += "%struct".length
    i = skipws(i)
    if (flat.startsWith("%implicit", i))
      i = skipws(i + "%implicit".length)
    val (structureName, positionAfter) = crawlIdentifier(i)   // read structure name
    i = positionAfter
    i = skipwscomments(i)

    if (flat.codePointAt(i) == ':') {
      i += 1
      i = skipws(i)
      val (dom, positionAfterDomain) = crawlIdentifier(i)   // jump over structure domain
      domain = Some(moduleToAbsoluteURI(i, dom, currentNS, prefixes))
      i = positionAfterDomain
      i = skipwscomments(i)
    }

    if (flat.codePointAt(i) == '=') {
      i += 1
      i = skipwscomments(i)
      if (flat.codePointAt(i) == '{') {
        // If the structure is defined via a list of assignments
        if (domain.isEmpty)
          throw ParseError(toPair(start, lineStarts).toString + ": error: structure is defined via a list of assignments, but its domain is not specified")
        i = crawlLinkBody(i, parentURI / structureName, children, new LinkedHashSet[URI](), currentNS, prefixes, false)
      }
      else if (!isIdentifierPartCharacter(flat.codePointAt(i)))
        throw ParseError(toPair(i, lineStarts).toString + ": error: morphism or assignment list expected after '='")
    }
    else if (domain.isEmpty)
      throw ParseError(toPair(start, lineStarts).toString + ": error: structure has no definiens and its domain is not specified")


    val endsAt = skipUntilDot(i) - 1       // skip over %open statement
    val position = new Position(toPair(start, lineStarts), toPair(endsAt, lineStarts))
    val url = new URI(Catalog.getPath(file) + "#" + position)
    return (StrDeclBlock(parentURI ? structureName, url, structureName, children, domain, position), endsAt + 1)
  }


  /** Reads a constant assignment.
    * @param start the position of the first character in the constant identifier
    * @param parentURI the URI of the parent module
    * @return Pair(a CstAssignment containing the information from the constant assignment, position after the block)
    * @throws ParseError for syntactical errors */
  private def crawlCstAssignment(start: Int, parentURI: URI) : (CstAssignmentBlock, Int) =
  {
    var i = start
    val (cstName, positionAfter) = crawlIdentifier(i)  // read constant name
    val constantName = cstName.replaceAll("\\Q.\\E", "/")
    i = positionAfter

    i = expectNext(i, ":=", toPair(i, lineStarts).toString + ": error: ':=' expected")
    i += ":=".length
    val endsAt = skipUntilDot(i) - 1
    val position = new Position(toPair(start, lineStarts), toPair(endsAt, lineStarts))
    val url = new URI(Catalog.getPath(file) + "#" + position)
    return (CstAssignmentBlock(parentURI ? constantName, url, constantName, position), endsAt + 1)
  }


  /** Reads a structure assignment.
    * @param start the position of the initial % from %struct
    * @param parentURI the URI of the parent module
    * @return Pair(a StrAssignment containing the information from the structure assignment, position after the block)
    * @throws ParseError for syntactical errors */
  private def crawlStrAssignment(start: Int, parentURI: URI) : (StrAssignmentBlock, Int) =
  {
    var i = start
    if (i + "%struct".length + 2 >= flat.length)
      throw ParseError(toPair(i, lineStarts).toString + ": error: %struct assignment does not end")
    i += "%struct".length    // jump over %struct
    i = skipwscomments(i)
    val (strName, positionAfter) = crawlIdentifier(i)  // read structure name
    val structureName = strName.replaceAll("\\Q.\\E", "/")
    i = positionAfter

    i = expectNext(i, ":=", toPair(i, lineStarts).toString + ": error: ':=' expected")
    i += ":=".length
    val endsAt = skipUntilDot(i) - 1
    val position = new Position(toPair(start, lineStarts), toPair(endsAt, lineStarts))
    val url = new URI(Catalog.getPath(file) + "#" + position)
    return (StrAssignmentBlock(parentURI ? structureName, url, structureName, position), endsAt + 1)
  }


  // ------------------------------- module-level -------------------------------


  /** Reads a theory body
    * @param start the position of the opening {
    * @param parentURI the URI of the enclosing signature
    * @param children list of constant and structure declarations, updated with the information read in the body
    * @param deps set of dependencies, updated with the information read in the body
    * @param currentNS the current namespace
    * @param prefixes the mapping from aliases to namespaces
    * @return the position after the closing }
    * @throws ParseError for syntactical errors */
  private def crawlSigBody(start: Int, parentURI: URI, children: ListBuffer[DeclBlock], deps: LinkedHashSet[URI], currentNS: Option[URI], prefixes: LinkedHashMap[String,URI]) : Int =
  {
    var i = start + 1       // jump over '{'
    keepComment = None          // reset the last semantic comment stored
    i = skipwscomments(i)       // check whether there is a new semantic comment
    while (i < flat.length) {
      if (flat.startsWith("%infix", i) || flat.startsWith("%prefix", i) || flat.startsWith("%postfix", i))
        i = skipUntilDot(i)      // skip over fixity declaration
      else if (isIdentifierPartCharacter(flat.codePointAt(i)) || flat.startsWith("%abbrev", i)) {
        // Read constant declaration
        val oldComment = keepComment
        val (cstDecl, positionAfter) = crawlCstDecl(i, parentURI)
        if (oldComment != None)
          cstDecl.associatedComment = oldComment
        children += cstDecl
        i = positionAfter
      }
      else if (flat.startsWith("%meta", i)) {
        if (i + "%meta".length + 2 >= flat.length)
          throw ParseError(toPair(i, lineStarts).toString + ": error: %meta statement does not end")
        i += "%meta".length
        i = skipws(i)
        val (metaTheoryName, positionAfter) = crawlIdentifier(i)    // read meta theory name
        deps += moduleToAbsoluteURI(i, metaTheoryName, currentNS, prefixes)
        i = positionAfter
        i = skipUntilDot(i)
      }
      else if (flat.startsWith("%include", i)) {
        if (i + "%include".length + 2 >= flat.length)
          throw ParseError(toPair(i, lineStarts).toString + ": error: %include statement does not end")
        i += "%include".length
        i = skipws(i)
        val (importName, positionAfter) = crawlIdentifier(i)    // read import name
        deps += moduleToAbsoluteURI(i, importName, currentNS, prefixes)
        i = positionAfter
        i = skipUntilDot(i)    // skip over the optional %open statement
      }
      else if (flat.startsWith("%struct", i)) {
        // Read structure declaration
        val oldComment = keepComment
        val (strDecl, positionAfter) = crawlStrDecl(i, parentURI, currentNS, prefixes)
        if (oldComment != None)
          strDecl.associatedComment = oldComment
        children += strDecl
        if (strDecl.domain.isDefined)
          deps.add(strDecl.domain.get)
        i = positionAfter
      }
      else if (flat.startsWith("%", i) && (i < flat.length && isIdentifierPartCharacter(flat.codePointAt(i + 1)))) { // unknown %-declaration => ignore it
        i = skipUntilDot(i)
      }
      else if (flat.codePointAt(i) == '}')
        return i + 1
      else
        throw ParseError(toPair(i, lineStarts).toString + ": error: unknown declaration in signature body")
      keepComment = None          // reset the last semantic comment stored
      i = skipwscomments(i)       // check whether there is a new semantic comment
    }
    return i
  }


  /** Reads a theory.
    * @param start the position of the initial '%'
    * @param currentNS the current namespace
    * @param prefixes the mapping from aliases to namespaces
    * @return Pair(a SigBlock containing the information from the theory, position after the block)
    * @throws ParseError for syntactical errors */
  private def crawlSigBlock(start: Int, currentNS: Option[URI], prefixes: LinkedHashMap[String,URI]) : (SigBlock, Int) =
  {
    var i = skipwscomments(start + "%sig".length)   // jump over %sig
    val (sigName, positionAfter) = crawlIdentifier(i)
    val uri = moduleToAbsoluteURI(i, sigName, currentNS, prefixes)
    i = positionAfter   // jump over identifier
    i = expectNext(i, "=",  toPair(start, lineStarts).toString + ": error: signature does not have '=' after its name")
    i += 1    // jump over "="
    i = expectNext(i, "{",  toPair(start, lineStarts).toString + ": error: signature does not have an initial '{'")

    var children = new ListBuffer[DeclBlock] ()
    var deps = LinkedHashSet[URI] ()
    i = crawlSigBody(i, uri, children, deps, currentNS, prefixes)    // read the { body } of the signature

    val endsAt = expectNext(i, ".", toPair(start, lineStarts).toString + ": error: signature does not end with a dot")
    val position = new Position(toPair(start, lineStarts), toPair(endsAt, lineStarts))
    val url = new URI(Catalog.getPath(file) + "#" + position)
    return (new SigBlock(uri, url, sigName, children, deps, position), endsAt + 1)
  }


  /** Reads a link (view or complex structure) body
    * @param start the position of the opening {
    * @param parentURI the URI of the enclosing view or structure declaration
    * @param children list of constant and structure assignments, updated with the information read in the body
    * @param deps set of dependencies, updated with the information read in the body
    * @param currentNS the current namespace
    * @param prefixes the mapping from aliases to namespaces
    * @param isView true if we are crawling a view, false if we are crawling a structure declaration body
    * @return the position after the closing }
    * @throws ParseError for syntactical errors */
  private def crawlLinkBody(start: Int, parentURI: URI, children: ListBuffer[AssignmentBlock], deps: LinkedHashSet[URI], currentNS: Option[URI], prefixes: LinkedHashMap[String,URI], isView: Boolean) : Int =
  {
    var i = start + 1       // jump over '{'
    keepComment = None          // reset the last semantic comment stored
    i = skipwscomments(i)       // check whether there is a new semantic comment
    while (i < flat.length) {
      if (flat.startsWith("%include", i)) {
        if (i + "%include".length + 2 >= flat.length)
          throw ParseError(toPair(i, lineStarts).toString + ": error: %include statement does not end")
        i += "%include".length    // jump over %include
        i = skipws(i)
        val (importName, positionAfter) = crawlIdentifier(i)    // read import name
        try {
            deps += moduleToAbsoluteURI(i, importName, currentNS, prefixes)
        } catch {
            case e : ParseError => if (isView) throw e else {} // if this is a structure body, just ignore prefixes you don't know
        }
        i = positionAfter
        i = skipUntilDot(i)
      }
      else if (flat.startsWith("%struct", i)) {
        val oldComment = keepComment
        val (strAssignment, positionAfter) = crawlStrAssignment(i, parentURI)
        if (oldComment != None)
          strAssignment.associatedComment = oldComment
        children += strAssignment
        i = positionAfter
      }
      else if (isIdentifierPartCharacter(flat.codePointAt(i))) {
        val oldComment = keepComment
        val (cstAssignment, positionAfter) = crawlCstAssignment(i, parentURI)
        if (oldComment != None)
          cstAssignment.associatedComment = oldComment
        children += cstAssignment
        i = positionAfter
      }
      else if (flat.startsWith("%", i) && (i < flat.length && isIdentifierPartCharacter(flat.codePointAt(i + 1)))) { // unknown %-declaration => ignore it
        i = skipUntilDot(i)
      }
      else if (flat.codePointAt(i) == '}')
        return i + 1
      else
        throw ParseError(toPair(i, lineStarts).toString + ": error: unknown declaration in link body")
      keepComment = None          // reset the last semantic comment stored
      i = skipwscomments(i)       // check whether there is a new semantic comment
    }
    return i
  }


  /** Reads a sequence of linespace-separated signature references, ended by either =, -> or a non-identifier-part-character
    * @param start the position of the first character in the sequence
    * @param currentNS the current namespace
    * @param prefixes the mapping from aliases to namespaces
    * @return Pair(the set of URIs of the signatures in the sequence, position after the sequence)
    * @throws ParseError for syntactical errors */
  private def crawlSignatureUnion(start: Int, currentNS: Option[URI], prefixes: LinkedHashMap[String,URI]) : (LinkedHashSet[URI], Int) =
  {
    var sigs = LinkedHashSet[URI] ()
    var i = start
    var break = false
    while (i < flat.length && isIdentifierPartCharacter(flat.codePointAt(i)) && !break) {
      val (sigRef, positionAfter) = crawlIdentifier(i)    // read signature reference
      if (sigRef == "=" || sigRef == "->")                // the sequence has ended, stop here
        break = true
      if (!break) {
        sigs += moduleToAbsoluteURI(i, sigRef, currentNS, prefixes)    // add its URI to the returned LinkedHashSet
        i = positionAfter  // go to the space after the identifier
        i = skipws(i)      // don't allow comments between two signature references
      }
    }
    val endsAt = i
    return (sigs, endsAt)
  }


  /** Reads a view.
    * @param start the position of the initial '%'
    * @param currentNS the current namespace
    * @param prefixes the mapping from aliases to namespaces
    * @return Pair(a ViewBlock containing the information from the view, position after the block)
    * @throws ParseError for syntactical errors */
  private def crawlViewBlock(start: Int, currentNS: Option[URI], prefixes: LinkedHashMap[String,URI]) : (ViewBlock, Int) =
  {
    var i = start + "%view".length     // jump over %view
    i = skipws(i)
    if (flat.startsWith("%implicit", i))
      i = skipws(i + "%implicit".length)

    val children = new ListBuffer[AssignmentBlock] ()
    val deps = LinkedHashSet[URI] ()

    i = skipwscomments(i)

    val (viewName, positionAfter) = crawlIdentifier(i)         // read view name
    val uri = moduleToAbsoluteURI(i, viewName, currentNS, prefixes)
    i = positionAfter   // jump over name

    i = expectNext(i, ":", toPair(start, lineStarts).toString + ": error: view does not have ':' after its name")
    i += 1    // jump over ":"

    i = skipwscomments(i)

    val (viewDomain, positionAfterDomain) = crawlIdentifier(i)             // read view domain
    val viewDomainURI = moduleToAbsoluteURI(i, viewDomain, currentNS, prefixes)
    deps += viewDomainURI
    i = positionAfterDomain   // jump over domain

    i = "->".length + expectNext(i, "->", toPair(start, lineStarts).toString + ": error: view does not have '->' between domain and codomain")

    i = skipwscomments(i)

    val (viewCodomain, positionAfterCodomain) = crawlSignatureUnion(i, currentNS, prefixes)    // read view codomain
    deps ++= viewCodomain
    i = positionAfterCodomain     // jump over codomain

    i = expectNext(i, "=", toPair(start, lineStarts).toString + ": error: view does not have '=' after its name")
    i += 1     // jump over "="
    i = expectNext(i, "{", toPair(start, lineStarts).toString + ": error: view does not have an initial '{'")

    i = crawlLinkBody(i, uri, children, deps, currentNS, prefixes, true) // read the { body } of the view

    i = expectNext(i, ".", toPair(start, lineStarts).toString + ": error: view does not end with a dot")
    val endsAt = i
    val position = new Position(toPair(start, lineStarts), toPair(endsAt, lineStarts))
    val url = new URI(Catalog.getPath(file) + "#" + position)

    return (new ViewBlock(uri, url, viewName, children, deps, viewDomainURI, viewCodomain, position), endsAt + 1)
  }


  // ------------------------------- auxiliary methods -------------------------------


  private def min(a: Int, b: Int) = if (a <= b) a else b


  /** Convert a module name to its URI. Only namespace prefixes are checked.
    * @param start position of the first character of the module name (for error reporting)
    * @param moduleName the module name as a string
    * @param currentNS the optional current namespace
    * @param prefixes map from aliases to remote absolute URIs
    * @return the absolute URI of the module
    * @throws ParseError if the module name has a prefix and the prefix is not a valid namespace alias, or if the module name has no prefix and the current namespace is not defined */
  private def moduleToAbsoluteURI(start: Int, moduleName: String, currentNS: Option[URI], prefixes: LinkedHashMap[String,URI]) : URI = {
    // the URI of the module is *not* used to compute the absolute URI of its dependencies
    // Replace dots with question marks
    val relativeURI : String = moduleName.trim().replaceAll("\\056", "?")
    val j = relativeURI.indexOf("?")
    // If it has no prefix, it belongs to the current namespace, so simply prepend the current namespace URI
    if (j == -1) {
      if (currentNS == None)
        throw ParseError(toPair(start, lineStarts).toString + ": error: cannot compute an absolute URI for this module " + moduleName + " since no current namespace is defined")
      return currentNS.get ? relativeURI
    }
    // If it has a prefix, it belongs to a different namespace, which must have been declared before in the document
    val prefix : String = relativeURI.substring(0, j)
    if (!prefixes.contains(prefix))                      // check if the alias is known
      throw ParseError(toPair(start, lineStarts).toString + ": error: " + prefix + " is not a valid namespace alias")
    val realURI = prefixes.get(prefix).get
    return new URI(realURI.toString() + relativeURI.substring(j))
  }


  /** Get the (line, column) that represents the same position as the given one-dimensional coordinate
  * @param index the one-dimensional coordinate to be transformed into two-dimensional
  * @param lineStarts array of pairs <position in the flat array, the number of the line that starts at this position>. All indexes are 0-based.
  * @return Pair(line, column) */
  def toPair(index: Int, lineStarts: ArraySeq[(Int, Int)]) : (Int,Int) =
  {
    val pair  = lineStarts.filter(p => (p._1 <= index)).last
    (pair._2, index - pair._1)  // the column may be the bogus space character at the end of the line
  }
}
