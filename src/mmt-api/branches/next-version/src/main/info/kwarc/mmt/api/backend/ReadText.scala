package info.kwarc.mmt.api.backend
import info.kwarc.mmt.api._
import metadata._
import documents._
import modules._
import presentation._
import objects._
import symbols._
import utils._

import scala.collection.mutable._

// TODO second phase should update from fields in structures, as well as all aliases with the correct references. Currently aliases point to originatingLink?name

/** A TextReader parses Twelf (for now) and calls controller.add(e) on every found content element e */
class TextReader(controller : frontend.Controller, report : frontend.Report) extends Reader(controller, report) {


  // ------------------------------- document level -------------------------------


  /** crawls a file.
    * @return a LinkedList of errors that occurred during parsing
    * @throws TextParseError for non-recoverable errors that occurred during parsing
    * note that line and column numbers start from 0 */
  def readDocument(source : scala.io.Source, dpath_ : DPath) : Pair[Document, LinkedList[TextParseError]] =
  {
    // initialization
    lines = source.getLines.toArray                          // get all lines from the file
    source.close       // does this really close the underlying file, which will be a BufferedSource?
    flat = ""
    lineStarts = new ArraySeq [(Int, Int)] (0)
    errors = LinkedList[TextParseError] ()
    keepComment = None
    dpath = dpath_
    currentNS = None
    prefixes = new LinkedHashMap[String,URI] ()

    // read the file and update lines, flat and lineStarts
    var lineNumber = 0
    for (x <- lines) {
      lineStarts = lineStarts :+ Pair(flat.length, lineNumber)
      flat += x + "\n"
      lineNumber += 1
    }

    var i = 0  // position in the flattened file

    // add (empty, for now) narrative document to the controller
    val doc = new Document(dpath)
    add(doc)
    try {
       // add document metadata and source references
       i = skipws(i)
       if (i < flat.length && flat.startsWith("%*", i)) {
         val (comment, positionAfter) = crawlSemanticCommentBlock(i)
         addSemanticComment(doc, Some(comment))
         i = positionAfter
       }
   
       // reset the last semantic comment stored and check whether there is a new semantic comment
       keepComment = None
       i = skipwscomments(i)
   
       while (i < flat.length) {
         if (flat.startsWith("%namespace", i))
           i = crawlNamespaceBlock(i)
         else if (flat.startsWith("%sig", i))
           i = crawlTheory(i)
         else if (flat.startsWith("%view", i))
           i = crawlView(i)
         else if (flat.startsWith("%", i) && (i < flat.length && isIdentifierPartCharacter(flat.codePointAt(i + 1)))) // unknown top-level %-declaration => ignore it
           i = skipAfterDot(i)
         else if (flat.startsWith("%.", i))  // this marks the end of file; ignore everything after
           i = flat.length
         else if (isIdentifierPartCharacter(flat.codePointAt(i)))   // top-level constant declaration => ignore it
           i = skipAfterDot(i)
         else
           throw TextParseError(toPos(i), "unknown entity. Module, comment or namespace declaration expected")
   
         keepComment = None          // reset the last semantic comment stored
         i = skipwscomments(i)       // check whether there is a new semantic comment
       }
       addSourceRef(doc, 0, i)
       Pair(doc, errors)
    } catch {
       case e: TextParseError =>
          // for fatal errors, return partially parsed document
          errors = (errors :+ e)
          Pair(doc, errors)
    }
  }


  // ------------------------------- private vars -------------------------------

  /** array containing all the lines in the file */
  private var lines : Array[String] = null

  /** all the lines in a single string, with " " instead of newline. */
  private var flat  : String = ""

  /** array of pairs <position in the flat array, the number of the line that starts at this position>. All indexes are 0-based. */
  private implicit var lineStarts = new ArraySeq [(Int, Int)] (0)

  /** list of parsing errors in the file */
  private var errors = LinkedList[TextParseError] ()

  /** temporary variable used during parsing: saves the last SemanticCommentBlock */
  private var keepComment : Option[MetaData] = None

  /** physical document path */
  private var dpath : DPath = null

  // temporary variable used during parsing: the namespace URI which is in effect at the current place in the file
  private var currentNS : Option[URI] = None

  // mapping from namespace prefixes to their URI
  private var prefixes = new LinkedHashMap[String,URI] ()



  // ------------------------------- lexer level: advancing -------------------------------


  /** jumps over a line.
    * @param start a position within a line
    * @return the index of the beginning of the next line */
  private def skipline(start: Int) : Int = {
    try {
      lineStarts.filter(p => (p._1 > start)).head._1
    } catch {
      case e : NoSuchElementException => flat.length     // there is no next line
    }
  }

  /** jumps over white space
    * @param start the starting position, assumed to be white space
    * @return the index of the first non-whitespace character. If the character at the starting position is not a white space, then it returns start */
  private def skipws(start: Int) : Int =
  {
    var i = start
    while (i < flat.length && Character.isWhitespace(flat.charAt(i)))
       i += 1
    return i
  }

  /** jumps over all comments and white spaces.
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


  /** jumps over a block surrounded by curly brackets
    * Skips over comments, strings and everything else.
    * @param start the position of an open {
    * @return the position after the corresponding } (note: it might be a whitespace)
    * @throws ParseError if the curly bracket doesn't close */
  private def closeCurlyBracket(start: Int) : Int = closeAnyBracket(start)


  /**jumps over a (), [] or {} block
   * @param start position of the opening bracket, assumed to be either (, [ or {
   * @return position after the matching closing bracket
   * @throw TextParseError if the character at the start position is not an opening bracket,
   * or if the bracket doesn't close or one of the internal brackets doesn't close
   */
  private def closeAnyBracket(start: Int) : Int =
  {
    val openingBracket = flat.codePointAt(start) // either (, [ or {
    if (!(openingBracket == '(' || openingBracket == '[' || openingBracket == '{'))
      throw TextParseError(toPos(start), "error: left bracket expected")
    val closingBracket = openingBracket match {
      case '(' => ')'
      case '[' => ']'
      case '{' => '}'
    }
    var i = start + 1  // jump over the opening bracket
    while (i < flat.length) {
      val c = flat.codePointAt(i)
      if (c == '"') // quote-surrounded string
        i = crawlString(i)._2
      else if (c == '%') { // comment or keyword
        val old = i
        i = skipwscomments(old)
        if (i == old)    // this starts a keyword
          i = crawlIdentifier(i + 1)._2
      }
      else if (isIdentifierPartCharacter(c)) // identifier
        i = crawlIdentifier(i)._2
      else if (c == '(' || c == '[' || c == '{') // bracketed block
        i = closeAnyBracket(i)
      else if (c == ')' || c == ']' || c == '}')
        if (c == closingBracket)      // found the matching right bracket
          return i + 1
        else
          throw TextParseError(toPos(i), "unmatched right bracket " + c)
      else if (c == ':') // type of an expression
        i += 1
      else if (c == '.') // end of a local declaration (not in the language yet; just in case)
        i += 1
      else if (Character.isWhitespace(flat.charAt(i))) // skip over white space
        i = skipws(i)
      else
        throw TextParseError(toPos(i), "illegal character " + c + " at this point")
    }
    i
  }



  /** puts a term into an OMSemiFormal object
   * @param start the position of the first character in the term
   * @return the semiformal object, position after the term
   * @throws TextParseError if there are unmatched right brackets }
   */
  private def crawlTerm(start: Int) : Pair[OMSemiFormal, Int] =
  {
    var i = start
    while (i < flat.length) {
      val c = flat.codePointAt(i)
      if (c == '"') // quote-surrounded string
        i = crawlString(i)._2
      else if (c == '%') { // comment or keyword
        val old = i
        i = skipwscomments(old)
        if (i == old)    // this starts a keyword: since not inside a bracket, this ends the term
          return computeReturnValue
      }
      else if (c == '(' || c == '[' || c == '{') // bracketed block
        i = closeAnyBracket(i)
      else if (c == ')' || c == ']' || c == '}')
        throw TextParseError(toPos(i), "unmatched right bracket " + c)
      else if (c == ':' || c == '.') // this ends the term
        return computeReturnValue
      else if (Character.isWhitespace(flat.charAt(i))) // skip over white space
        i += 1
      else if (isIdentifierPartCharacter(c)) { // identifier
        val (id, posAfter) = crawlIdentifier(i)
        if (id == "=" || id == "#") // this ends the term
          return computeReturnValue
        i = posAfter
      }
      else
        throw TextParseError(toPos(i), "illegal character " + c + " at this point")
    }

    // computes the return value. i is assumed to be the position after the end of the term
    def computeReturnValue = {
      val obj = OMSemiFormal((objects.Text("Twelf", getSlice(start, i - 1))))
      addSourceRef(obj, start, i - 1)
      Pair(obj, i)
    }

    throw TextParseError(toPos(i), "end of file reached while reading term")
  }



  /** finds the first dot which is not part of an identifier. Useful for jumping over a declaration.
    * Skips over comments and quote-surrounded strings, so dots inside them do not count.
    * @param start the position where to start looking
    * @return the position after the first dot with the desired properties
    * @throws TextParseError if end of file was encountered before finding a dot */
  private def skipAfterDot(start: Int) : Int =
  {
    var j = start
    while (j < flat.length) {
      val c = flat.codePointAt(j)
      if (c == '"')
        j = crawlString(j)._2
      else if (c == '%') {
        val old = j
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
    // we now reached the end of file without encountering a dot
    throw TextParseError(toPos(j), "dot expected before end of file")
  }


  /** reads a quote-surrounded string
    * @param start the position of the quotes at the beginning of the string
    * @return the string, position after the final quote
    * @throws TextParseError if the string does not close */
  private def crawlString(start: Int) : Pair[String, Int] =
  {
    val endsAt = flat.indexOf('"', start + 1)    // position of the final quotes
    if (endsAt == -1)
      throw TextParseError(toPos(start), "the string does not close")
    return Pair(flat.substring(start + 1, endsAt), endsAt + 1)
  }


  /** reads an identifier.
    * @param start the position of the first character of the identifier
    * @return Pair(identifier as a string, position after the last character of the identifier)
    * @throws TextParseError if the current position does not start an identifier */
  private def crawlIdentifier(start: Int) : Pair[String, Int] =
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
      throw TextParseError(toPos(start), "identifier expected")
    return Pair(myId, i)
  }


  /** jumps over a specified keyword
    * @param startsAt the position of the first character in the keyword
    * @return the position after the keyword
    * @throw TextParseError if there is something else instead of the keyword, or the file finishes too early */
  private def crawlKeyword(startsAt : Int, keyword : String) : Int = {
    val positionAfter = startsAt + keyword.length
    if (positionAfter + 2 >= flat.length)
      throw TextParseError(toPos(startsAt), keyword + " block does not end")
    else if (!flat.startsWith(keyword, startsAt))
      throw TextParseError(toPos(startsAt), keyword + " expected")
    positionAfter
  }


  // ------------------------------- lexer level: peeking -------------------------------


  /** finds the position of the given string, after some white space and (possibly) some comments.
    * @param start the starting position
    * @param whatToExpect the string to find
    * @param error the error that will be sent along with the exception if the string after the white space is not whatToExpect
    * @param acceptComments if true, then all comments are ignored. If false, then only white space is allowed before whatToExpect.
    * @return the position of the first character in the whatToExpect string
    * @throws TextParseError if the string after white space is not whatToExpect. */
  private def expectNext(start: Int, whatToExpect: String, acceptComments: Boolean = true) : Int =
  {
    var i = 0
    if (acceptComments)
      i = skipwscomments(start)
    else
      i = skipws(start)
    if (i == flat.length || !flat.startsWith(whatToExpect, i))
      throw TextParseError(toPos(start), whatToExpect + " expected")
    return i
  }


  /** checks whether a character, given as a unicode (UTF-8) code point is valid within an identifier part
    * @param c a Unicode code point
    * @return true iff c not one of .:()[]{}%" or white space */
  private def isIdentifierPartCharacter(c: Int) : Boolean =
    !Character.isWhitespace(c) && c != '.' && c != ':' && c != '(' && c != ')' && c != '[' && c != ']' &&
              c != '{' && c != '}' && c != '%' && c != '"'


  // ------------------------------- namespace declarations -------------------------------


  /** reads a namespace block
    * @param start the position of the initial %
    * @return position after the declaration
    * @throws TextParseError for syntactical errors
    * @note  If this is a namespace alias declaration, then URI is the relative URI that alias points to.
    * If this is an absolute namespace declaration, then the first return value is None and URI is the absolute URI that was read.
    * In all cases, position is the position after the closing dot*/
  private def crawlNamespaceBlock(start: Int) : Int =
  {
    var i = skipws(start + "%namespace".length)    // jump over %namespace


    var endsAt : Int = 0
    if (flat.charAt(i) == '"') {
      // An absolute URI which becomes the current namespace URI
      val (string, positionAfterString) = crawlString(i)
      var uri : URI = null
      try {
        uri = URI(string.trim)
      } catch {
        case exc: java.net.URISyntaxException => throw TextParseError(toPos(i), "" + exc.getMessage)
      }
      endsAt = expectNext(positionAfterString, ".", false)
      if (currentNS.isEmpty)
        currentNS = Some(uri)
      else
        currentNS = Some(currentNS.get.resolve(uri).normalize)
    }
    else {
      // A namespace alias declaration
      val (alias, positionAfter) = crawlIdentifier(i)        // read the identifier (alias)
      i = positionAfter
      i = 1 + expectNext(i, "=", false)
      i = expectNext(i, "\"", false)

      val (string, positionAfterString) = crawlString(i)         // read the URI
      var uri : URI = null
      try {
        uri = URI(string.trim)
      } catch {
        case exc: java.net.URISyntaxException => throw TextParseError(toPos(i), "" + exc.getMessage)
      }
      endsAt = expectNext(positionAfterString, ".", false)
      if (currentNS == None)
        throw TextParseError(toPos(i), "current namespace must be defined before the namespace alias declaration")
      else {
        val absoluteRemoteURI = currentNS.get.resolve(uri).normalize
        prefixes += Pair(alias, absoluteRemoteURI)
      }
    }
    return 1 + endsAt
  }


  // ------------------------------- comments -------------------------------


  /** jumps over a non-semantic %{ comment }%
    * @param start the position of the initial %
    * @return the position after the final %
    * @throws TextParseError if the comment does not close */
  private def crawlCommentThrowBlock(start: Int) : Int =
  {
    var i = start + "%{".length
    i = flat.indexOf("}%", i)
    if (i == -1)
      throw TextParseError(toPos(start), "comment does not close")
      return i + "}%".length
  }


  /** reads a semantic %* comment *%
    * @param start the position of the initial %
    * @return The first return value is the structured comment, saved as Metadata, whose end position is on the final %. The second return value is the position after the block.
    * @throws TextParseError if the comment does not close. Syntactical errors are only printed
    */
  private def crawlSemanticCommentBlock(start: Int) : Pair[MetaData, Int] =
  {
    var endsAt : Int = flat.indexOf("*%", start)    // position of the final *
    if (endsAt == -1)
      throw TextParseError(toPos(start), "comment does not close")
    endsAt += 1                                     // position of the final %
    val properties = LinkedHashMap[String, String] ()

    val entireComment = flat.substring(start+2, endsAt-1).trim
    val commentLines = entireComment.split("\n")
    var firstPropertyLine = commentLines.indexWhere(_.trim.startsWith("@"))
    if (firstPropertyLine == -1) firstPropertyLine = commentLines.length

    // Add the short comment
    if (firstPropertyLine >= 1 && commentLines(0).trim.nonEmpty)
      properties += Pair("short", commentLines(0).trim)

    // Add the long comment
    if (firstPropertyLine >= 2)
      if (commentLines.slice(1, firstPropertyLine).mkString("\n").trim.nonEmpty) {
        // Remove initial and final empty lines
        val longCommentLines = commentLines.slice(1, firstPropertyLine)
        val firstNonEmptyLine = longCommentLines.indexWhere(_.trim.nonEmpty)
        val lastNonEmptyLine = longCommentLines.lastIndexWhere(_.trim.nonEmpty)
        properties += Pair("long", longCommentLines.slice(firstNonEmptyLine, lastNonEmptyLine + 1).mkString("\n"))
      }

    // Add the key-value properties
    try {
        for (line <- commentLines.drop(firstPropertyLine).map(_.trim)) {
            if (!line.startsWith("@"))
                throw TextParseError(toPos(start), "key-value properties (starting with '@') must be grouped at the end of the comment")
            val keyValue = line.drop(1).trim
            if (keyValue.isEmpty)
                throw TextParseError(toPos(start), "empty key in @-starting property")
            var i = 0
            var c = keyValue.codePointAt(i)
            while (!Character.isWhitespace(c) && i < keyValue.length) {
                c = keyValue.codePointAt(i)
                i += Character.charCount(c);
            }
            properties += Pair(keyValue.take(i).trim, keyValue.drop(i).trim)
        }
    } catch {
        case e : TextParseError => errors = (errors :+ e)   // add to the list of errors returned
    }

    return Pair(MetaData(properties.map(keyValue => new MetaDatum(Path.parseS("??" + keyValue._1, TextReader.metadataBase), OMSTR(keyValue._2))).toSeq : _*), endsAt + 1)
  }



  // ------------------------------- object level -------------------------------



  /** Reads a sequence of linespace-separated module references, ended by either =, -> or a non-identifier-part-character
    * @param start the position of the first character in the sequence
    * @return Pair(the set modules URIs in the sequence, position after the sequence)
    * @throws TextParseError for syntactical errors */
  private def crawlModuleReferences(start: Int) : Pair[LinkedList[URI], Int] =
  {
    var moduleRefs = LinkedList[URI] ()
    var i = start
    var break = false
    while (i < flat.length && isIdentifierPartCharacter(flat.codePointAt(i)) && !break) {
      val (moduleRef, positionAfter) = crawlIdentifier(i)    // read module reference
      if (moduleRef == "->" || moduleRef == "=")          // the sequence has ended, stop here
        break = true
      if (!break) {
        moduleRefs :+ moduleToAbsoluteURI(i, moduleRef)    // add its URI to the returned LinkedHashSet
        i = positionAfter  // go to the space after the identifier
        i = skipws(i)      // don't allow comments between two module references
      }
    }
    val endsAt = i
    return Pair(moduleRefs, endsAt)
  }


  /**reads a morphism object
   * @param start the start position of the morphism object
   * @param theory the theory in which the module references might be in (they might also be top-level views)
   * @return Pair[Term, position after]. For now, the Term is just an informal object, see note below
   * @throws TextParseError for syntactical errors
   * Note: The reason why this returns an informal object is that in views, it has no way
   * to know whether a morphism reference is a structure in the codomain
   * or a top-level morphism in a particular namespace, since both the entire namespace and
   * the codomain might not have been read yet into the library
   */
  private def crawlMorphismObject(start: Int, theory: Option[Term]) : Pair[Term, Int] =
  {
    val (linkRefs, positionAfter) = crawlModuleReferences(start)
    val morphism = OMSemiFormal((objects.Text("Twelf", getSlice(start, positionAfter - 1))))
      /*if (linkRefs.size == 1)   // a single reference
        OMMOD(theory.path)
      else                      // a sequence of references
        OMCOMP(linkRefs.map(uri => OMMOD(Path.parseM(uri.toString, base))) : _*)    */
    addSourceRef(morphism, start, positionAfter - 1)
    Pair(morphism, positionAfter)
  }


  /**reads a theory object
   * @param start the start position of the theory object
   * @param base the path of the content element in which the object resides
   * @return Pair[TheoryObj, position after]
   * @throws TextParseError for syntactical errors
   */
  private def crawlTheoryObject(start: Int, base: Path) : Pair[Term, Int] =
  {
    val (linkRefs, positionAfter) = crawlModuleReferences(start)
    val theories : LinkedList[Term] = linkRefs.map(uri => OMMOD(Path.parseM(uri.toString, base)))
    val theoryObject : Term = {
      if (theories.size == 1)
        theories.head
      else
        TUnion(theories.toList)
    }
    addSourceRef(theoryObject, start, positionAfter - 1)
    Pair(theoryObject, positionAfter)
  }



  // ------------------------------- symbol level -------------------------------


  /** reads a constant declaration.
    * @param start the position of the first character in the constant identifier
    * @param parent the parent theory
    * @return position after the block
    * @throws TextParseError for syntactical errors */
  private def crawlConstantDeclaration(start: Int, parent: Theory) : Int =
  {
    val oldComment = keepComment

    var i = start
    if (flat.startsWith("%abbrev", i)) {
      i += "%abbrev".length
      i = skipwscomments(i)
    }
    val (cstName, positionAfter) = crawlIdentifier(i)  // read constant name
    i = positionAfter
    i = skipwscomments(i)

    // read the optional type
    var constantType : Option[OMSemiFormal] = None
    if (flat.codePointAt(i) == ':') {
      i += 1  // jump over ':'
      i = skipwscomments(i)
      val (term, posAfter) = crawlTerm(i)
      constantType = Some(term)
      i = posAfter
      i = skipwscomments(i)
    }

    // read the optional definition
    var constantDef : Option[OMSemiFormal] = None
    if (flat.codePointAt(i) == '=') {
      i += 1  // jump over '='
      i = skipwscomments(i)
      val (term, posAfter) = crawlTerm(i)
      constantDef = Some(term)
      i = posAfter
      i = skipwscomments(i)
    }

    // read the optional notation
    var constantNotation : Option[NotationProperties] = None
    if (flat.codePointAt(i) == '#') {
      i += 1  // jump over '='
      i = skipwscomments(i)
      val (term, posAfter) = crawlTerm(i)

      val notationProperties = Notation.parseInlineNotation(term.toString)
      addSourceRef(notationProperties, i, posAfter - 1)
      constantNotation = Some(notationProperties)

      i = posAfter
      i = skipwscomments(i)
    }

    val endsAt = expectNext(i, ".")

    // create the constant object
    val constant = new Constant(parent.toTheoryObj, LocalName(cstName), constantType, constantDef, None, constantNotation)
    addSourceRef(constant, start, endsAt)
    addSemanticComment(constant, oldComment)
    add(constant)
    return endsAt + 1
  }


  /** reads an %open block
    * @param start the position of the initial % from %open
    * @param link the structure declaration before the %open statement
    * @return position after the block, i.e. ideally on the '.'
    * @throws TextParseError for syntactical errors
    * note: if link's domain is not yet known (link.from == null), the alias points to link.toMorph ? name */
  private def crawlOpen(start: Int, link: Link) : Int =
  {
    var i = crawlKeyword(start, "%open")

    // reset comment and check for new comments
    keepComment = None
    i = skipwscomments(i)

    while (isIdentifierPartCharacter(flat.codePointAt(i))) {
      // save the last comment
      val oldComment = keepComment

      // read symbol reference
      val startOfAlias = i
      val (ref, positionAfterRef) = crawlIdentifier(i)
      var endOfAlias = positionAfterRef - 1 // in case there is no %as statement
      val refGlobalName = GlobalName(if (link.from != null) link.from else link.toMorph, LocalName(ref))

      // reset comment and check for new comments, in case there is no %as statement
      keepComment = None
      i = skipwscomments(positionAfterRef)

      // read optional %as statement, and the alias introduced
      var newLocalName : LocalName = null
      if (flat.startsWith("%as", i)) {
        i = crawlKeyword(i, "%as")
        i = skipwscomments(i)

        // read new local name
        val (identifier, positionAfterId) = crawlIdentifier(i)
        newLocalName = LocalName(identifier)
        endOfAlias = positionAfterId - 1

        // reset comment and check for new comments
        keepComment = None
        i = skipwscomments(positionAfterId)
      }
      else // otherwise, the alias is simply the old name
        newLocalName = LocalName(ref)

      // construct the alias
      val alias = new Alias(link.to, newLocalName, refGlobalName)

      // add metadata and source references
      addSemanticComment(alias, oldComment)
      addSourceRef(alias, startOfAlias, endOfAlias)

      // add to controller
      add(alias)
    }
    return i
  }


  /** reads an %include declaration
    * @param start the position of the initial % from %include
    * @param parent the parent theory
    * @return position after the block
    * @throws TextParseError for syntactical errors */
  private def crawlIncludeDeclaration(start: Int, parent: Theory) : Int =
  {
    val oldComment = keepComment
    var i = crawlKeyword(start, "%include")
    i = skipws(i)
    val (importName, positionAfter) = crawlIdentifier(i)    // read import name
    val from : URI = moduleToAbsoluteURI(i, importName)
    i = positionAfter

    // create the Include object
    val include = Include(parent.toTheoryObj, OMMOD(Path.parseM(from.toString, parent.path)))

    // read the optional %open statement
    i = skipwscomments(i)
    if (flat.startsWith("%open", i))
      i = crawlOpen(i, include)

    val endsAt = expectNext(i, ".")

    // add metadata and source references
    addSemanticComment(include, oldComment)
    addSourceRef(include, start, endsAt)

    add(include)
    return endsAt + 1
  }


  /** reads a %meta declaration
    * @param start the position of the initial % from %meta
    * @param parent the parent theory
    * @return position after the block
    * @throws TextParseError for syntactical errors */
  private def crawlMetaDeclaration(start: Int, parent: DeclaredTheory) : Int =
  {
    var i = skipws(crawlKeyword(start, "%meta"))
    val (metaTheoryName, positionAfter) = crawlIdentifier(i)    // read meta theory name
    parent.meta = Some(Path.parseM(moduleToAbsoluteURI(i, metaTheoryName).toString, parent.path))
    i = positionAfter
    val endsAt = expectNext(i, ".")
    return endsAt + 1
  }


  /** reads a structure declaration.
    * @param start the position of the initial % from %struct
    * @param parent the parent theory
    * @return position after the block
    * @throws TextParseError for syntactical errors */
  private def crawlStructureDeclaration(start: Int, parent: Theory) : Int =
  {
    var domain : Option[URI] = None
    var isImplicit : Boolean = false

    val oldComment = keepComment

    var i = skipws(crawlKeyword(start, "%struct"))

    // parse the implicit keyword
    if (flat.startsWith("%implicit", i)) {
      i = skipws(crawlKeyword(i, "%implicit"))
      isImplicit = true                      // TODO %implicit keyword in structures ignored for now
    }

    // parse the name
    val (name, positionAfter) = crawlIdentifier(i)
    i = positionAfter
    i = skipwscomments(i)

    // parse the optional domain
    if (flat.codePointAt(i) == ':') {
      i += 1
      i = skipws(i)
      val (dom, positionAfterDomain) = crawlIdentifier(i)   // jump over structure domain
      domain = Some(moduleToAbsoluteURI(i, dom))
      i = positionAfterDomain
      i = skipwscomments(i)
    }

    // check if it's defined via a morphism or a list of assignments
    var structure : Structure = null
    if (flat.codePointAt(i) == '=') {
      i += 1
      i = skipwscomments(i)
      if (flat.codePointAt(i) == '{') {
        // It's a DeclaredStructure
        domain match {
          case None => throw TextParseError(toPos(start), "structure is defined via a list of assignments, but its domain is not specified")
          case Some(dom)  =>
            structure = new DeclaredStructure(parent.toTheoryObj, LocalName(name), OMMOD(Path.parseM(dom.toString, parent.path)))
            add(structure)
            i = crawlLinkBody(i, structure.asInstanceOf[DeclaredStructure])
        }
      }
      else if (!isIdentifierPartCharacter(flat.codePointAt(i)))
        throw TextParseError(toPos(i), "morphism or assignment list expected after '='")
      else {
        // It's a DefinedStructure
        val (morphism, positionAfter) = crawlMorphismObject(i, Some(OMMOD(parent.path)))
        i = positionAfter
        domain match {
          case Some(dom) => structure = new DefinedStructure(parent.toTheoryObj, LocalName(name), OMMOD(Path.parseM(dom.toString, parent.path)), morphism)
          case None => structure = new DefinedStructure(parent.toTheoryObj, LocalName(name), null, morphism)
        }

        add(structure)
      }
    }
    else domain match {
      case None => throw TextParseError(toPos(start), "structure has no definiens and its domain is not specified")
      case Some(dom) =>
        // It's a DeclaredStructure with empty body
        structure = new DeclaredStructure(parent.toTheoryObj, LocalName(name), OMMOD(Path.parseM(dom.toString, parent.path)))
        add(structure)
    }

    // read the optional %open statement
    i = skipwscomments(i)
    if (flat.startsWith("%open", i))
      i = crawlOpen(i, structure)

    val endsAt = expectNext(i, ".")

    // add the semantic comment and source reference
    addSemanticComment(structure, oldComment)
    addSourceRef(structure, start, endsAt)

    return endsAt + 1
  }


  /** Reads a constant assignment.
    * @param start the position of the first character in the constant identifier
    * @param parent the parent link
    * @return position after the block
    * @throws TextParseError for syntactical errors */
  private def crawlConstantAssignment(start: Int, parent: Link) : Int =
  {
    val oldComment = keepComment
    var i = start

    // read constant name
    val (cstName, positionAfter) = crawlIdentifier(i)
    val constantName = cstName.replaceAll("\\Q.\\E", "/")    // TODO replace . with / in names?
    i = positionAfter

    i = expectNext(i, ":=")
    i += ":=".length
    i = skipwscomments(i)

    // read the assigned term
    val (term, posAfter) = crawlTerm(i)
    i = posAfter
    i = skipwscomments(i)

    val endsAt = expectNext(i, ".")

    // add the constant assignment to the controller
    val constantAssignment = new ConstantAssignment(parent.toMorph, LocalName(constantName), term)
    add(constantAssignment)

    // add semantic comment and source references
    addSemanticComment(constantAssignment, oldComment)
    addSourceRef(constantAssignment, start, endsAt)

    return endsAt + 1
  }


  /** Reads a structure assignment.
    * @param start the position of the initial %
    * @param parent the parent link
    * @return position after the block
    * @throws TextParseError for syntactical errors */
  private def crawlStructureAssignment(start: Int, parent: Link) : Int =
  {
    val oldComment = keepComment
    var i = skipws(crawlKeyword(start, "%struct"))

    // read structure name
    val (strName, positionAfter) = crawlIdentifier(i)
    val structureName = strName.replaceAll("\\Q.\\E", "/")   // TODO replace . with / in names?
    i = positionAfter

    i = expectNext(i, ":=")
    i += ":=".length
    i = skipwscomments(i)

    // get the morphism
    val (morphism, positionAfter2) = crawlMorphismObject(i, Some(OMMOD(parent.toMorph.toMPath)))

    val endsAt = expectNext(positionAfter2, ".")    // on the final dot

    // add the structure assignment to the controller
    val defLinkAssignment = new DefLinkAssignment(parent.toMorph, LocalName(strName), morphism)
    add(defLinkAssignment)

    // add semantic comment and source references
    addSemanticComment(defLinkAssignment, oldComment)
    addSourceRef(defLinkAssignment, start, endsAt)

    return endsAt + 1
  }


  /** Reads an include in a link.
    * @param start the position of the initial % from %include
    * @param parent the parent link
    * @return position after the block
    * @throws TextParseError for syntactical errors */
  private def crawlIncludeAssignment(start: Int, parent: Link) : Int =
  {
    val oldComment = keepComment
    val i = skipws(crawlKeyword(start, "%include"))

    // get the morphism
    val (morphism, positionAfter) = crawlMorphismObject(i, Some(OMMOD(parent.toMorph.toMPath)))

    val endsAt = expectNext(positionAfter, ".")    // on the final dot

    // compute the name of the include
    val localName = LocalName("")                         // TODO have an "update" method in controller for setting the name of an include, since it can't be determined now

    // add the include assignment to the controller
    val defLinkAssignment = new DefLinkAssignment(parent.toMorph, localName, morphism)
    add(defLinkAssignment)

    // add semantic comment and source references
    addSemanticComment(defLinkAssignment, oldComment)
    addSourceRef(defLinkAssignment, start, endsAt)

    return endsAt + 1
  }


  // ------------------------------- module level -------------------------------



  /** Reads a theory body
    * @param start the position of the opening {
    * @param parent the enclosing DeclaredTheory
    * @return the position after the closing }
    * @throws TextParseError for syntactical errors */
  private def crawlTheoryBody(start: Int, parent: DeclaredTheory) : Int =
  {
    var i = start + 1       // jump over '{'
    keepComment = None          // reset the last semantic comment stored
    i = skipwscomments(i)       // check whether there is a new semantic comment
    var foundMeta = false
    while (i < flat.length) {
      if (flat.startsWith("%infix", i) || flat.startsWith("%prefix", i) || flat.startsWith("%postfix", i))
        i = skipAfterDot(i)      // skip over fixity declaration           TODO fixity declarations
      else if (isIdentifierPartCharacter(flat.codePointAt(i)) || flat.startsWith("%abbrev", i)) {
        // read constant declaration
        i = crawlConstantDeclaration(i, parent)
      }
      else if (flat.startsWith("%meta", i)) {
        // ensure the meta declaration is unique
        if (foundMeta == true) throw TextParseError(toPos(i), "second %meta statement in theory " + parent.name)
        else foundMeta = true
        i = crawlMetaDeclaration(i, parent)
      }
      else if (flat.startsWith("%include", i)) {
        // read include declaration
        i = crawlIncludeDeclaration(i, parent)
      }
      else if (flat.startsWith("%struct", i)) {
        // read structure declaration
        i = crawlStructureDeclaration(i, parent)
      }
      else if (flat.startsWith("%", i) && (i < flat.length && isIdentifierPartCharacter(flat.codePointAt(i + 1)))) { // unknown %-declaration => ignore it
        i = skipAfterDot(i)
      }
      else if (flat.codePointAt(i) == '}')    // end of signature body
        return i + 1
      else
        throw TextParseError(toPos(i), "unknown declaration in signature body")
      keepComment = None          // reset the last semantic comment stored
      i = skipwscomments(i)       // check whether there is a new semantic comment
    }
    return i
  }


  /** Reads a theory.
    * @param start the position of the initial '%'
    * @return position after the block
    * @throws TextParseError for syntactical errors */
  private def crawlTheory(start: Int) : Int =
  {
    val oldComment = keepComment
    var i = skipws(crawlKeyword(start, "%sig"))

    // read the name
    val (sigName, positionAfter) = crawlIdentifier(i)
    i = positionAfter   // jump over identifier
    i = expectNext(i, "=")
    i += 1    // jump over "="
    i = expectNext(i, "{")

    // add the (empty, for now) theory to the controller
    val declaredTheory = new DeclaredTheory(getCurrentDPath, LocalPath(List(sigName)), None)
    add(declaredTheory)

    // read the theory body
    i = crawlTheoryBody(i, declaredTheory)

    val endsAt = expectNext(i, ".")

    // add the semantic comment and source reference
    addSemanticComment(declaredTheory, oldComment)
    addSourceRef(declaredTheory, start, endsAt)

    // add the link to this theory to the narrative document
    add(MRef(dpath, declaredTheory.path))
    return endsAt + 1
  }


  /** Reads a link (view or complex structure) body
    * @param start the position of the opening {
    * @param parent the parent DeclaredLink
    * @return the position after the closing }
    * @throws TextParseError for syntactical errors */
  private def crawlLinkBody(start: Int, parent: DeclaredLink) : Int =
  {
    var i = start + 1       // jump over '{'
    keepComment = None          // reset the last semantic comment stored
    i = skipwscomments(i)       // check whether there is a new semantic comment
    while (i < flat.length) {
      if (flat.startsWith("%include", i))
        i = crawlIncludeAssignment(i, parent)
      else if (flat.startsWith("%struct", i))
        i = crawlStructureAssignment(i, parent)
      else if (isIdentifierPartCharacter(flat.codePointAt(i)))
        i = crawlConstantAssignment(i, parent)
      else if (flat.startsWith("%", i) && (i < flat.length && isIdentifierPartCharacter(flat.codePointAt(i + 1)))) // unknown %-declaration => ignore it
        i = skipAfterDot(i)
      else if (flat.codePointAt(i) == '}')
        return i + 1
      else
        throw TextParseError(toPos(i), "unknown declaration in link body")

      keepComment = None          // reset the last semantic comment stored
      i = skipwscomments(i)       // check whether there is a new semantic comment
    }
    return i
  }


  /** Reads a view.
    * @param start the position of the initial '%'
    * @return position after the block
    * @throws TextParseError for syntactical errors */
  private def crawlView(start: Int) : Int =
  {
    val oldComment = keepComment

    var i = skipws(crawlKeyword(start, "%view"))

    // read the optional %implicit keyword
    var isImplicit : Boolean = false
    if (flat.startsWith("%implicit", i)) {
      isImplicit = true
      i = skipws(crawlKeyword(i, "%implicit"))   // TODO implicit views
    }
    i = skipwscomments(i)

    // read the name
    val (name, positionAfter) = crawlIdentifier(i)
    i = positionAfter   // jump over name

    i = expectNext(i, ":")
    i += 1    // jump over ":"
    i = skipwscomments(i)

    // read the domain
    val (domain, positionAfterDomain) = crawlTheoryObject(i, Path.parse(""))
    i = positionAfterDomain   // jump over domain

    i = expectNext(i, "->")
    i += "->".length
    i = skipwscomments(i)

    // read the codomain
    val (codomain, positionAfterCodomain) = crawlTheoryObject(i, Path.parse(""))
    i = positionAfterCodomain     // jump over codomain

    i = expectNext(i, "=")
    i += 1     // jump over "="
    i = skipwscomments(i)

    // check if it's defined via a morphism or a list of assignments
    var view : View = null
    if (flat.codePointAt(i) == '{') {
      // It's a DeclaredView
      view = new DeclaredView(getCurrentDPath, LocalPath(List(name)), domain, codomain)
      add(view)
      i = crawlLinkBody(i, view.asInstanceOf[DeclaredView])
    }
    else if (isIdentifierPartCharacter(flat.codePointAt(i))) {
      // It's a DefinedView
      val (morphism, positionAfter) = crawlMorphismObject(i, None)
      i = positionAfter
      view = new DefinedView(getCurrentDPath, LocalPath(List(name)), domain, codomain, morphism)
      add(view)
    }
    else
      throw TextParseError(toPos(i), "morphism or assignment list expected after '='")

    val endsAt = expectNext(i, ".")

    // add the semantic comment and source reference
    addSemanticComment(view, oldComment)
    addSourceRef(view, start, endsAt)

    // add a link to this view to the narrative document
    add(MRef(dpath, view.path))
    return endsAt + 1
  }


  // ------------------------------- auxiliary methods -------------------------------


  private def log(s : String) = report("textReader", s)

  /** tells the controller given as class parameter to add the StructuralElement */
  private def add(e : StructuralElement) {
     controller.add(e)
  }


  private def min(a: Int, b: Int) = if (a <= b) a else b


  /** Convert a module name to its URI. Only namespace prefixes are checked.
    * @param start position of the first character of the module name (for error reporting)
    * @param moduleName the module name as a string
    * @return the absolute URI of the module
    * @throws TextParseError if the module name has a prefix and the prefix is not a valid namespace alias, or if the module name has no prefix and the current namespace is not defined */
  private def moduleToAbsoluteURI(start: Int, moduleName: String) : URI = {
    // the URI of the module is *not* used to compute the absolute URI of its dependencies
    // Replace dots with question marks
    val relativeURI : String = moduleName.trim().replaceAll("\\056", "?")
    val j = relativeURI.indexOf("?")
    // If it has no prefix, it belongs to the current namespace, so simply prepend the current namespace URI
    if (j == -1) {
      if (currentNS == None)
        throw TextParseError(toPos(start), "cannot compute an absolute URI for this module " + moduleName + " since no current namespace is defined")
      return currentNS.get ? relativeURI
    }
    // If it has a prefix, it belongs to a different namespace, which must have been declared before in the document
    val prefix : String = relativeURI.substring(0, j)
    if (!prefixes.contains(prefix))                      // check if the alias is known
      throw TextParseError(toPos(start), prefix + " is not a valid namespace alias")
    val realURI = prefixes.get(prefix).get
    return URI(realURI.toString() + relativeURI.substring(j))
  }


  /** computes the (line, column) that represents the same position as the given one-dimensional coordinate
  * @param index the one-dimensional coordinate to be transformed into two-dimensional
  * @return Pair(line, column) */
  private def toPos(index: Int) : SourcePosition = {
    val pair  = lineStarts.filter(p => (p._1 <= index)).last
    SourcePosition(index, pair._2, index - pair._1)  // the column may be the bogus space character at the end of the line
  }

  /** returns the fragment between startsAt and endsAt (inclusive), as a String */
  private def getSlice(startsAt : Int, endsAt : Int) : String = flat.substring(startsAt, endsAt+1)

  /** returns dpath#startline.startcol-endline.endcol */
  private def getSourceRef(startsAt : Int, endsAt : Int) = SourceRef(dpath.uri, SourceRegion(toPos(startsAt), toPos(endsAt)))

  /** adds sourceRef to the metadata of the first argument */
  private def addSourceRef(target : HasMetaData, startsAt : Int, endsAt : Int) {
    target.metadata.add(metadata.Link(SourceRef.metaDataKey, getSourceRef(startsAt, endsAt).toURI))
  }

  /** adds metadata from the optional semantic comment to the target */
  private def addSemanticComment(target : HasMetaData, comment : Option[MetaData] = keepComment) {
    comment match {
      case Some(metadata) => target.metadata.add(metadata.getAll: _*)
      case None =>
    }
  }

  /** returns the top-level dpath of the current module */
  private def getCurrentDPath : DPath = currentNS.map(uri => DPath(uri)).getOrElse(dpath)
}


object TextReader {
   /** the theory in which the parsed metadata keys reside */
   def metadataBase : MPath = MetaDatum.keyBase

   /** the "sourceRef" metadata key */
   def sourceRefKey : GlobalName = Path.parseS(xml.namespace("omdoc") + "?metadata?sourceRef", Path.empty)
}