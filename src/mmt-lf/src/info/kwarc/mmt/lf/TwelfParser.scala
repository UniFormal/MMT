package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import metadata._
import documents._
import modules._
import notations._
import parser._
import patterns._
import objects._
import symbols._
import utils._
import scala.collection.mutable._

/** A TextReader parses Twelf */
// TODO second phase should update from fields in structures, as well as all aliases with the correct references. Currently aliases point to originatingLink?name
class TwelfParser extends Parser(new NotationBasedParser) {
  val format = "elf"

  def apply(ps: ParsingStream)(implicit cont: StructureParserContinuations): Document = {
    init(ps)
    val (doc, errors) = readDocument()
    errors reverseMap {e => cont.errorCont(e)}
    doc
  }

  // ------------------------------- private vars -------------------------------

  /** array containing all the lines in the file */
  private var lines : Array[String] = null

  /** all the lines in a single string, with " " instead of newline. */
  private var flat  : String = ""

  /** array of pairs <position in the flat array, the number of the line that starts at this position>. All indexes are 0-based. */
  private implicit var lineStarts = new Array[(Int, Int)](0)

  /** list of parsing errors in the file */
  private var errors : List[Error] = Nil

  /** temporary variable used during parsing: saves the last SemanticCommentBlock */
  private var keepComment : Option[MetaData] = None

  /** logical document URI */
  private var dpath : DPath = null
  /** physical document URI */
  private var source : URI = null

  // mapping from namespace prefixes to their URI
  private var prefixes:  NamespaceMap = null
  private def currentNS = prefixes.default


  private def init(ps: ParsingStream): Unit = {
    var line: String = null
    var lineList: List[String] = Nil
    while ({line = ps.stream.readLine; line != null}){
       lineList ::= line
    }
    lines = lineList.reverse.toArray
    dpath = ps.parentInfo match {
       case IsRootDoc(dp) => dp
       case _ => throw LocalError("parsing of non-root documents not supported")
    }
    source = ps.source
    flat = ""
    lineStarts = new Array [(Int, Int)] (0)
    errors = Nil
    keepComment = None
    prefixes = ps.nsMap(dpath)

    // read the file and update lines, flat and lineStarts
    var lineNumber = 0
    for (x <- lines) {
      lineStarts = lineStarts :+ (flat.length, lineNumber)
      flat += x + "\n"
      lineNumber += 1
    }

  }

  private var keyW : List[String] = Nil

  /** errors that occur during parsing */
   object TextParseError {
     def apply(pos: SourcePosition, s : String) = SourceError("structure-parser", SourceRef(source, pos.toRegion), s)
   }

  // ------------------------------- document level -------------------------------

  /*
  def readTheory(source : String, ns_ : URI)(puCont_ : ParsingUnit => Term) : (Theory, List[SourceError]) = {
    //init
    lines = source.split("\n").toArray
    puCont = puCont_
    init()
    crawlTheory(0)
  }
*/

  private def readDocument() : (Document, List[Error]) = {
    var i = 0  // position in the flattened file

    // add (empty, for now) narrative document to the controller
    val doc = new Document(dpath, FileLevel)
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
         if (flat.startsWith("%namespace", i)){
           i = crawlNamespaceBlock(i)
         }
         else if (flat.startsWith("%sig", i)){
           i = crawlTheory(i)
         }
         else if (flat.startsWith("%view", i))
           i = crawlView(i)
         else if (flat.startsWith("%spec", i)) {
           i = crawlSpec(i)
         }
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
       (doc, errors)
    } catch {
       case e: SourceError =>
          // for fatal errors, return partially parsed document
          errors ::= e.copy(level = Level.Fatal)
          (doc, errors)
    }
  }


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
   * @throw SourceError if the character at the start position is not an opening bracket,
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
          throw TextParseError(toPos(i), "unmatched right bracket " + c.toChar)
      else if (c == ':') // type of an expression
        i += 1
      else if (c == '.') // end of a local declaration (not in the language yet; just in case)
        i += 1
      else if (Character.isWhitespace(flat.charAt(i))) // skip over white space
        i = skipws(i)
      else
        throw TextParseError(toPos(i), "illegal character " + c.toChar + " at this point")
    }
    i
  }



  /** reads a term and calls the TermParser on it
   * @param start the position of the first character in the term
   * @param delimChars characters that terminate the term if they occur outside brackets (e.g., ] in [x:A]
   * @param delimiters identifiers that terminate the term if they occur outside brackets (e.g., = in c:A=t) but not inside an identifier
   * @return the parsed term, position after the term
   * @throws SourceError if there are unmatched right brackets }
   */
  private def crawlTerm(start: Int, delimChars: List[Char], delimiters: List[String], component: CPath, context: Context) : (Term, Int) =
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
      else if (delimChars contains c) // these end the term if they occur anywhere outside a bracketed block
        return computeReturnValue
      else if (c == ')' || c == ']' || c == '}')
        throw TextParseError(toPos(i), "unmatched right bracket " + c)
      else if (c == '.') // TODO remove this check and add . to delimChars
        return computeReturnValue
      else if (Character.isWhitespace(flat.charAt(i))) // skip over white space
        i += 1
      else if (isIdentifierPartCharacter(c)) { // identifier
        val (id, posAfter) = crawlIdentifier(i) // also finds identifiers that contain .
        if (delimiters contains id) // these end the term  if they occur anywhere outside a bracketed block and are not part of a larger identifier
          return computeReturnValue
        i = posAfter
      }
      else
        throw TextParseError(toPos(i), "illegal character " + c.toChar + " at this point")
    }

    // computes the return value. i is assumed to be the position after the end of the term
    def computeReturnValue = {
      val term = getSlice(start, i - 1)
      val iiC = new documents.InterpretationInstructionContext(prefixes)
      val pu = ParsingUnit(getSourceRef(start, i - 1), context, term, iiC)
      val pr = try {
         objectLevel(pu)(ErrorThrower)
      } catch {case e: Error =>
         errors ::= TextParseError(toPos(start), "object continuation caused error: " + e.getMessage)
         DefaultObjectParser(pu)(ErrorThrower)
      }

      (pr.toTerm, i)
    }

    throw TextParseError(toPos(i), "end of file reached while reading term")
  }

  private def crawlNotation(start: Int, delimiters: List[String], cpath : GlobalName): (TextNotation,Int) = {
    var i = start
    while (i < flat.length) {
      if (delimiters exists {d => flat.startsWith(d, i)}) {
         val not = TextNotation.parse(getSlice(start, i - 1), NamespaceMap(cpath))
         return (not, i)
      } else {
         i += 1
      }
    }
    throw TextParseError(toPos(i), "end of file reached while reading notation ")
  }

  /** finds the first dot which is not part of an identifier. Useful for jumping over a declaration.
    * Skips over comments and quote-surrounded strings, so dots inside them do not count.
    * @param start the position where to start looking
    * @return the position after the first dot with the desired properties
    * @throws SourceError if end of file was encountered before finding a dot */
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
    * @throws SourceError if the string does not close */
  private def crawlString(start: Int) : (String, Int) =
  {
    val endsAt = flat.indexOf('"', start + 1)    // position of the final quotes
    if (endsAt == -1)
      throw TextParseError(toPos(start), "the string does not close")
    return (flat.substring(start + 1, endsAt), endsAt + 1)
  }


  /** reads an identifier.
    * @param start the position of the first character of the identifier
    * @return (identifier as a string, position after the last character of the identifier)
    * @throws SourceError if the current position does not start an identifier */
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
      throw TextParseError(toPos(start), "identifier expected")
    return (myId, i)
  }


  /** jumps over a specified keyword
    * @param startsAt the position of the first character in the keyword
    * @return the position after the keyword
    * @throw SourceError if there is something else instead of the keyword, or the file finishes too early */
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
    * @throws SourceError if the string after white space is not whatToExpect. */
  private def expectNext(start: Int, whatToExpect: String, acceptComments: Boolean = true) : Int =
  {
    var i = 0
    if (acceptComments) {
      i = skipwscomments(start)
    }
    else {
      i = skipws(start)
    }
    if (i == flat.length || !flat.startsWith(whatToExpect, i)) {
      throw TextParseError(toPos(start), whatToExpect + " expected")
    }
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
    * @throws SourceError for syntactical errors
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
      val uri = try {
        URI(string.trim)
      } catch {
        case exc: java.net.URISyntaxException => throw TextParseError(toPos(i), "" + exc.getMessage)
      }
      endsAt = expectNext(positionAfterString, ".", false)
      prefixes = prefixes(DPath(uri))
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
      val absoluteRemoteURI = prefixes.default.resolve(uri).normalize
      prefixes = prefixes.add(alias, absoluteRemoteURI)
    }
    return 1 + endsAt
  }


  // ------------------------------- comments -------------------------------


  /** jumps over a non-semantic %{ comment }%
    * @param start the position of the initial %
    * @return the position after the final %
    * @throws SourceError if the comment does not close */
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
    * @throws SourceError if the comment does not close. Syntactical errors are only printed
    */
  private def crawlSemanticCommentBlock(start: Int) : (MetaData, Int) =
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
      properties += (("short", commentLines(0).trim))

    // Add the long comment
    if (firstPropertyLine >= 2)
      if (commentLines.slice(1, firstPropertyLine).mkString("\n").trim.nonEmpty) {
        // Remove initial and final empty lines
        val longCommentLines = commentLines.slice(1, firstPropertyLine)
        val firstNonEmptyLine = longCommentLines.indexWhere(_.trim.nonEmpty)
        val lastNonEmptyLine = longCommentLines.lastIndexWhere(_.trim.nonEmpty)
        properties += (("long", longCommentLines.slice(firstNonEmptyLine, lastNonEmptyLine + 1).mkString("\n")))
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
            properties += ((keyValue.take(i).trim, keyValue.drop(i).trim))
        }
    } catch {
        case e : SourceError => errors ::= e   // add to the list of errors returned
    }

    return (MetaData(properties.map(keyValue => new MetaDatum(Path.parseS("??" + keyValue._1, NamespaceMap(MetaDatum.keyBase)), uom.OMLiteral.OMSTR(keyValue._2))).toSeq : _*), endsAt + 1)
  }

  // ------------------------------- symbol level -------------------------------


  /** reads a constant declaration.
    * @param start the position of the first character in the constant identifier
    * @param parent the parent theory
    * @return position after the block
    * @throws SourceError for syntactical errors */
  private def crawlConstantDeclaration(start: Int, parent: Theory) : Int =
  {
    val oldComment = keepComment
    val context = Context(parent.path)

    var i = start
    if (flat.startsWith("%abbrev", i)) {
      i += "%abbrev".length
      i = skipwscomments(i)
    }
    val (cstName, positionAfter) = crawlIdentifier(i)  // read constant name
    i = positionAfter
    i = skipwscomments(i)

    val cpath = parent.path ? cstName

    // read the optional type
    val constantType = new TermContainer
    if (flat.codePointAt(i) == ':') {
      i += 1  // jump over ':'
      i = skipwscomments(i)
      val (term, posAfter) = crawlTerm(i, Nil, List("=","#"), cpath $ TypeComponent, context)
      constantType.read = getSlice(i, posAfter - 1)
      constantType.parsed = term
      i = posAfter
      i = skipwscomments(i)
    }

    // read the optional definition
    val constantDef = new TermContainer
    if (flat.codePointAt(i) == '=') {
      i += 1  // jump over '='
      i = skipwscomments(i)
      val (term, posAfter) = crawlTerm(i, Nil, List("#"), cpath $ DefComponent, context)
      constantDef.read = getSlice(i, posAfter - 1)
      constantDef.parsed = term
      i = posAfter
      i = skipwscomments(i)
    }

    // read the optional notation
    var constantNotation = new NotationContainer
    if (flat.codePointAt(i) == '#') {
      i += 1  // jump over '#'
      i = skipwscomments(i)
      val (not, posAfter) = crawlNotation(i, List("."), cpath)
      addSourceRef(not, i, posAfter - 1)
      constantNotation.parsingDim.set(not)

      i = posAfter
      i = skipwscomments(i)
    }
    val endsAt = expectNext(i, ".")

    // create the constant object
    val constant = Constant(OMMOD(cpath.module), cpath.name, Nil, constantType, constantDef, None, constantNotation)
    addSourceRef(constant, start, endsAt)
    addSemanticComment(constant, oldComment)
    add(constant)
    return endsAt + 1
  }


  /** reads an %open block
    * @param start the position of the initial % from %open
    * @param link the structure declaration before the %open statement
    * @return position after the block, i.e. ideally on the '.'
    * @throws SourceError for syntactical errors
    * note: if link's domain is not yet known (link.from == null), the alias points to link.toMorph ? name */
  //TODO: the whole method should be removed and parsing of aliases changed
  //this affects backwards compatibility with Twelf though
  //for now, all found aliases are ignored
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
      val refGlobalName = GlobalName((if (link.from != null) link.from else link.toTerm).toMPath, LocalName(ref))

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

      /*
      // construct the alias
      val alias = new Alias(link.to, newLocalName, refGlobalName)

      // add metadata and source references
      addSemanticComment(alias, oldComment)
      addSourceRef(alias, startOfAlias, endOfAlias)

      // add to controller
      add(alias)
      */
    }
    return i
  }


  /** reads an %include declaration
    * @param start the position of the initial % from %include
    * @param parent the parent theory
    * @return position after the block
    * @throws SourceError for syntactical errors */
  private def crawlIncludeDeclaration(start: Int, parent: Theory) : Int =
  {
    val oldComment = keepComment
    var i = crawlKeyword(start, "%include")
    i = skipws(i)
    val (importName, positionAfter) = crawlIdentifier(i)    // read import name
    val fromOpt = moduleToAbsoluteURI(i, importName)
    i = positionAfter

    // read the optional %open statement
    i = skipwscomments(i)
    fromOpt match {
      case Some(from) =>
          // create the Include object
          val include = Include(parent.toTerm, from, Nil)
          if (flat.startsWith("%open", i))
             i = crawlOpen(i, include)
          // add metadata and source references
          addSemanticComment(include, oldComment)
          val endsAt = expectNext(i, ".")
          addSourceRef(include, start, endsAt)
          add(include)
          i = endsAt
      case None => //recover from error by trying to skip include
          i = expectNext(i, ".")
    }
    return i + 1
  }


  /** reads a %meta declaration
    * @param start the position of the initial % from %meta
    * @param parent the parent theory
    * @return position after the block
    * @throws SourceError for syntactical errors */
  private def crawlMetaDeclaration(start: Int, parent: Theory) : Int =
  {
    var i = skipws(crawlKeyword(start, "%meta"))
    val (metaTheoryName, positionAfter) = crawlIdentifier(i)    // read meta theory name
    val mt = moduleToAbsoluteURI(i, metaTheoryName)
    parent.metaC.update(TermContainer(mt map (OMMOD(_))))
    i = positionAfter
    val endsAt = expectNext(i, ".")
    return endsAt + 1
  }


  /** reads a structure declaration.
    * @param start the position of the initial % from %struct
    * @param parent the parent theory
    * @return position after the block
    * @throws SourceError for syntactical errors */
  private def crawlStructureDeclaration(start: Int, parent: Theory) : Int =
  {
    var domain : Option[MPath] = None
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

    val spath = parent.path ? name

    // parse the optional domain
    if (flat.codePointAt(i) == ':') {
      i += 1
      i = skipws(i)
      val (dom, positionAfterDomain) = crawlIdentifier(i)   // jump over structure domain
      domain = moduleToAbsoluteURI(i, dom)
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
            val declstructure = SimpleDeclaredStructure(parent.toTerm, LocalName(name), dom, isImplicit)
            add(declstructure)
            structure = declstructure
            i = crawlLinkBody(i, declstructure)
        }
      }
      else if (!isIdentifierPartCharacter(flat.codePointAt(i)))
        throw TextParseError(toPos(i), "morphism or assignment list expected after '='")
      else {
        // It's a DefinedStructure
        val (morphism, positionAfter) = crawlTerm(i, Nil, Nil, spath $ DefComponent, Context(parent.path))
        i = positionAfter
        domain match {
          case Some(dom) => structure = Structure(parent.toTerm, LocalName(name), OMMOD(dom), Some(morphism), isImplicit, false)
          //TODO: the domain should be obligatory so that this case goes away; but currently the Twelf parser expects it to be omitted
          case None => structure = Structure(parent.toTerm, LocalName(name), null, Some(morphism), isImplicit, false)
        }
        add(structure)
      }
    }
    else domain match {
      case None => throw TextParseError(toPos(start), "structure has no definiens and its domain is not specified")
      case Some(dom) =>
        // It's a DeclaredStructure with empty body
        structure = SimpleDeclaredStructure(parent.toTerm, LocalName(name), Path.parseM(dom.toString, NamespaceMap(parent.path)), isImplicit)
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

  // parse a parameter list [param_1]...[param_n]
  private def crawlParameterList(start: Int, theory: MPath) : (Context, Int) = {
     var parameterContext = Context()
     var i = start
     if (flat.startsWith("[", i)) {
       do {
         // parse a single declaration
         i = expectNext(i, "[")
         i += "[".length
         i = skipwscomments(i)
         val (varDecl, posAfterParameter) = crawlParameter(i, theory, parameterContext)
         i = posAfterParameter
         parameterContext = parameterContext ++ varDecl
         i = skipwscomments(i)
       } while (! flat.startsWith("{", i))
     }
     (parameterContext, i)
  }

  // parses a single parameter x1 : T1 = D1]
  private def crawlParameter(start: Int, theory: MPath, context: Context) : (VarDecl, Int) =
  {
     var i = start

     // parse the variable name
     val (name, positionAfter) = crawlIdentifier(i)
     i = positionAfter
     i = skipwscomments(i)

     // skip over ':'
     i = expectNext(i, ":")
     i += ":".length
     i = skipwscomments(i)

     // parse the type
     val (term, posAfter) = crawlTerm(i, List(']'), Nil, theory ? name $ TypeComponent, context ++ (theory ^))
     val vd = OMV(name) % term
     i = posAfter

     // skip over ']'
     i = expectNext(i, "]")
     i += "]".length
     i = skipwscomments(i)
     (vd, i)
  }


  /** Reads a pattern body
      * @param start the position of the opening {
      * @param theory the URI of the pattern
      * @return pattern body as a context, position after the closing }
      * @throws SourceError for syntactical errors */
    private def crawlPatternBody(start: Int, theory: MPath, context: Context) : (Context, Int) =
    {
      var i = start + 1       // jump over '{'
      keepComment = None          // reset the last semantic comment stored
      i = skipwscomments(i)       // check whether there is a new semantic comment
      var body = Context()

      while (i < flat.length) {
        if (isIdentifierPartCharacter(flat.codePointAt(i))) {
          // read variable declaration
          val (varDecl, posAfterDeclaration) = crawlDeclarationInPatternBody(i, theory, context ++ body)
          i = posAfterDeclaration
          body = body ++ varDecl
        }
        else if (flat.startsWith("%", i) && (i < flat.length && isIdentifierPartCharacter(flat.codePointAt(i + 1)))) { // unknown %-declaration => ignore it
          i = skipAfterDot(i)
        }
        else if (flat.codePointAt(i) == '}')    // end of signature body
          return (body, i)
        else
          throw TextParseError(toPos(i), "unknown declaration in pattern body")
        keepComment = None          // reset the last semantic comment stored
        i = skipwscomments(i)       // check whether there is a new semantic comment
      }
      return (body, i)
    }


   /** reads a variable declaration inside a pattern body.
     * @param start the position of the first character in the variable identifier
     * @return variable declaration, position after the block
     * @throws SourceError for syntactical errors */
    private def crawlDeclarationInPatternBody(start: Int, theory: MPath, context: Context) : (VarDecl, Int) =
    {
      val oldComment = keepComment

      var i = start
      val (name, positionAfter) = crawlIdentifier(i)  // read variable name
      i = positionAfter
      i = skipwscomments(i)

      // read the optional type
      var varType : Option[Term] = None
      if (flat.codePointAt(i) == ':') {
        i += 1  // jump over ':'
        i = skipwscomments(i)
        val (term, posAfter) = crawlTerm(i, Nil, List("=","#"), theory ? name $ TypeComponent, context ++ (theory ^)) // TODO remove ^ to avoid ignoring notations inside pattern body (occurs multiple times)
        varType = Some(term)
        i = posAfter
        i = skipwscomments(i)
      }

      // read the optional definition
      var varDef : Option[Term] = None
      if (flat.codePointAt(i) == '=') {
        i += 1  // jump over '='
        i = skipwscomments(i)
        val (term, posAfter) = crawlTerm(i, Nil, List("#"), theory ? name $ DefComponent, context ++ (theory ^))
        varDef = Some(term)
        i = posAfter
        i = skipwscomments(i)
      }

      val endsAt = expectNext(i, ".")

      // create the VarDecl object
      val varDecl = VarDecl(LocalName(name), None, varType, varDef, None)
      addSourceRef(varDecl, start, endsAt)
      addSemanticComment(varDecl, oldComment)

      return (varDecl, endsAt + 1)
    }

   private def resolveAssignmentName(parent: Link, name: LocalName) = {
      controller.globalLookup.resolveRealizedName(parent, name) match {
          case List(p) => Some(p.toLocalName)
          case _ => None
      }
   }

   private def assPath(l: Link, n: LocalName) = l match {
       case v: View => v.path ? n
       case s: Structure => s.path / n
    }

  /** Reads a constant assignment.
    * @param start the position of the first character in the constant identifier
    * @param parent the parent link
    * @return position after the block
    * @throws SourceError for syntactical errors */
  private def crawlConstantAssignment(start: Int, parent: Link) : Int =
  {
    val oldComment = keepComment
    var i = start

    // read constant name
    val (cstName, positionAfter) = crawlIdentifier(i)
    val constantName = LocalName.parse(cstName.replaceAll("\\.", "/"))
    i = positionAfter
    val cstNameRes = resolveAssignmentName(parent, constantName).getOrElse(constantName)
    val apath = assPath(parent, cstNameRes)

    i = expectNext(i, ":=")
    i += ":=".length
    i = skipwscomments(i)

    // read the assigned term
    val (term, posAfter) = crawlTerm(i, Nil, Nil, apath $ DefComponent, parent.codomainAsContext)
    i = posAfter
    i = skipwscomments(i)

    val endsAt = expectNext(i, ".")

    // add the constant assignment to the controller
    val constantAssignment = ConstantAssignment(parent.toTerm, apath.name, Nil, Some(term))
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
    * @throws SourceError for syntactical errors */
  private def crawlStructureAssignment(start: Int, parent: Link) : Int =
  {
    val oldComment = keepComment
    var i = skipws(crawlKeyword(start, "%struct"))

    // read structure name
    val (strName, positionAfter) = crawlIdentifier(i)
    val structureName = LocalName.parse(strName.replaceAll("\\.", "/"))
    i = positionAfter
    val strNameRes = resolveAssignmentName(parent, structureName).getOrElse(structureName)
    val apath = assPath(parent, strNameRes)

    i = expectNext(i, ":=")
    i += ":=".length
    i = skipwscomments(i)

    // get the morphism
    val (morphism, positionAfter2) = crawlTerm(i, Nil, Nil, apath $ DefComponent, parent.codomainAsContext)

    val endsAt = expectNext(positionAfter2, ".")    // on the final dot

    // add the structure assignment to the controller
    val domain = null //TODO should be domain of morphism
    val defLinkAssignment = DefLinkAssignment(parent.toTerm, LocalName(strName), domain, morphism)
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
    * @throws SourceError for syntactical errors */
  private def crawlIncludeAssignment(start: Int, parent: Link) : Int =
  {
    val oldComment = keepComment
    val i = skipws(crawlKeyword(start, "%include"))

    val domain = utils.mmt.mmtbase ? i.toString //dummy value - should be required in input, i is needed for uniqueness
    val apath = assPath(parent, LocalName(domain))

    // get the morphism
    val (morphism, positionAfter) = crawlTerm(i, Nil, Nil, apath $ DefComponent, parent.codomainAsContext)

    val endsAt = expectNext(positionAfter, ".")    // on the final dot

    // add the include assignment to the controller
    val defLinkAssignment = LinkInclude(parent.toTerm, domain, morphism)
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
    * @throws SourceError for syntactical errors */
  private def crawlTheoryBody(start: Int, parent: Theory) : Int =
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
    * @throws SourceError for syntactical errors */
  private def crawlTheory(start: Int) : Int =
  {
    val oldComment = keepComment
    var i = skipws(crawlKeyword(start, "%sig"))

    // read the name
    val (sigName, positionAfter) = crawlIdentifier(i)
    i = skipwscomments(positionAfter)   // jump over identifier
    val tpath = getCurrentDPath ? sigName
    var meta : Option[MPath] = None
    if (flat.codePointAt(i) == ':') {
       i = expectNext(i, ":") + 1
       i = skipwscomments(i)
       val (mtId, positionAfterMeta) = crawlIdentifier(i)
       meta = moduleToAbsoluteURI(i, mtId)
       i = positionAfterMeta
    }
    i = expectNext(i, "=")
    i += 1    // jump over "="
    i = skipws(i)
    // default to meta-theory LF
    if (meta.isEmpty)
       meta = Some(LF.theoryPath)

    var theory : Theory = null
    if (flat.codePointAt(i) == '{') {
       // It's a DeclaredTheory
       i = expectNext(i, "{")
       // add the (empty, for now) theory to the controller
       val declTheory = Theory.empty(tpath.parent, tpath.name, meta)
       theory = declTheory
       add(theory)
       // read the theory body
       i = crawlTheoryBody(i, declTheory)
    }
    else {
      // It's a DefinedTheory
      val (theoryExp, positionAfter) = crawlTerm(i, Nil, Nil, tpath $ DefComponent, Context())
      i = positionAfter
      theory = new Theory(tpath.parent, tpath.name, None, Theory.noParams, TermContainer(theoryExp))
      add(theory)
      if (meta.isDefined) {
         errors ::= TextParseError(toPos(i), "meta-theory of defined theory is ignored").copy(level = Level.Warning)
      }
    }

    val endsAt = expectNext(i, ".")

    // add the semantic comment and source reference
    addSemanticComment(theory, oldComment)
    addSourceRef(theory, start, endsAt)

    // add the link to this theory to the narrative document
    add(MRef(dpath, theory.path))
    return endsAt + 1
  }


  /** Reads a link (view or complex structure) body
    * @param start the position of the opening {
    * @param parent the parent DeclaredLink
    * @return the position after the closing }
    * @throws SourceError for syntactical errors */
  private def crawlLinkBody(start: Int, parent: Link) : Int =
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
    * @throws SourceError for syntactical errors */
  private def crawlView(start: Int) : Int =
  {
    val oldComment = keepComment

    var i = skipws(crawlKeyword(start, "%view"))

    // read the optional %implicit keyword
    var isImplicit : Boolean = false
    if (flat.startsWith("%implicit", i)) {
      isImplicit = true
      i = skipws(crawlKeyword(i, "%implicit"))
    }
    i = skipwscomments(i)

    // read the name
    val (name, positionAfter) = crawlIdentifier(i)
    i = positionAfter   // jump over name
    val vpath = getCurrentDPath ? name

    i = expectNext(i, ":")
    i += 1    // jump over ":"
    i = skipwscomments(i)

    // read the domain
    val (domain, positionAfterDomain) = crawlTerm(i, Nil, List("->"), vpath $ DomComponent, Context())
    i = positionAfterDomain   // jump over domain

    i = expectNext(i, "->")
    i += "->".length
    i = skipwscomments(i)

    // read the codomain
    val (codomain, positionAfterCodomain) = crawlTerm(i, Nil, List("="), vpath $ CodComponent, Context())
    i = positionAfterCodomain     // jump over codomain

    i = expectNext(i, "=")
    i += 1     // jump over "="
    i = skipwscomments(i)

    // check if it's defined via a morphism or a list of assignments
    var view : View = null
    if (flat.codePointAt(i) == '{') {
      // It's a DeclaredView
      view = View(vpath.parent, vpath.name, domain, codomain, isImplicit)
      add(view)
      i = crawlLinkBody(i, view.asInstanceOf[View])
    }
    else {
      // It's a DefinedView
      val (morphism, positionAfter) = crawlTerm(i, Nil, Nil, vpath $ DefComponent, Context())
      i = positionAfter
      view = View(vpath.parent, vpath.name, domain, codomain, TermContainer.asParsed(morphism), isImplicit)
      add(view)
    }

    val endsAt = expectNext(i, ".")

    // add the semantic comment and source reference
    addSemanticComment(view, oldComment)
    addSourceRef(view, start, endsAt)

    // add a link to this view to the narrative document
    add(MRef(dpath, view.path))
    return endsAt + 1
  }


  // ------------------------------- auxiliary methods -------------------------------


  /** tells the controller given as class parameter to add the StructuralElement */
  private def add(e : StructuralElement): Unit = {
    controller.add(e)
  }

  private def min(a: Int, b: Int) = if (a <= b) a else b

  /** Convert a module name to its URI. Only namespace prefixes are checked.
    * @param start position of the first character of the module name (for error reporting)
    * @param moduleName the module name as a string
    * @return the absolute URI of the module
    * @throws SourceError if the module name has a prefix and the prefix is not a valid namespace alias, or if the module name has no prefix and the current namespace is not defined */
  private def moduleToAbsoluteURI(start: Int, moduleName: String) : Option[MPath] = {
    // the URI of the module is *not* used to compute the absolute URI of its dependencies
    // Replace dots with question marks
    val relativeURI : String = moduleName.trim().replaceAll("\\056", "?")
    val j = relativeURI.indexOf("?")
    // If it has no prefix, it belongs to the current namespace, so simply prepend the current namespace URI
    if (j == -1) {
        Some(DPath(prefixes.default) ? relativeURI)
    } else {
       // If it has a prefix, it belongs to a different namespace, which must have been declared before in the document
       val prefix : String = relativeURI.substring(0, j)
       prefixes.get(prefix) match {
          // unknown prefix
          case None =>
             errors ::= TextParseError(toPos(start), "unknown namespace prefix").copy(level = Level.Warning)
             None
          case Some(ns) =>
             Some(DPath(ns) ? relativeURI.substring(j+1))
       }
    }
  }

  /** computes two- from one-dimensional coordinate
  * @param index the one-dimensional coordinate
  * @return the the two-dimensional coordinate
  */
  private def toPos(index: Int) : SourcePosition = {
    val pair = try {
      lineStarts.filter(p => (p._1 <= index)).last
    } catch {
      // this catches the degenerate case where .last fails; this happens iff the input file is empty
      case _ : java.util.NoSuchElementException => (0,0)
    }
    SourcePosition(index, pair._2, index - pair._1)  // the column may be the bogus space character at the end of the line
  }

  /** returns the fragment between startsAt and endsAt (inclusive), as a String */
  private def getSlice(startsAt : Int, endsAt : Int) : String = flat.substring(startsAt, endsAt+1)

  /** as getSlice but additionally  carrying the current file */
  private def getSourceRef(startsAt : Int, endsAt : Int) = SourceRef(dpath.uri, SourceRegion(toPos(startsAt), toPos(endsAt)))

  /** adds sourceRef to the metadata of the first argument */
  private def addSourceRef(target : HasMetaData, startsAt : Int, endsAt : Int): Unit = {
    SourceRef.update(target, getSourceRef(startsAt, endsAt))
  }

  /** adds metadata from the optional semantic comment to the target */
  private def addSemanticComment(target : HasMetaData, comment : Option[MetaData] = keepComment): Unit = {
    comment match {
      case Some(metadata) => target.metadata.add(metadata.getAll: _*)
      case None =>
    }
  }

  /** returns the top-level dpath of the current module */
  private def getCurrentDPath : DPath = DPath(prefixes.default)

  //TODO
  /** Reads a logic specification.
    * @param start the position of the initial '%'
    * @return position after the block
    * @throws SourceError for syntactical errors */
  // * should read spec name (do we store it?)
  // * read include reference to the  home theory (load it?)
  // * parse instance declarations, create Instance objects, add to home theory?
  private def crawlSpec(start: Int) : Int =
  {
    val oldComment = keepComment
    var i = skipws(crawlKeyword(start, "%spec"))

    // read the name
    val (specName, positionAfter) = crawlIdentifier(i)
    i = skipwscomments(positionAfter)   // jump over identifier
    val tpath = getCurrentDPath ? specName
    var meta : Option[MPath] = None

    // <-------- ':' ?
    if (flat.codePointAt(i) == ':') {
       i = expectNext(i, ":") + 1
       i = skipwscomments(i)
       val (mtId, positionAfterMeta) = crawlIdentifier(i)
       meta = moduleToAbsoluteURI(i, mtId)
       i = positionAfterMeta
    }
    i = expectNext(i, "=")
    i += 1    // jump over "="
    i = skipws(i)


    i = expectNext(i, "{")
    // <-----------  how to manage specs in controller?
    // add the (empty, for now) theory to the controller
    val theory = Theory.empty(tpath.parent, tpath.name, meta)
    add(theory)
    //        read the theory body
    i = crawlSpecBody(i, theory)

    val endsAt = expectNext(i, ".")

    // add the semantic comment and source reference
    addSemanticComment(theory, oldComment)
    addSourceRef(theory, start, endsAt)

    // add the link to this theory to the narrative document
    add(MRef(dpath, theory.path))
    return endsAt + 1
  }

   /** Reads a theory body
    * @param start the position of the opening {
    * @param parent the enclosing DeclaredTheory
    * @return the position after the closing }
    * @throws SourceError for syntactical errors */
  private def crawlSpecBody(start: Int, parent: Theory) : Int =
  {
    var i = start + 1       // jump over '{'
    keepComment = None          // reset the last semantic comment store/
    i = skipwscomments(i)       // check whether there is a new semantic comment


    // parent theorem
    var thm : Term = null

    while (i < flat.length) {
      if (flat.startsWith("%include", i)) {
        // read include declaration
//        i = crawlIncludeDeclaration(i, parent)
        i = crawlKeyword(i, "%include")
        i = skipws(i)
        val (importName, positionAfter) = crawlIdentifier(i)    // read import name
        thm = OMMOD(moduleToAbsoluteURI(i, importName).get)
        i = positionAfter
        i = expectNext(i, ".") + 1
      }
      else if (flat.startsWith("%", i) && (i < flat.length && isIdentifierPartCharacter(flat.codePointAt(i + 1)))) { // unknown %-declaration => ignore it
        i = skipAfterDot(i)
      }
      else if (flat.codePointAt(i) == '}')    // end of signature body
        return i + 1
//      else if crawlIdentifier(i)
      else
        throw TextParseError(toPos(i), "unknown declaration in signature body")
      keepComment = None          // reset the last semantic comment stored
      i = skipwscomments(i)       // check whether there is a new semantic comment
    }
    return i
  }
}
