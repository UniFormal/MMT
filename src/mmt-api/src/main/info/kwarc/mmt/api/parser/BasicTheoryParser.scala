package info.kwarc.mmt.api.parser

import info.kwarc.mmt.api._
import documents._
import modules._
import utils._
import Theory._

/**
 * parses input streams as theory bodies, i.e., without MMT style module system
 * 
 * the stream is parsed into a single [[DeclaredTheory]]
 * the URI of the stream is split such that the last part (e.g., the file name) becomes the theory name,
 * the rest (e.g., the path to the folder) becomes the namespace
 * 
 * @param meta the meta-theory to be used when constructing an MMT theory
 */
abstract class BasicTheoryParser(objectParser: ObjectParser, meta: Option[MPath]) extends Parser(objectParser) {
   def apply(ps: ParsingStream)(implicit cont: StructureParserContinuations) : Document = {
      val unp = new Unparsed(ps.fullString, s => throw LocalError(s))
      val uri = ps.nsMap.default
      val ns = DPath(uri.copy(path = uri.path.init))
      val name = LocalName(FilePath(uri.path).stripExtension.name)
      val mpath = parseHeader(ns?name)(unp)
      val thy = Theory.empty(mpath.doc, mpath.name, meta)
      val doc = new Document(ns, root = true, inititems = List(MRef(ns, thy.path)))
      controller.add(doc)
      controller.add(thy)
      parseBody(thy.path)(thy, unp)
      doc
   }
   
   
   /* a regular expression marking the end of a declaration
    * default: . followed by whitespace
    */
   def endOfDecl: String = ".\\s"
   /** read the next declaration, may be overridden; default: everything until (excluding but dropping) endOfDecl */
   def readDeclaration(implicit u: Unparsed): String = u.takeUntilRegex(endOfDecl)
   def parseDeclaration(s: String)(implicit thy: DeclaredTheory): Unit
   
   def comments: List[BracketPair]
   
   def parseHeader(default: MPath)(implicit u: Unparsed): MPath
   
   def parseBody(t: MPath)(implicit thy: DeclaredTheory, u: Unparsed) {
      while ({u.trim; !u.empty}) {
        val r = u.remainder
        comments.find(bp => r.startsWith(bp.open)) match {
          case Some(bp) =>
            u.drop(bp.open)
            u.takeUntilString(bp.close, comments)
          case None =>
             val s = readDeclaration
             parseDeclaration(s)
        }
      }
   }
}
