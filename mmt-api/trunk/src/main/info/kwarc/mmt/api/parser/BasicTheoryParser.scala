package info.kwarc.mmt.api.parser

import info.kwarc.mmt.api._
import documents._
import modules._

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
   def apply(ps: ParsingStream)(implicit errorCont: ErrorHandler) : Document = {
      val uri = ps.nsMap.default
      val ns = DPath(uri.copy(path = uri.path.init))
      val name = LocalName(uri.path.last)
      val thy = new DeclaredTheory(ns, name, meta)
      val doc = new Document(ns, true, List(MRef(ns, thy.path)))
      controller.add(doc)
      controller.add(thy)
      
      parseDeclarations(thy.path, ps)
      doc
   }
   
   def parseDeclarations(t: MPath, ps: ParsingStream)
}