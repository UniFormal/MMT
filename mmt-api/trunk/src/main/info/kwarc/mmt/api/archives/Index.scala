package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import documents._
import frontend._
import Level.Level
import modules._
import parser._
import utils._

/** a build target for importing an archive in some source syntax
  *
  * This should only be needed when OMDoc is received from a third party.
  * OMDoc produced by [[Compiler]]s is indexed automatically.
  *
  */
abstract class Importer extends TraversingBuildTarget {imp =>
  /** source by default, may be overridden */
  def inDim = source

  /** the file extensions to which this may be applicable */
  def inExts: List[String]

  /** narration, produces also content and relational */
  val outDim = narration
  /** omdoc */
  override val outExt = "omdoc"

  def includeFile(s: String) = inExts.exists(e => s.endsWith("." + e))

  /** the main abstract method to be implemented by importers
    *
    * @param bt information about the input document and error reporting
    * @param index a continuation function to be called on every generated document
    */
  def importDocument(bt: BuildTask, index: Document => Unit): BuildResult

  def buildFile(bf: BuildTask): BuildResult = {
    importDocument(bf, doc => indexDocument(bf.archive, doc, bf.inPath))
  }

  override def buildDir(bd: BuildTask, builtChildren: List[BuildTask], level: Level): BuildResult = {
    bd.outFile.up.mkdirs
    val doc = controller.get(DPath(bd.archive.narrationBase / bd.inPath.segments)).asInstanceOf[Document]
    val inPathFile = Archive.narrationSegmentsAsFile(bd.inPath, "omdoc")
    writeToRel(doc, bd.archive / relational / inPathFile)
    BuildResult.empty
  }

  /** Write a module to content folder */
  private def writeToContent(a: Archive, mod: Module) {
    val contFile = a.MMTPathToContentPath(mod.path)
    log("[  -> content   ]     " + contFile.getPath)
    val w = new presentation.FileWriter(contFile)
    w("""<omdoc xmlns="http://omdoc.org/ns" xmlns:om="http://www.openmath.org/OpenMath">""")
    mod.toNode(w)
    w("</omdoc>")
    w.done
  }

  /** extract and write the relational information about a knowledge item */
  private def writeToRel(se: StructuralElement, file: File) {
    val relFile = file.setExtension("rel")
    log("[  -> relational]     " + relFile.getPath)
    val relFileHandle = File.Writer(relFile)
    controller.relman.extract(se) {
      r => relFileHandle.write(r.toPath + "\n")
    }
    relFileHandle.close
  }

  /** index a document */
  private def indexDocument(a: Archive, doc: Document, inPath: FilePath) {
    // write narration file
    val narrFile = getOutFile(a, inPath)
    log("[  -> narration ]     " + narrFile)
    val node = doc.toNode
    xml.writeFile(node, narrFile)
    // write relational file
    writeToRel(doc, a / relational / inPath)
    doc.getModulesResolved(controller.globalLookup) foreach { mod => indexModule(a, mod) }
  }

  /** index a module */
  private def indexModule(a: Archive, mod: Module) {
    // write content file
    writeToContent(a, mod)
    // write relational file
    writeToRel(mod, a / relational / Archive.MMTPathToContentPath(mod.path))
  }

  /** additionally deletes content and relational */
  override def cleanFile(a: Archive, curr: Current) {
    val controller = new Controller(report)
    val Current(inFile, narrPath) = curr
    val narrFile = getOutFile(a, narrPath)
    if (!narrFile.exists) {
      super.cleanFile(a, curr)
      return
    }
    try {
      val doc = controller.read(ParsingStream.fromFile(narrFile, Some(DPath(a.narrationBase / narrPath.segments)), Some(a.namespaceMap)), interpret = false)(new ErrorLogger(report))
      // TODO remove document from controller? mark document as dirty in controller?
      //TODO if the same module occurs in multiple narrations, we have to use getLocalItems and write/parse the documents in narration accordingly
      doc.collectModules(controller) foreach {mp =>
        val cPath = Archive.MMTPathToContentPath(mp)
        delete(a / content / cPath)
        delete((a / relational / cPath).setExtension("rel"))
      }
    } catch {
      case e: Exception =>
        report(LocalError("error, could not clean content of " + narrFile).setCausedBy(e))
    }
    delete((a / relational / narrPath).setExtension("rel"))
    super.cleanFile(a, curr)
  }

  override def cleanDir(a: Archive, curr: Current) {
    val inPathFile = Archive.narrationSegmentsAsFile(curr.path, "omdoc")
    delete((a / relational / inPathFile).setExtension("rel"))
  }


  /**
    * an Interpreter that calls this importer to interpret a parsing stream
    *
    * This Interpreter is only applicable if it can determine an archive that the parsing stream is created from.
    * In that case, it will import the file, i.e., change the state of the archive.
    */
  object asInterpreter extends checking.Interpreter {
    init(imp.controller)

    def format = imp.inExts.headOption.getOrElse(imp.key)

    def apply(ps: ParsingStream)(implicit errorCont: ErrorHandler) = {
      val (arch, path) = controller.backend.resolveLogical(ps.source).getOrElse {
        throw LocalError("cannot find source file for URI: " + ps.source)
      }
      val dpath = ps.parentInfo match {
         case IsRootDoc(dp) => dp
         case _ => throw LocalError("can only interpret root documents")
      }
      imp.build(arch, Build.update, FilePath(path), Some(errorCont))
      try {
        controller.globalLookup.getAs(classOf[Document],dpath)
      } catch {
        case e: Error => throw LocalError("no document produced")
      }
    }
  }
}

/** a trivial importer that reads OMDoc documents and returns them */
class OMDocImporter extends Importer {
  val key = "index"

  override def inDim = RedirectableDimension("omdoc", Some(source))

  def inExts = List("omdoc")

  def importDocument(bf: BuildTask, seCont: Document => Unit) = {
    val ps = ParsingStream.fromFile(bf.inFile, Some(bf.narrationDPath), Some(bf.archive.namespaceMap))
    val doc = controller.read(ps, interpret = false)(bf.errorCont)
    seCont(doc)
    BuildResult.empty
  }
}
