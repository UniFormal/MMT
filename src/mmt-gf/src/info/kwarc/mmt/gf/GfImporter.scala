package info.kwarc.mmt.gf

import info.kwarc.mmt.api.{DPath, LocalName, modules, symbols}
import info.kwarc.mmt.api.archives.{BuildResult, BuildSuccess, BuildTask, Importer}
import info.kwarc.mmt.api.documents.{DRef, Document, FileLevel, MRef, SectionLevel}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects.{OMS, OMV, Term, VarDecl}
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.utils.{File, URI}
import info.kwarc.mmt.lf.{FunType, LF, Typed}


// NOTE: A lot if this is based on IMPSImporter code without deeper understanding


class GfImporter extends Importer {
  val key = "gf-omdoc"
  val inExts = List("gf")


  val rootdpath = DPath(URI.https colon "glf.kwarc.info")    // at least for now...
  val docpath = rootdpath / "gfImport"
  val log_progress = Some("progress")

  def importDocument(bt: BuildTask, index: Document => Unit): BuildResult = {
    val s = File.read(bt.inFile)
    if (!s.contains("abstract")) {
      log("Skipping " + bt.inPath, log_progress)
      return BuildSuccess(Nil, Nil)  // TODO: Is this correct BuildResult?
    }
    log("Importing " + bt.inPath, log_progress)
    val name = bt.inFile.name
    val gf = (new GfParser).parse(s)


    val toplevelDoc = new Document(docpath, FileLevel)
    controller.add(toplevelDoc)
    // val doc = new Document(DPath((docpath / bt.inFile.name).uri.setExtension("omdoc")), SectionLevel)
    // controller.add(doc)
    // controller.add(DRef(toplevelDoc.path, doc.path))

    val langTheory = new Theory(bt.narrationDPath,
      LocalName(name), Some(LF.theoryPath),
      modules.Theory.noParams, modules.Theory.noBase)

    // TODO: Includes

    for (typename <- gf.types) {
      val c = symbols.Constant(langTheory.toTerm, LocalName(typename), Nil, Some(OMS(Typed.ktype)), None, None)
      langTheory.add(c)
    }

    for ((funname, funtypes) <- gf.functions) {
      val c = symbols.Constant(langTheory.toTerm, LocalName(funname), Nil,
        Some(FunType(funtypes.dropRight(1).map(s => (None, OMV(LocalName(s)))).toList, OMV(LocalName(funtypes.last)))),
        None, None)
      langTheory.add(c)
    }

    controller.add(langTheory)
    controller.add(MRef(toplevelDoc.path, langTheory.path))


    BuildSuccess(Nil, Nil)   // TODO: put right arguments
  }
}
