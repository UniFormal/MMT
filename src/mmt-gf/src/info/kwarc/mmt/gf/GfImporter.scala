package info.kwarc.mmt.gf

import info.kwarc.mmt.api.{DPath, GlobalName, LocalName, modules, symbols}
import info.kwarc.mmt.api.archives.{BuildResult, BuildSuccess, BuildTask, Importer}
import info.kwarc.mmt.api.documents.{Document, FileLevel, MRef}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects.{OMS, OMV}
import info.kwarc.mmt.api.symbols.PlainInclude
import info.kwarc.mmt.api.utils.{File, URI}
import info.kwarc.mmt.lf.{FunType, LF, Typed}

// NOTE: Some of this is based on IMPSImporter code without deep understanding

class GfImporter extends Importer {
  val key = "gf-omdoc"
  val inExts = List("gf")

  val log_progress = Some("progress")

  def importDocument(bt: BuildTask, index: Document => Unit): BuildResult = {
    val docpath = DPath(URI(bt.narrationDPath.toPath))
    val s = File.read(bt.inFile)
    if (!s.contains("abstract")) {
      log("Skipping " + bt.inPath, log_progress)
      return BuildSuccess(Nil, Nil)  // TODO: Is this correct BuildResult?
    }
    log("Importing " + bt.inPath, log_progress)
    val name = bt.inFile.name.dropRight(3)
    val gf = (new GfParser).parse(s)


    val toplevelDoc = new Document(docpath, FileLevel)
    controller.add(toplevelDoc)

    val langTheory = new Theory(docpath,
      LocalName(name), Some(LF.theoryPath),
      modules.Theory.noParams, modules.Theory.noBase)

    controller.add(langTheory)
    controller.add(MRef(toplevelDoc.path, langTheory.path))

    for (incl <- gf.includes) {
      val pi = PlainInclude((docpath.toMPath.parent / (incl+".gf") / incl).toMPath, langTheory.toTerm.toMPath)
      controller.add(pi)
    }

    for (typename <- gf.types) {
      val c = symbols.Constant(langTheory.toTerm, LocalName(typename), Nil, Some(OMS(Typed.ktype)), None, None)
      langTheory.add(c)
    }

    for ((funname, funtypes) <- gf.functions) {
      val c = symbols.Constant(langTheory.toTerm, LocalName(funname), Nil,
        Some(FunType(funtypes.dropRight(1).map(s => (None, OMS(GlobalName(langTheory.path, LocalName(s.toString))))).toList,
                     OMS(GlobalName(langTheory.path, LocalName(funtypes.last))))),
        None, None)       // TODO: can langTheory.path be wrong module? - how to determine right one?
      langTheory.add(c)
    }


    index(toplevelDoc)

    BuildSuccess(Nil, Nil)   // TODO: put right arguments
  }
}
