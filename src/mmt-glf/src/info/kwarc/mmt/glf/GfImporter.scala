package info.kwarc.mmt.glf

import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.documents.{Document, FileLevel, MRef}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects.{OMID, OMS}
import info.kwarc.mmt.api.symbols.{Constant, PlainInclude}
import info.kwarc.mmt.api.utils.{File, URI}
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.ontology.{RelationalElement, ULOStatement}
import info.kwarc.mmt.lf.{FunType, LF, Typed}

import scala.collection.mutable

// NOTE: Some of this is based on IMPSImporter code without deep understanding

class GfImporter extends Importer {
  val key = "gf-omdoc"
  val inExts = List("gf")

  val log_progress = Some("progress")

  def importDocument(bt: BuildTask, index: Document => Unit,rel:ULOStatement => Unit): BuildResult = {
    /*
      PREPARATION
    */

    val docpath = DPath(URI(bt.narrationDPath.toPath))
    val s = File.read(bt.inFile)
    if (!s.contains("abstract")) {
      log("Skipping " + bt.inPath, log_progress)
      return BuildEmpty("Doesn't appear to be an abstract syntax")
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

    val constMap : mutable.Map[String, Constant] = mutable.Map()      // maps constant names to their constants

    /*
      INCLUDES (we need to fill up the constMap)
    */

    val directlyincluded : mutable.Set[LogicalDependency] = mutable.Set() // included theories
    val inclSet : mutable.Set[LogicalDependency] = mutable.Set()          // transitively included theories
    def doIncludes(mpath : MPath) : Boolean = {
      val asdep = LogicalDependency(mpath)
      if (inclSet.contains(asdep)) {
        return true
      } else {
        inclSet.add(asdep)
      }

      controller.get(mpath) match {
        case t : Theory =>
          for (incl <- t.getIncludesWithoutMeta) doIncludes(incl)
          for (const <- t.getConstants) constMap.put(const.name.toString, const)
        case _ => throw new Exception(mpath.toString + "doesn't appear to be a theory")
      }
      true
    }

    for (incl <- gf.includes) {
      val from = (docpath.toMPath.parent / (incl+".gf") / incl).toMPath
      val pi = PlainInclude(from, langTheory.toTerm.toMPath)
      if (!doIncludes(from)) {
        return MissingDependency(LogicalDependency(from) :: Nil, Nil, directlyincluded.toList)
      } else {
        directlyincluded.add(LogicalDependency(from))
      }
      controller.add(pi)
    }

    /*
      BODY
    */
    for (typename <- gf.types) {
      val c = symbols.Constant(langTheory.toTerm, LocalName(typename), Nil, Some(OMS(Typed.ktype)), None, None)
      constMap.put(c.name.toString, c)
      langTheory.add(c)
    }

    var foundint = false

    def symbname2OMS(s : String) : OMID = {
      if (s == "Int") {
        if (!foundint) {
          // MPath(DPath(URI("http://mathhub.info/COMMA/GLF")), LocalName("GLF_Int"))
          // MPath(DPath(URI("http://mathhub.info/MitM/Foundation")), LocalName("IntLiterals"))
          controller.add(PlainInclude(MPath(DPath(URI("http://cds.omdoc.org/urtheories")), LocalName("NatLiteralsOnly")),
            langTheory.toTerm.toMPath))
        }
        foundint = true
        // OMS(GlobalName(MPath(DPath(URI("http://mathhub.info/COMMA/GLF")), LocalName("GLF_Int")), LocalName("Int")))
        // OMS(GlobalName(MPath(DPath(URI("http://mathhub.info/MitM/Foundation")), LocalName("IntLiterals")), LocalName("int_lit")))
        OMS(GlobalName(MPath(DPath(URI("http://cds.omdoc.org/urtheories")), LocalName("NatSymbols")), LocalName("NAT")))
      } else constMap.get(s) match {
        case Some(c : Constant) => OMS(GlobalName(c.home.toMPath, c.name))
        case None => throw new Exception("Failed to find symbol " + s)
      }

    }

    for ((funname, funtypes) <- gf.functions) {
      val c = symbols.Constant(langTheory.toTerm, LocalName(funname), Nil,
        Some(FunType(funtypes.dropRight(1).map(s => (None, symbname2OMS(s))).toList, symbname2OMS(funtypes.last))),
        None, None)
      langTheory.add(c)
    }

    /*
      FINISHING UP
    */
    index(toplevelDoc)
    BuildSuccess(directlyincluded.toList, LogicalDependency(langTheory.toTerm.toMPath)::Nil)
  }
}
