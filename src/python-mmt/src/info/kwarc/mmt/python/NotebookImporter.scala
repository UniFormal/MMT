package info.kwarc.mmt.python

import info.kwarc.mmt.api._
import archives._
import documents._
import utils._
import parser._
import MyList._
import info.kwarc.mmt.api.ontology.{RelationalElement, ULOStatement}

/** imports notebooks written with the MMT kernel by extracting the "omdoc" field inserted by the [[Finalize]] command in the [[REPLServer]] */
class NotebookImporter extends Importer {
  val key = "ipynb-omdoc"
  
  def inExts = List("ipynb")

  /** extracts the OMDoc field from a notebook */
  private def extractOMDoc(j: JSON): Option[String] = {
    val cells = j(Left("cells")).getOrElse(return None)
    cells match {
      case a: JSONArray =>
        val docS = a.values.toList.reverse.mapFind {cell =>
          cell(Left("outputs"), Right(0), Left("data"), Left("application/omdoc"))
        }
        docS flatMap {
          case js: JSONString => Some(js.value)
          case _ => None
        }
      case _ => None
    }
  }
  
  def importDocument(bt: BuildTask, index: Document => Unit,rel:ULOStatement => Unit) = {
     val s = File.read(bt.inFile)
     val json = JSON.parse(s)
     val docS = extractOMDoc(json).getOrElse {
       throw LocalError("no omdoc found in notebook " + bt.inPath)
     }
     val sourceURL = bt.base / bt.inPath
     val dpath = bt.narrationDPath
     val ps = new ParsingStream(sourceURL, IsRootDoc(dpath), NamespaceMap(dpath), "omdoc", ParsingStream.stringToReader(docS))
     val doc = controller.read(ps, interpret = false)(ErrorThrower)
     SourceRef.update(doc, SourceRef(sourceURL, SourceRegion(SourcePosition(0, 0, 0), SourcePosition(s.length - 1, -1, -1))))
     index(doc)
     val mods = doc.getModules(controller.globalLookup)
     BuildSuccess(Nil, mods map LogicalDependency)
  }
}