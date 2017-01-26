package info.kwarc.mmt.imps

import scala.io.Source

import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.documents._


class IMPSImporter extends Importer
{
	val key : String = "imps-omdoc"
	
	def inExts = List("omdoc")
	
	def importDocument(bf: BuildTask, index: Document => Unit): BuildResult =
	{
		log("Reading " + bf.inFile)
		val e = try
		{
			val fileContents = Source.fromFile(bf.inFile).getLines.mkString
			val lp : LispParser = new LispParser()
			lp.parse(fileContents)
		} catch {
			case e : ExtractError =>
				log(e.getMessage)
				sys.exit
		}

		val conv = new IMPSImportTask(controller, bf, index)
		
		e match
		{
			case (d : LispExp) => conv.doDocument(d) ; return BuildSuccess(Nil, Nil)
			case _             => println("DBG: parsing did not return Exp") ; return BuildFailure(Nil, Nil)
		}
	}
}
