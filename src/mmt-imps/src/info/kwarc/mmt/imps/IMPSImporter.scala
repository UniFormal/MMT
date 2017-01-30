package info.kwarc.mmt.imps

import scala.io.Source

import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.documents._

class IMPSImporter extends Importer
{
	val key : String = "imps-omdoc"
	
	def inExts = List("t")
	
	def importDocument(bf: BuildTask, index: Document => Unit): BuildResult =
	{
		log("Reading " + bf.inFile)
		val e = try
		{
			val fileLines = Source.fromFile(bf.inFile).getLines
			var contents : String = ""
			for (line <- fileLines)
			{
				contents = contents + line + "\n"
			}
			val lp : IMPSParser = new IMPSParser()
			lp.parse(contents, FileURI(bf.inFile))
		} catch {
			case e : ExtractError =>
				log(e.getMessage)
				sys.exit
		}

		val conv = new IMPSImportTask(controller, bf, index)
		
		e match
		{
			case (d : LispExp) => conv.doDocument(d)
			case _             => println("DBG: parsing did not return Exp") ; BuildResult.empty
		}
	}
}
