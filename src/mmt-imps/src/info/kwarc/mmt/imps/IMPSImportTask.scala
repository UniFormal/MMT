package info.kwarc.mmt.imps

import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._

class IMPSImportTask(val controller: Controller, bt: BuildTask, index: Document => Unit) extends Logger with MMTTask
{
	def logPrefix = "imps-omdoc"
	protected def report: Report = controller.report
	
	def doDocument(d : LispExp) : BuildResult =
	{
		BuildResult.empty
	}
}
