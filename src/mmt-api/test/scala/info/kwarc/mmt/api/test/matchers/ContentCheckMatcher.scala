package info.kwarc.mmt.api.test.matchers

import info.kwarc.mmt.api.Level.Level
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.parser.ParsingStream
import info.kwarc.mmt.api.{DPath, Error, ErrorHandler, Invalid, InvalidElement, InvalidObject, InvalidUnit, Level, Path, SourceError, archives, utils}
import info.kwarc.mmt.api.utils.{File, FilePath, URI}
import info.kwarc.mmt.doc.Setup

import scala.collection.mutable.ListBuffer

/** handling content check related archives */
trait ContentCheckMatcher extends MMTMatcher {
  /** a list of archives needed for this matcher to work properly */
  val archives: List[String]

  /** configures the controller for archives */
  def configureArchives: Unit = {
    // we do not use the [[BuildQueue]], even if it has been enabled
    // TODO: Once the build server is re-done we no longer need this
    controller.extman.get(classOf[BuildQueue]).foreach(controller.extman.removeExtension)

    // configure the folders and prepare setup
    val rootFolder = File("test/target").canonical
    val contentFolder = rootFolder / "content"
    val systemFolder = rootFolder / "system"

    // create a setup instance
    val setup = new Setup
    controller.extman.addExtension(setup)

    // wipe anything old
    if (rootFolder.exists()){
      rootFolder.children foreach (_.deleteDir)
    }

    // and run the setup
    setup.setup(systemFolder, contentFolder, None, quiet = true)

    // and print an overview
    log(s"Configured MMT Testing root in $rootFolder")

    // and flush the report please
    report.flush
  }

  /** return an Archive instance or throw an Error */
  private def getArchive(id: String) = {
    if(!archives.contains(id)){
      testWarn(s"Archive missing from test specification: $id is missing from Test")
    }
    controller.backend.getArchive(id).getOrElse(
      throw testError(s"Archive missing from controller: $id")
    )
  }

  /** check that all the archives get installed properly */
  def shouldInstallArchives : Unit = {
    archives.foreach(installArchive(_))
  }

  /** check that a given archive gets installed properly */
  def installArchive(id: String, version: Option[String] = None): Unit = {
    it should s"get archive $id" in {
      handleLine(s"oaf clone $id")
      // TODO: Install the given version of the archive
      getArchive(id)
    }
  }

  /** checks a single file */
  private def checkFile(archive : Archive, filename : String): List[(Path,Error)] = {
    // create an ErrorHandler to catch all errors
    val errorBuffer = new TestErrorBuffer(controller)
    errorBuffer.reset

    try {
      // read the document in a task that can be cancelled by the stop method
      val ps = ParsingStream.fromSourceFile(archive, FilePath(filename))
      val doc = controller.read(ps, true, true)(errorBuffer) match {
        case d: Document => d
        case _ => throw testError(s"Expected a document: $filename")
      }

      // print a warning for all errors
      val ret = errorBuffer.getAll
      ret.foreach(p => testWarn(Level.toString(p._2.level) + " in " + p._1 + ": " + p._2.shortMsg))
      ret

    } catch {case e: Exception =>
      val msg = e.getClass + ": " + e.getMessage
      throw testError("Unknown Error occured: " + msg, Some(e))
    }
  }

  /** check all .mmt files in the source of an archive and a set of normal files */
  private def check(archive : Archive, files : String*)(onlyfiles : Boolean = false): List[(Path, Error)] = {
    // check all the requested files
    var ret : ListBuffer[(Path,Error)] = new ListBuffer
    ret ++= files.flatMap(f => checkFile(archive, f))

    if (!onlyfiles) {
      // find all files in the root of the archive
      val src = archive.root / archive.resolveDimension(RedirectableDimension("source")).toString
      archive.traverse(RedirectableDimension("source"), FilePath(""), TraverseMode(f => File(f).getExtension.contains("mmt"), _ => true, false)) {
        case Current(inFile, _) =>
          ret ++= checkFile(archive, src.relativize(inFile).toString)
      }
    }

    // and return all the errors
    ret.toList
  }

  /**
    * Checks a set of files inside an archive
    *
    * @param archiveID ID of the archive that we should check files in
    * @param files Set of files to check in
    * @param mayfail a list of archives that are ignored in the return value of the error
    * @param mustfail a list of archives that may not file in the return value of the error
    */
  def shouldCheck(archiveID : String, files : String*)(onlyfiles : Boolean = false,mayfail : List[String] = Nil, mustfail : List[String] = Nil): Unit =
    it should s"build $archiveID" in {
      // find the source folder of the archive
      val archive = getArchive(archiveID)

      // run the check
      val ret = check(archive, files:_*)(onlyfiles)

      // check all the files
      var testOK: Boolean = true
      ret.map(_._1.toString).distinct.foreach( f => {
        // find all errors for this file, default counting to zero
        val errorCounts = ret.filter(_._1.toString == f).groupBy(_._2.level).mapValues(_.length).withDefault(_ => 0)

        // count warning and above
        val errorsAndAbove = errorCounts(Level.Error) + errorCounts(Level.Fatal)
        val warningsAndAbove = errorCounts(Level.Warning) + errorsAndAbove

        // a status message for this file
        var statusMessage = new StringBuffer

        // build an info message to show the user
        val countMessage = errorCounts.map({
          case (t: Level, 1) => s"1 ${Level.toString(t)}"
          case (t: Level, c) => s"$c ${Level.toString(t)}s"
        }).mkString("(", ", ", ")")
        statusMessage.append(s"$archiveID ${archive.root.relativize(File(f))}: $countMessage ")

        // in case a failure was expected
        if(mustfail.contains(f)){
          if(warningsAndAbove == 0){
            statusMessage.append("[SUCCESS] -- [TEST NOT OK] -- expected a failure")
            testOK = false
          } else {
            statusMessage.append("[FAILURE] -- [TEST OK]     -- expected a failure")
          }

          // in case a failure was allowed
        } else if(mustfail.contains(f)) {
          if (errorsAndAbove == 0) {
            statusMessage.append("[SUCCESS] -- [TEST OK]     -- failure allowed")
          } else {
            statusMessage.append("[FAILURE] -- [TEST OK]     -- failure allowed")
          }

          // in case we expected a success
        } else {
          if(errorsAndAbove == 0) {
            statusMessage.append("[SUCCESS] -- [TEST OK]")
          } else {
            statusMessage.append("[FAILURE] -- [TEST NOT OK]")
            testOK = false
          }
        }

        // and log the status message
        log(statusMessage.toString)
      })

      if(!testOK){
        throw testError(s"Archive $archiveID behaved unexpectedly")
      }
    }
}


/** an [[ErrorHandler]] that stores errors in a buffer */
class TestErrorBuffer(controller: Controller) extends ErrorHandler {

  /** a buffer of errors */
  private var errors: ListBuffer[(Path, Error)] = new ListBuffer

  /** resets the internal state to an empty state */
  override def reset {
    errors.clear()
    super.reset
  }

  /** gets all errors and then resets this Forwarder */
  def getAll: List[(Path, Error)] = {
    val ret = errors.toList
    reset
    ret
  }

  /** adds an error to this Forwarder */
  def addError(e: Error) : Unit = e match {
    //generated by parsers
    case s: SourceError =>
      // permit errors in files other than the current one
      val file = controller.backend.resolveLogical(s.ref.container) match {
        case Some((a, p)) => (a / archives.source / p).toString

        case None => s.ref.container match {
          case utils.FileURI(f) => f.toString
          case u => u.toString
        }
      }
      errors += ((DPath(URI(file)), s))

    // generated by checkers
    case e: Invalid =>
      (e match {
        case e: InvalidObject => None
        case e: InvalidElement => Some(e.elem.path)
        case e: InvalidUnit => e.unit.component
      }) foreach (p => errors += ((p,e)))
  }
}
