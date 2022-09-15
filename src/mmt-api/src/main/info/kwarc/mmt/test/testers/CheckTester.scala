package info.kwarc.mmt.test.testers

import info.kwarc.mmt.api.Level.Level
import info.kwarc.mmt.api.archives.{Archive, Current, RedirectableDimension, TraverseMode}
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.parser.ParsingStream
import info.kwarc.mmt.api.utils.{File, FilePath, URI}
import info.kwarc.mmt.api.{DPath, Error, ErrorHandler, Invalid, InvalidElement, InvalidObject, InvalidUnit, Level, Path, SourceError, archives, utils}

import scala.collection.mutable.ListBuffer

/** implements testing of individual files */
trait CheckTester extends BaseTester {
  this: ArchiveTester =>

  /** checks a single file */
  private def checkFile(archive : Archive, filename : String): List[(Path,Error)] = {
    // create an ErrorHandler to catch all errors
    val errorBuffer = new TestErrorBuffer(controller)
    errorBuffer.reset

    try {
      // read the document in a task that can be cancelled by the stop method
      val ps = ParsingStream.fromSourceFile(archive, FilePath(filename),nsMapOpt = Some(controller.getNamespaceMap))
      val doc = controller.read(ps, true, true)(errorBuffer) match {
        case d: Document => d
        case _ => throw testError(s"Expected a document: $filename")
      }

      errorBuffer.getAll
    } catch {case e: Exception =>
      val msg = e.getClass.toString + ": " + e.getMessage
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
    * Checks that a given dimension is deleted from an archive
    * @param archiveID
    * @param dimensionName
    */
  def shouldClearTarget(archiveID: String, dimensionName: String): Unit =
    test(s"delete $archiveID target $dimensionName", {

      val archive = this.getArchive(archiveID)
      val folder = archive.root / archive.resolveDimension(RedirectableDimension(dimensionName))

      if(folder.exists()){
        log(s"deleted $folder")
        folder.deleteDir
      } else {
        log(s"skipped")
      }
    })

  /**
    * Checks a set of files inside an archive
    *
    * @param archiveID ID of the archive that we should check files in
    * @param files Set of files to check in
    * @param onlyfiles If set to true, only build those files expliticly
    * @param mayfail a list of archives that are ignored in the return value of the error
    * @param mustfail a list of archives that may not file in the return value of the error
    */
  def shouldCheck(archiveID : String, files : String*)(onlyfiles : Boolean = false, mayfail : List[String] = Nil, mustfail : List[String] = Nil): Unit =
    test(s"check $archiveID", {
      // find the source folder of the archive
      val archive = this.getArchive(archiveID)

      // run the check
      val ret = check(archive, files:_*)(onlyfiles)

      // check all the files
      var testOK: Boolean = true
      var firstTestError: Option[Error] = None

      ret.map(_._1.toString).distinct.foreach( f => {

        // find all messages for this file, with different potential levels
        val messages = ret.filter(_._1.toString == f)
        val messageCounts = messages.groupBy(_._2.level).view.mapValues(_.length).toMap.withDefault(_ => 0)

        // find all the different types of errors
        val errorsAndAbove = messageCounts(Level.Error) + messageCounts(Level.Fatal)
        val warningsAndAbove = messageCounts(Level.Warning) + errorsAndAbove

        // prepare a prefix message
        val messagePrefix = s"$archiveID ${archive.root.relativize(File(f))}"
        val statusMessage = new StringBuffer

        // build an info message to show the user
        val countMessage = messageCounts.map({
          case (t: Level, 1) => s"1 ${t.toString}"
          case (t: Level, c) => s"$c ${t.toString}s"
        }).mkString(", ")

        statusMessage.append(s"$messagePrefix: $countMessage ")

        // in case a failure was expected
        if(mustfail.contains(f)){
          if(warningsAndAbove == 0){
            statusMessage.append("[READ SUCCESS] -- [TEST NOT OK] -- expected a failure")
            testOK = false
          } else {
            statusMessage.append("[READ FAILURE] -- [TEST OK]     -- expected a failure")
          }

          // in case a failure was allowed
        } else if(mustfail.contains(f)) {
          if (errorsAndAbove == 0) {
            statusMessage.append("[READ SUCCESS] -- [TEST OK]     -- failure allowed")
          } else {
            statusMessage.append("[READ FAILURE] -- [TEST OK]     -- failure allowed")
          }

          // in case we expected a success
        } else {
          if(errorsAndAbove == 0) {
            statusMessage.append("[READ SUCCESS] -- [TEST OK]")
          } else {
            statusMessage.append("[READ FAILURE] -- [TEST NOT OK]")
            testOK = false
            if(firstTestError.isEmpty) {firstTestError = Some(messages.head._2)}
          }
        }

        // and then the status message
        log(statusMessage.toString)

        logGroup {
          // print all the error messages
          messages.foreach(m =>
            log(m._2.shortMsg)
          )
        }
      })

      if(!testOK){
        throw testError(s"Archive $archiveID behaved unexpectedly", firstTestError)
      }
    })
}


/** an [[ErrorHandler]] that stores errors in a buffer */
class TestErrorBuffer(controller: Controller) extends ErrorHandler {

  /** a buffer of errors */
  private var errors: ListBuffer[(Path, Error)] = new ListBuffer

  /** resets the internal state to an empty state */
  override def reset: Unit = {
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
