package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api.ExtensionError
import info.kwarc.mmt.api.utils._
import File._

/** a simple build target for testing */
class DummyBuildTarget extends TraversingBuildTarget {
  def key = "foobar"

  def inDim = source

  def includeFile(n: String) = n.endsWith(".foo")

  def outDim = RedirectableDimension(key)

  override val outExt = "bar"

  override def estimateResult(bt: BuildTask) = {
    val (needed, _) = readSource(bt: BuildTask)
    val fooFiles = needed.map(l => (l, bt.archive / inDim / l)).collect {
      case (l, f) if f.exists => l
    }
    val ds = fooFiles.map(l => FileBuildDependency(key, bt.archive, File(l).toFilePath))
    BuildSuccess(ds, Nil)
  }

  def readSource(bt: BuildTask): (List[String], List[String]) = {
    var missingDeps: List[String] = Nil
    var providedEntities: List[String] = Nil
    var readDeps = true
    val source = scala.io.Source.fromFile(bt.inFile)
    source.getLines().foreach { line =>
      val l = line.trim
      if (l == "#provides") {
        readDeps = false
      } else if (readDeps) {
        missingDeps ::= l
      }
      else {
        providedEntities ::= l
      }
    }
    (missingDeps, providedEntities)
  }

  case class FooError(s: String) extends ExtensionError(key, s)

  def buildFile(bf: BuildTask): BuildResult = {
    val (needed, provided) = readSource(bf)
    val providedFiles = provided.map(File(_))
    val (used, missingDeps) = needed.map(l => File(bf.archive / outDim / l).setExtension(outExt)).partition(_.exists)
    val usedPDs = used.map(PhysicalDependency)
    if (missingDeps.isEmpty) {
      if (provided.isEmpty) {
        logResult("failure for " + bf.inFile)
        bf.errorCont(FooError("nothing provided"))
        BuildFailure(usedPDs, Nil)
      } else {
        File.write(bf.outFile, provided.mkString("", "\n", "\n"))
        logResult("success " + bf.outPath)
        BuildSuccess(usedPDs, (bf.outFile :: providedFiles).map(PhysicalDependency))
      }
    }
    else {
      var err = "missing dependencies for " + bf.inPath + ": " + missingDeps.mkString(" ")
      logResult(err)
      bf.errorCont(FooError(err))
      MissingDependency(missingDeps.map(PhysicalDependency), providedFiles.map(PhysicalDependency))
    }
  }
}
