package info.kwarc.mmt.api.archives
import info.kwarc.mmt.api._
import frontend._
import backend._
import utils._
import utils.FileConversion._

// obsolete; Compiler now inherits from BuildTarget
/*
trait CompilableArchive extends WritableArchive {
    // stores for each file the time of the last call of produceCompiled
    private val compiledTimestamps = new Timestamps(root / sourceDim, root / "META-INF" / "timestamps" / sourceDim)
    /** apply all compilation steps, e.g., from "source" into "compiled" */
    def produceCompiled(in : List[String] = Nil) {
       //reset errors
       compsteps match {
          case Some(CompilationStep(from, _ , compiler) :: _) => 
             traverse(from, in, compiler.includeFile, false) {case Current(_,inPath) => compErrors(inPath) = Nil}
          case _ => return
       }
       //execute every compilation step for each file
       compsteps.get map {case CompilationStep(from,to,compiler) =>
          val prefix = "[" + from + " -> " + to + "] "
          traverse(from, in, compiler.includeFile) {case Current(inFile,inPath) =>
            val outFile = (root / to / inPath).setExtension("???")
            log(prefix + inFile + " -> " + outFile)
            // only compile if the previous compilation step did not report errors
            if (compErrors.getOrElse(inPath, Nil) == Nil) {
              val errors = compiler.compile(inFile, Some(DPath(narrationBase / inPath)), outFile)
              compErrors(inPath) = errors
              if (! errors.isEmpty)
                log(errors.mkString("errors follow\n", "\n", "\n"))
              compiledTimestamps.set(inPath)
            }
          }
        }
    }
    /** deletes all files produced in the compilation chain */
    def deleteCompiled(in: List[String] = Nil) {
       compsteps.getOrElse(Nil) map {case CompilationStep(_,to,_) =>
          traverse(to, in, _ => true) {case Current(_, inPath) =>
             deleteFile(root / to / inPath) //TODO delete files with correct extension
          }
        }
    }
    /** partially reruns produceCompiled using the time stamps and the system's last-modified information */  
    def updateCompiled(in: List[String] = Nil) {
       traverse(sourceDim, in, _ => true) {case Current(inFile, inPath) =>
          compiledTimestamps.modified(inPath) match {
             case Deleted =>
                deleteCompiled(inPath)
             case Added =>
                produceCompiled(inPath)
             case Modified =>
                deleteCompiled(inPath)
                produceCompiled(inPath)
             case Unmodified => //nothing to do
          }
       }
    }
}
*/