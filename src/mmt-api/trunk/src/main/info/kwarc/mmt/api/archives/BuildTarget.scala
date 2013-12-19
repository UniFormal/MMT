package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import frontend._
import utils.File
import utils.FileConversion._

sealed abstract class BuildTargetModifier {
   def toString(dim: String) : String
}
case object Clean  extends BuildTargetModifier {
   def toString(dim: String) = "-" + dim
}
case object Update extends BuildTargetModifier {
   def toString(dim: String) = dim + "*"
}
case object Build  extends BuildTargetModifier {
   def toString(dim: String) = dim
}

/** A BuildTarget provides build/update/clean methods that generate one or more dimensions in an [[Archive]]
 *  from an input dimension.
 */
abstract class BuildTarget extends Extension {
   /** the input dimension/archive folder */
   def inDim:  ArchiveDimension

   /** a string identifying this build target, used for parsing commands, logging, error messages
    */
   def key: String
   
   /** defaults to the key */
   override def logPrefix = key

   /** determines whether this builder can be used to build a certain target; by default: if it is the key */
   def isApplicable(k: String) = k == key

   /** number of required arguments, defaults to 0, override as needed */
   def requiredArguments(m: BuildTargetModifier): Int = 0

   /** build this target in a given archive */
   def build (a: Archive, args: List[String], in: List[String])
   /** update this target in a given archive */
   def update(a: Archive, args: List[String], in: List[String])
   /** clean this target in a given archive */
   def clean (a: Archive, args: List[String], in: List[String])

   /** the main function to run the build target
    *  
    *  @param modifier chooses build, clean, or update
    *  @param arch the archive to build on
    *  @param in the folder inside the archive's inDim folder to which building in restricted (i.e., Nil for whole archive)
    *  @param additional arguments for the discretion of the BuildTarget; number must be equal to requiredArguments(modifier)
    */
   def apply(modifier: BuildTargetModifier, arch: Archive, in: List[String], args: List[String]) {
      val reqArgs = requiredArguments(modifier)
      if (reqArgs != args.length)
         throw ParseError("wrong nunmber of arguments, required: " + reqArgs)
      modifier match {
         case Update => update(arch, args, in)
         case Clean  => clean(arch, args, in)
         case Build  => build(arch, args, in)
      }
   }
   
   /** registers an archive with this target */
   def register(arch: Archive) {
      arch.timestamps.add(key, inDim.toString)
      arch.errors.add(key)
   }
   /** unregisters an archive with this target */
   def unregister(arch: Archive) {}

   /** auxiliary method for deleting a file */
   protected def delete(f: File) {
       log("deleting " + f)
       f.delete
   }
}

/** auxiliary type to represent the parameters and result of building a file/directory */
abstract class BuildTask {
   /** build targets should set this to true if they skipped the file so that it is not passed on to the parent directory */
   var skipped = false
   /** build targets should add all errors here */
   var errors : List[Error] = Nil
}
/**
 * passed when building a file
 * @param inFile the input file
 * @param inPath the path of the input file inside the archive, relative to the input dimension
 * @param dpath the base to use for resolving relative names
 * @param outFile the intended output file
 */
class BuildFile(val inFile: File, val inPath: List[String], val dpath: DPath, val outFile: File) extends BuildTask
/**
 * passed when building a directory
 * @param inFile the input file
 * @param inPath the path of the input file inside the archive, relative to the input dimension
 * @param outFile the intended output file
 */
class BuildDir(val inFile: File, val inPath: List[String], val outFile: File) extends BuildTask {
   def dirName = outFile.segments.init.last
}

/**
 * This trait provides common functionality for BuildTargets that traverse all files in the input dimension.
 * 
 * It implements BuildTarget in terms of a single abstract method called to build a path in the archive.
 */
abstract class TraversingBuildTarget extends BuildTarget {
   /** the output archive folder */
   def outDim: ArchiveDimension
   
   /** the file extension used for generated files, defaults to outDim, override as needed */
   def outExt: String = outDim match {
      case Dim(path@_*) => path.last
      case d => d.toString
   }
   /** the name that is used for the special file representing the containing folder, empty by default */
   protected val folderName = ""
   
   private def outPath(a: Archive, inPath: List[String]) = (a / outDim / inPath).setExtension(outExt)
   private def folderOutPath(a: Archive, inPath: List[String]) = a / outDim / inPath / (folderName + "." + outExt)
   
   /**
    * there is no inExt, instead we test to check which files should be used; 
    * this is often a test for the file extension
    * 
    * This must be such that all auxiliary files are skipped. 
    */
   def includeFile(name: String) : Boolean

   /** the main abstract method that implementations must provide: builds one file
     * @param a the containing archive  
     * @param bfFile information about input/output file etc
     */ 
   def buildFile(a: Archive, bf: BuildFile)

   /** similar to buildOne but called on every directory (after all its children have been processed)
     * @param a the containing archive  
     * @param bd information about input/output file etc
     * @param buildChildren results from building the children
     * This does nothing by default and can be overridden if needed.
     */ 
   def buildDir(a: Archive, bd: BuildDir, builtChildren: List[BuildTask]) {}
   
   /** entry point for recursive building */
   def build(a: Archive, args: List[String], in: List[String] = Nil) {
      buildAux(in)(a)
   }
   /** recursive building */
   private def buildAux(in : List[String] = Nil)(implicit a: Archive) {
       //reset errors
       val errorMap = a.errors(key)
       a.traverse(inDim, in, includeFile, false) {case Current(_,inPath) => errorMap(inPath) = Nil}
       //build every file
       val prefix = "[" + inDim + " -> " + outDim + "] "
       a.traverse[BuildTask](inDim, in, includeFile) ({case Current(inFile,inPath) =>
           val outFile = outPath(a, inPath)
           log(prefix + inFile + " -> " + outFile)
           val bf = new BuildFile(inFile, inPath, DPath(a.narrationBase / inPath), outFile)
           buildFile(a, bf)
           errorMap(inPath) = bf.errors
           if (! bf.errors.isEmpty) {
             log("errors follow")
             bf.errors foreach log
           }
           a.timestamps(key).set(inPath)
           bf
       }, {
          case (Current(inDir, inPath), builtChildren) =>
             val outFile = folderOutPath(a, inPath)
             val bd = new BuildDir(inDir, inPath, outFile) 
             buildDir(a, bd, builtChildren)
             bd
       })
    }

   /** additional method that implementations may provide: cleans one file
     * @param a the containing archive  
     * @param curr the outDim file to be deleted
     * deletes the output file by default, may be overridden to, e.g., delete auxiliary files
     */ 
   def cleanFile(a: Archive, curr: Current) {
      delete(curr.file)
   }
   /** additional method that implementations may provide: cleans one directory
     * @param a the containing archive  
     * @param curr the outDim directory to be deleted
     * does nothing by default
     */ 
   def cleanDir(a: Archive, curr: Current) {}

   /** recursively delete output files */
   def clean (a: Archive, args: List[String], in: List[String] = Nil) {
       a.traverse[Unit](outDim, in, Archive.extensionIs(outExt))({c => cleanFile(a, c)}, {case (c,_) => cleanDir(a, c)})
   }
   
   /** recursively reruns build if the input file has changed
     *  
     * the decision is made based on the time stamps and the system's last-modified date
     */  
   def update(a: Archive, args: List[String], in: List[String] = Nil) {
       a.traverse[Boolean](inDim, in, _ => true) ({case c @ Current(inFile, inPath) =>
          a.timestamps(key).modified(inPath) match {
             case Deleted =>
                val outFile = outPath(a, inPath)
                cleanFile(a, c)
                true
             case Added =>
                buildAux(inPath)(a)
                true
             case Modified =>
                val outFile = outPath(a, inPath)
                cleanFile(a, c)
                buildAux(inPath)(a)
                false
             case Unmodified =>
                //nothing to do
                false
          }
       }, {case (c @ Current(inDir, inPath), childChanged) =>
          if (childChanged.exists(_ == true)) {
             val outFile = folderOutPath(a, inPath)
             val bd = new BuildDir(inDir, inPath, outFile) 
             buildDir(a, bd, Nil) // TODO pass proper builtChildren
             false
          } else
             false
       })
   }
}