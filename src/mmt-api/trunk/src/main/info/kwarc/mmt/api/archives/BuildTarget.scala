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
   val inDim:  String

   /** a string identifying this build target, used for parsing commands, logging, error messages
    */
   val key: String
   
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
      arch.timestamps.add(key, inDim)
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

/** auxiliary type to represent the result of building a file/directory */
sealed abstract class BuildResult
case class BuiltFile(inPath: List[String], outFile: File) extends BuildResult
case class BuiltDir(inPath: List[String], outFile: File) extends BuildResult {
   def dirName = outFile.segments.init.last
}

/**
 * This trait provides common functionality for BuildTargets that traverse all files in the input dimension.
 * 
 * It implements BuildTarget in terms of a single abstract method called to build a path in the archive.
 */
trait GenericTraversingBuildTarget extends BuildTarget {
   /** the output archive folder */
   val outDim: String
   /** the file extension used for generated files, defaults to outDim, override as needed */
   def outExt: String = outDim
   /** the name that is used for the special file representing the containing folder, empty by default */
   protected val folderName = ""
   
   private def outPath(root: File, inPath: List[String]) = (root / outDim / inPath).setExtension(outExt)
   
   /**
    * there is no inExt, instead we test to check which files should be used; 
    * this is often a test for the file extension
    * 
    * This must be such that all auxiliary files are skipped. 
    */
   def includeFile(name: String) : Boolean

   /** the main abstract method that implementations must provide: builds one file
     * @param a the containing archive  
     * @param inFile the input file 
     * @param inPath the path in the archive to the input file
     * @param outFile the output file
     */ 
   def buildFile(a: Archive, inFile: File, inPath: List[String], outFile: File): List[Error]

   /** similar to buildOne but called on every directory (after all its children have been processed)
     * @param a the containing archive  
     * @param inDir the input directory 
     * @param inPath the path in the archive to the input file
     * @param childPaths the paths of the children that were built
     * @param outFile the output file
     * This does nothing by default and can be overridden if needed.
     */ 
   def buildDir(a: Archive, inDir: File, inPath: List[String], buildChildren: List[BuildResult], outFile: File): List[Error] = Nil
   
   /** entry point for recursive building */
   def build(a: Archive, args: List[String], in: List[String] = Nil) {
      buildAux(in)(a)
   }
   /** recursively reruns build if the input file has changed
     *  
     * the decision is made based on the time stamps and the system's last-modified date
     */  
   def update(a: Archive, args: List[String], in: List[String] = Nil) {
       a.traverse(inDim, in, _ => true) {case Current(inFile, inPath) =>
          a.timestamps(key).modified(inPath) match {
             case Deleted =>
                val outFile = outPath(a.root, inPath)
                delete(outFile)
             case Added =>
                buildAux(inPath)(a)
             case Modified =>
                val outFile = outPath(a.root, inPath)
                delete(outFile)
                buildAux(inPath)(a)
             case Unmodified => //nothing to do
          }
       }
   }
   /** recursively delete output files */
   def clean (a: Archive, args: List[String], in: List[String] = Nil) {
       a.traverse(outDim, in, _ => true) {case Current(inFile, _) =>
          delete(inFile)
       }
   }
   
   /** recursive building */
   private def buildAux(in : List[String] = Nil)(implicit a: Archive) {
       //reset errors
       val errorMap = a.errors(key)
       a.traverse(inDim, in, includeFile, false) {case Current(_,inPath) => errorMap(inPath) = Nil}
       //build every file
       val prefix = "[" + inDim + " -> " + outDim + "] "
       a.traverse[BuildResult](inDim, in, includeFile) ({case Current(inFile,inPath) =>
         val outFile = outPath(a.root, inPath)
         log(prefix + inFile + " -> " + outFile)
         val errors = buildFile(a, inFile, inPath, outFile)
           errorMap(inPath) = errors
           if (! errors.isEmpty) {
             log("errors follow")
             errors foreach log
           }
           a.timestamps(key).set(inPath)
           BuiltFile(inPath, outFile)
       }, {
          case (Current(inDir, inPath), buildChildren) =>
             val outFile = a.root / outDim / inPath / (folderName + "." + outExt)
             buildDir(a, inDir, inPath, buildChildren, outFile)
             BuiltDir(inPath, outFile)
       })
    }
}

abstract class TraversingBuildTarget extends GenericTraversingBuildTarget {
   def buildOne(inFile: File, dpath: DPath, outFile: File): List[Error]
   def buildFile(a: Archive, inFile: File, in: List[String], outFile: File) =
      buildOne(inFile, DPath(a.narrationBase / in), outFile)
}