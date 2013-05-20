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

abstract class BuildTarget extends Extension {
   /** the input archive folder */
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

abstract class TraversingBuildTarget extends BuildTarget {
   /** the output archive folder */
   val outDim: String
   /** the file extension used for generated files, defaults to outDim, override as needed */
   val outExt: String = outDim
   /**
    * there is no inExt, instead we test to check which files should be used; 
    * this is often a test for the file extension
    * 
    * This must be such that all auxiliary files are skipped. 
    */
   def includeFile(name: String) : Boolean
   
   /** the main abstract method that implementations must provide: builds one file
     * @param in the input file 
     * @param dpath the base URI of the input file
     * @param out the output file
     */ 
   def buildOne(inFile: File, dpath: Option[DPath], outFile: File): List[Error]

   /** entry for recursive building */
   def build(a: Archive, args: List[String], in: List[String] = Nil) {
      buildAux(in)(a)
   }
   /** entry for recursive updating */
   def update(a: Archive, args: List[String], in: List[String] = Nil) {
      updateAux(in)(a)
   }
   /** entry for recursive cleaning */
   def clean (a: Archive, args: List[String], in: List[String] = Nil) {
      cleanAux(in)(a)
   }
   
   /** recursive building */
   private def buildAux(in : List[String] = Nil)(implicit a: Archive) {
       //reset errors
       val errorMap = a.errors(key)
       a.traverse(inDim, in, includeFile, false) {case Current(_,inPath) => errorMap(inPath) = Nil}
       //build every file
       val prefix = "[" + inDim + " -> " + outDim + "] "
       a.traverse(inDim, in, includeFile) {case Current(inFile,inPath) =>
         val outFile = (a.root / outDim / inPath).setExtension(outExt)
         log(prefix + inFile + " -> " + outFile)
         val errors = buildOne(inFile, Some(DPath(a.narrationBase / inPath)), outFile)
           errorMap(inPath) = errors
           if (! errors.isEmpty) {
             log(errors.mkString("errors follow\n", "\n", "\n"))
           }
           a.timestamps(key).set(inPath)
       }
    }
   
    /** recursively deletes all files produced in the compilation chain */
    private def cleanAux(in: List[String] = Nil)(implicit a: Archive) {
       a.traverse(outDim, in, _ => true) {case Current(inFile, _) =>
          delete(inFile)
       }
    }
    /** recursively reruns build if the input file has changed
     *  
     * the decision is made based on the time stamps and the system's last-modified date
     */  
    private def updateAux(in: List[String] = Nil)(implicit a: Archive) {
       a.traverse(inDim, in, _ => true) {case Current(inFile, inPath) =>
          a.timestamps(key).modified(inPath) match {
             case Deleted =>
                delete(inFile)
             case Added =>
                buildAux(inPath)
             case Modified =>
                delete(inFile)
                buildAux(inPath)
             case Unmodified => //nothing to do
          }
       }
    }
}