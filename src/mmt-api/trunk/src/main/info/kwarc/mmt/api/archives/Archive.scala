package info.kwarc.mmt.api.archives
import info.kwarc.mmt.api._
import libraries._
import frontend._
import backend._
import modules._
import ontology._
import patterns._
import symbols._
import objects._
import utils._
import utils.FileConversion._

import scala.collection.mutable._
import scala.xml.NodeSeq

//case class CompilationError(s: String) extends Exception(s)
case class CompilationStep(from: String, to: String, compiler: Compiler)
/** convenience class for traversing an Archive */
case class Current(file: File, path: List[String])
                                                                       
abstract class ROArchive extends Storage with Logger {
  val rootString: String
  override def toString = "archive " + rootString
  val properties: Map[String, String]
  val report : Report
  val logPrefix = "archive"
  
  val narrationBackend : Storage
  
  /** Get a module from content folder (wrapped in <omdoc>) */
  def get(m: MPath) : scala.xml.Node

  def load(p: Path)(implicit controller: Controller) {p match {
    case doc : DPath => narrationBackend.load(doc)
    case mod : MPath =>
      val node = try { get(mod) }
      catch {case e: java.io.FileNotFoundException => throw NotApplicable("file not found")}
      loadXML(mod.doc.uri, node)
    case OMMOD(m) % _ => load(m)
  }}
}

abstract class WritableArchive extends ROArchive {
    val root : File
    val id = properties("id")
    val narrationBase = utils.URI(properties.getOrElse("narration-base", ""))
   
    val sourceDim = properties.get("source").getOrElse("source")
    val narrationDim = properties.get("narration").getOrElse("narration")
    val contentDim = properties.get("content").getOrElse("content")
    val relDim = properties.get("relational").getOrElse("relational")
    val errorDim = properties.get("errors").getOrElse("errors")
    val flatDir = root / "flat"
    
    /**
     * @param dim a dimension in the archive
     * @return the relative path to that
     */
    def /(dim: ArchiveDimension) : File = dim match {
       case `source` => root / sourceDim
       case `content` => root / contentDim
       case `narration` => root / narrationDim
       case `relational` => root / relDim
       case `errors` => root / errorDim
       case Dim(path@_*) => root / path.toList
    }
    
    val timestamps = new TimestampManager(this, root / "META-INF" / "timestamps" )
    
    def includeDir(n: String) : Boolean = n != ".svn" && n != ".mmt"

    val narrationBackend = LocalCopy(narrationBase.schemeNull, narrationBase.authorityNull, narrationBase.pathAsString, this/narration)
    /** Get a module from content folder */ 
    def get(m: MPath) : scala.xml.Node = {
       val p = MMTPathToContentPath(m)
       utils.xml.readFile(p)
    }
    
    protected val custom : ArchiveCustomization = {
       properties.get("customization") match {
          case None => new DefaultCustomization
          case Some(c) => java.lang.Class.forName(c).asInstanceOf[java.lang.Class[ArchiveCustomization]].newInstance
       }
    }
    
    protected def deleteFile(f: File) {
       log("deleting " + f)
       f.delete
    }
    
    /** Get the disk path of the module in the content folder
      * @param m the MPath of the module 
      * @return the File descriptor of the destination  file in the content folder
      */
    def MMTPathToContentPath(m: MPath) : File = this/content / Archive.MMTPathToContentPath(m)

    /** traverses a dimension calling continuations on files and subdirectories */
    def traverse[A](dim: ArchiveDimension, in: List[String], filter: String => Boolean, sendLog: Boolean = true)
                     (onFile: Current => A, onDir: (Current,List[A]) => A = (_:Current,_:List[A]) => ()) : Option[A] = { 
        val inFile = this / dim / in
        if (inFile.isDirectory) {
           if (sendLog) log("entering " + inFile)
           val results = inFile.list flatMap {n =>
              if (includeDir(n)) {
                  val r = traverse(dim, in ::: List(n), filter, sendLog)(onFile, onDir)
                  r.toList
              } else
                 Nil
           }
           val result = onDir(Current(inFile,in), results.toList)
           if (sendLog) log("leaving  " + inFile)
           Some(result)
        } else {
           try {
              if (filter(inFile.getName)) {
                 val r = onFile(Current(inFile, in))
                 Some(r)
              } else
                 None
           } catch {
              case e : Error => report(e); None
           }
        }
    }
}  
    
/** archive management
  * @param root the root folder that contains the source folder
  * @param properties 
  * @param compsteps the list of compilation steps that produces an already initialized compiler
  * @param report the reporting mechanism
  * 
  * Archive is a very big class, so most of its functionality is outsourced to various traits that are mixed in here
  */
class Archive(val root: File, val properties: Map[String,String], val report: Report)
    extends WritableArchive with ValidatedArchive with ScalaArchive with ZipArchive {

   val rootString = root.toString
   def clean(in: List[String] = Nil, dim: String) {
       traverse(Dim(dim), in, _ => true) {case Current(inFile, inPath) =>
         deleteFile(inFile)
       }
    }

  /**
   * computes the flattened theories by elaborating the patterns
   * @param in input path
   * @param controller the controller
   */
    def produceFlat(in: List[String], controller: Controller) {
       val inFile = this/content/ in
       log("to do: [CONT -> FLAT]        -> " + inFile)
       if (inFile.isDirectory) {
           inFile.list foreach {n =>
              if (includeDir(n)) produceFlat(in ::: List(n), controller)
           }
       } else if (inFile.getExtension == Some("omdoc")) {
           val mpath = Archive.ContentPathToMMTPath(in)
           val mod = controller.globalLookup.getModule(mpath)
           val ie = new InstanceElaborator(controller)
           val flatNode = mod match {
              case thy: DeclaredTheory =>
                 ie.elaborate(thy)
                 thy.toNodeElab
              case _ => mod.toNode
           }
           val flatNodeOMDoc = <omdoc xmlns="http://omdoc.org/ns" xmlns:om="http://www.openmath.org/OpenMath">{flatNode}</omdoc>
           xml.writeFile(flatNodeOMDoc, flatDir / in)
           controller.delete(mpath)
        }
       log("done:  [CONT -> FLAT]        -> " + inFile)
    }


  /**
   * collapses (de-structures) the theory graph by replacing arrows with automatically constructed objects (e.g. a view becomes a theory by applying it to the
   * domain).
   * @param in input path
   * @param controller the controller
   */
  def produceEnriched(in : List[String], modElab : ModuleElaborator, controller: Controller) {
    val inFile= this/content / in
    //val modElab = new ModuleElaborator(controller)
    val enrichedDir = root / "enriched"
    log("to do: [CONT -> FLAT]       -> " + inFile)
    if (inFile.isDirectory) {
      inFile.list foreach {n =>
        if (includeDir(n)) produceEnriched(in ::: List(n), modElab, controller)
      }
    } else if (inFile.getExtension == Some("omdoc")) {
      try {
        val mpath = Archive.ContentPathToMMTPath(in)
        val mod = controller.globalLookup.getModule(mpath)
        modElab.apply(mod){
          case m : Module =>
            val collapsedOMDoc = <omdoc xmlns="http://omdoc.org/ns" xmlns:om="http://www.openmath.org/OpenMath">{m.toNode}</omdoc>
            xml.writeFile(collapsedOMDoc, enrichedDir / Archive.MMTPathToContentPath(m.path))
          case _ => None
        }
      } catch {
        case e : Throwable => log("ERR : " + e)
      }
    }
    log("done:  [CONT -> FLAT]       -> " + inFile)
  }

  def readRelational(in: List[String] = Nil, controller: Controller, kd: String) {
       if ((this/relational).exists) {
          traverse(relational, in, Archive.extensionIs(kd)) {case Current(inFile, inPath) =>
             utils.File.ReadLineWise(inFile) {line => 
               val re = RelationalElement.parse(line, DPath(narrationBase))
               re match {
                   case Relation(Includes, to: MPath, from: MPath) =>
                      controller.library.addImplicit(OMMOD(from), OMMOD(to), OMIDENT(OMMOD(to)))
                   case Relation(HasMeta, thy: MPath, meta: MPath) =>
                      controller.library.addImplicit(OMMOD(meta), OMMOD(thy), OMIDENT(OMMOD(thy)))
                   case _ => 
               }
               controller.depstore += re
            }
          }
       }
    }
}



object Archive {
   /** a string containing all characters that are illegal in file names */ 
   val illegalChars = "'"
   // TODO: (un)escape illegal characters, make case-insensitive distinct
   def escape(s: String) : String = s.replace("'","(apos)")
   def unescape(s: String) : String = s.replace("(apos)", "'")
    // scheme..authority / seg / ments / name.omdoc ----> scheme :// authority / seg / ments ? name
   def ContentPathToMMTPath(segs: List[String]) : MPath = segs match {
       case Nil => throw ImplementationError("")
       case hd :: tl =>
          val p = hd.indexOf("..")
          val fileNameNoExt = tl.length match {
            case 0 => throw ImplementationError("")
            case _ => tl.last.lastIndexOf(".") match {
               case -1 => tl.last
               case i => tl.last.substring(0,i)
            }
          }
          DPath(URI(hd.substring(0,p), hd.substring(p+2)) / tl.init) ? unescape(fileNameNoExt)
   }
    // scheme..authority / seg / ments  ----> scheme :// authority / seg / ments
   def ContentPathToDPath(segs: List[String]) : DPath = segs match {
       case Nil => DPath(URI.empty)
       case hd :: tl =>
          val p = hd.indexOf("..")
          DPath(URI(hd.substring(0,p), hd.substring(p+2)) / tl)
   }
    /** Get the disk path of the module in the content folder
      * @param m the MPath of the module 
      * @return the File descriptor of the destination  file in the content folder
      */
    def MMTPathToContentPath(m: MPath) : List[String] = {            // TODO: Use narrationBase instead of "NONE"?
       val uri = m.parent.uri
       val schemeString = uri.scheme.map(_ + "..").getOrElse("")
       (schemeString + uri.authority.getOrElse("NONE")) :: uri.path ::: List(escape(m.name.toPath) + ".omdoc")
    }
   /**
    * Makes sure that a path refers to a file, not to a folder, using .extension files to store information about folders  
    * @param segs a path in a folder with narration structure
    * @param extension a file extension
    * @return segs with an appended segment ".extension" if there is no such segment yet
    */
    def narrationSegmentsAsFile(segs: List[String], extension: String) : List[String] = {
      if (! segs.isEmpty && segs.last.endsWith("."+extension)) segs
      else segs ::: List("."+extension)
    }
   /**
    * Inverse of narrationSegmentsAsFile  
    * @param segs a path in a folder with narration structure that ends in ".extension"
    * @param extension a file extension
    * @return segs with a final segment ".extension" removed if there is one
    * 
    * This is inverse to narrationSegmentsAsFile if all files and no folders in the respective dimension end in ".extension"
    */
    def narrationSegmentsAsFolder(segs: List[String], extension: String) : List[String] = {
      if (! segs.isEmpty && segs.last == "." + extension) segs.init
      else segs
    }
    /** returns a functions that filters by file name extension */
    def extensionIs(e: String) : String => Boolean = _.endsWith("." + e)  
}