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
  //def traverse(dim: String, in: List[String], filter: String => Boolean)(f: List[String] => Unit)

  val rootString: String
  override def toString = "archive rootString"
  val properties: Map[String, String]
  val report : Report
  val logPrefix = "archive"
  
  val narrationBackend : Storage

  /** Get a module from content folder (wrapped in <omdoc>) */
  def get(m: MPath) : scala.xml.Node

  def get(p: Path)(implicit cont: (URI,NodeSeq) => Unit) {p match {
    case doc : DPath => narrationBackend.get(doc)
    case mod : MPath =>
      val node = try { get(mod) }
      catch {case e: java.io.FileNotFoundException => throw NotApplicable}
      cont(mod.doc.uri, node)
    case OMMOD(m) % _ => get(m)
  }}
}

abstract class WritableArchive extends ROArchive {
    val root : File
    val id = properties("id")
    val narrationBase = utils.URI(properties.getOrElse("narration-base", ""))
   
    val sourceDim = properties.get("source").getOrElse("source")
    val compiledDim = properties.get("compiled").getOrElse("compiled")
    val sourceDir = root / sourceDim
    val narrationDir = root / "narration"
    val contentDir = root / "content"
    val relDir = root / "relational"
    val flatDir = root / "flat"
    
    val timestamps = new TimestampManager(this, root / "META-INF" / "timestamps" )
    val errors     = new ErrorManager(this)
    
    def includeDir(n: String) : Boolean = n != ".svn"

    val narrationBackend = LocalCopy(narrationBase.schemeNull, narrationBase.authorityNull, narrationBase.pathAsString, narrationDir)
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
    
    /** compilation errors */
    protected val compErrors = new LinkedHashMap[List[String], List[SourceError]]
    def getErrors(l: List[String]) = compErrors.getOrElse(l, Nil)

    protected def deleteFile(f: File) {
       log("deleting " + f)
       f.delete
    }
    
    /** Get the disk path of the module in the content folder
      * @param m the MPath of the module 
      * @return the File descriptor of the destination  file in the content folder
      */
    def MMTPathToContentPath(m: MPath) : File = contentDir / Archive.MMTPathToContentPath(m)

    /** traverses a dimension calling continuations on files and subdirectories */
    def traverse[A](dim: String, in: List[String], filter: String => Boolean, sendLog: Boolean = true)
                     (onFile: Current => A, onDir: (Current,List[A]) => A = (_:Current,_:List[A]) => ()) : Option[A] = { 
        val inFile = root / dim / in
        if (inFile.isDirectory) {
           if (sendLog) log("entering " + inFile)
           val results = inFile.list flatMap {n =>
              if (includeDir(n)) {
                  val r = traverse(dim, in ::: List(n), filter, sendLog)(onFile, onDir)
                  r.toList
              } else
                 Nil
           }
           val result = onDir(Current(inFile,in), results.toList.reverse)
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
       traverse(dim, in, _ => true) {case Current(inFile, inPath) =>
         deleteFile(inFile)
       }
    }

  /**
   * computes the flattened theories by elaborating the patterns
   * @param in input path
   * @param controller the controller
   */
    def produceFlat(in: List[String], controller: Controller) {
       val inFile = contentDir / in
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
    val inFile= contentDir / in
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

        //controller.delete(mpath)
      } catch {
        case e : Throwable => log("ERR : " + e)
      }
    }
    log("done:  [CONT -> FLAT]       -> " + inFile)
  }

    /** Generate presentation from content */
    def producePres(in : List[String] = Nil, param : String, controller : Controller) {
      val inFile = contentDir / in
      log("to do: [CONT -> PRES]        -> " + inFile)

      traverse("content", in, Archive.extensionIs("omdoc")) { case Current(inFile, inPath) =>
        val outFile = (root / "presentation" / param / inPath).setExtension("xhtml")
        controller.read(inFile,None)
        val mpath = Archive.ContentPathToMMTPath(inPath)
        val file = File(outFile)
        file.getParentFile.mkdirs
        val fs = new presentation.FileWriter(file)
        frontend.Present(Get(mpath),param).make(controller, fs)
        fs.done
      }
      log("done:  [CONT -> PRES]        -> " + inFile)
    }
    
    def getPresentation(in : MPath, controller: Controller, nset : MPath) : Option[scala.xml.Node] = {
      try {
      val fpath = root / "presentation" / nset.last / {
        val uri = in.parent.uri
        val schemeString = uri.scheme.map(_ + "..").getOrElse("")
        (schemeString + uri.authority.getOrElse("NONE")) :: uri.path ::: List(Archive.escape(in.name.flat) + ".xhtml")
      }
      
      val src = scala.io.Source.fromFile(fpath)
      val cp = scala.xml.parsing.ConstructingParser.fromSource(src, false)
      val input : scala.xml.Node = cp.document()(0)
      src.close
      Some(input)
      } catch {
        case _ : Throwable => None
      }
    }

    def readRelational(in: List[String] = Nil, controller: Controller, kd: String) {
       if (relDir.exists) {
          traverse("relational", in, Archive.extensionIs(kd)) {case Current(inFile, inPath) =>
             ontology.RelationalElementReader.read(inFile, DPath(narrationBase), controller.depstore)
          }
          //TODO this should only add implicits for the dependencies it read
          controller.depstore.getDeps foreach {
             case Relation(Includes, to: MPath, from: MPath) => controller.library.addImplicit(OMMOD(from), OMMOD(to), OMIDENT(OMMOD(to)))
             case Relation(HasMeta, thy: MPath, meta: MPath) => controller.library.addImplicit(OMMOD(meta), OMMOD(thy), OMIDENT(OMMOD(thy)))
             case _ => 
          }
       }
    }
/*
    def readNotation(in: List[String] = Nil, controller: Controller) {
       if ((root / "notation").exists) {
          traverse("notation", in, Archive.extensionIs("not")) {case Current(inFile, inPath) =>
             val thy = Archive.ContentPathToMMTPath(inPath)
             File.ReadLineWise(inFile) {line => controller.notstore.add(presentation.Notation.parseString(line, thy))}
          }
       }
    }
*/
    def produceMWS(in : List[String] = Nil, dim: String) {
        val sourceDim = dim match {
          case "mws-flat" => "flat"
          case "mws-enriched" => "enriched"
          case _ => "content"
        }
        traverse(sourceDim, in, Archive.extensionIs("omdoc")) {case Current(inFile, inPath) =>
           val outFile = (root / "mws" / dim / inPath).setExtension("mws")
           log("[  -> MWS]  " + inFile + " -> " + outFile)
           val controller = new Controller
           controller.read(inFile,None)
           val mpath = Archive.ContentPathToMMTPath(inPath)
           val mod = controller.localLookup.getModule(mpath)
           mod match {
              case thy : DeclaredTheory =>
                 outFile.toJava.getParentFile().mkdirs()
                 val outStream = new java.io.FileWriter(outFile)
                 def writeEntry(t: Term, url: String) {
                   val node = <mws:expr url={url}>{t.toCML}</mws:expr> 
                   outStream.write(node.toString + "\n")
                 }
                 outStream.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
                 outStream.write("<mws:harvest xmlns:mws=\"http://search.mathweb.org/ns\" xmlns:m=\"http://www.w3.org/1998/Math/MathML\">\n")
                 thy.getDeclarations foreach {
                    case c: Constant =>
                       List(c.tp,c.df).map(tO => tO map { 
                          t =>
                            val url = custom.mwsurl(c.path) 
                            val cml = custom.prepareQuery(t)
                            val node = <mws:expr url={url}>{cml}</mws:expr> 
                            outStream.write(node.toString + "\n")
                            //writeEntry(t, mwsbase + "/" + c.name.flat)
                       })
                    /*case i : Instance => {
                      val url = mwsbase + "/" + thy.name + ".html" + "#" + i.name.flat
                      val node = <mws:expr url={url}>{i.matches.toCML}</mws:expr>
                      outStream.write(node.toString + "\n")
                    }*/
                    case _ =>
                 }
                 outStream.write("</mws:harvest>\n")
                 outStream.close
              case _ => //TODO index other modules
           }
        }
    }
}



object Archive {
   /** a string containing all characters that are illegal in file names */ 
   val illegalChars = "'"
   // TODO: (un)escape illegal characters
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
       case Nil => throw ImplementationError("")
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
       (schemeString + uri.authority.getOrElse("NONE")) :: uri.path ::: List(escape(m.name.flat) + ".omdoc")
    }
    /** returns a functions that filters by file name extension */
    def extensionIs(e: String) : String => Boolean = _.endsWith("." + e)  
}