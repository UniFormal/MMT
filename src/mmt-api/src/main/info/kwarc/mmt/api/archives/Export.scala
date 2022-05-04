package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import documents._
import frontend._
import Level.Level
import modules._
import objects._
import symbols._
import presentation._
import utils._

trait Exporter extends BuildTarget {self =>
  /** must be set by deriving classes to direct output, not necessary if outputTo is used */
  protected var _rh: RenderingHandler = null
  /**
    * sends output to a certain file, file is created new and deleted if empty afterwards
    *
    * @param out the output file to be used while executing body
    * @param body any code that produces output
    */
  protected def outputTo(out: File)(body: => Unit) {
    val fw = new presentation.FileWriter(out)
    _rh = fw
    body
    fw.done
    if (out.toJava.length == 0)
      out.toJava.delete
  }
  /** gives access to the RenderingHandler for sending output */
  protected def rh = _rh

  override def init(controller: Controller) {
    this.controller = controller
    report = controller.report
    contentExporter.init(controller)
    narrationExporter.init(controller)
  }

  override def start(args: List[String]) {
    controller.extman.addExtension(contentExporter, args)
    controller.extman.addExtension(narrationExporter, args)
  }

  /** override this method if your exporter cannot export arbitrary content
   *  @param p the MMT URI of a content element to be exported
   */
  def canHandle(p: Path) = true

  /** applied to each document (i.e., narration-folders and .omdoc files) */
  def exportDocument(doc: Document, bf: BuildTask): Unit

  /** applied to each theory */
  def exportTheory(thy: Theory, bf: BuildTask): Unit

  /** applied to each view */
  def exportView(view: View, bf: BuildTask): Unit

  /** applied to each derived module, does nothing by default */
  def exportDerivedModule(dm: DerivedModule, bf: BuildTask) = {}
  
  /** applied to every namespace
 *
    * @param dpath the namespace
    * @param namespaces the sub-namespace in this namespace
    * @param modules the modules in this namespace
    */
  def exportNamespace(dpath: DPath, bd: BuildTask, namespaces: List[BuildTask], modules: List[BuildTask]): Unit

  def build(a: Archive, w: Build, in: FilePath, errorCont: Option[ErrorHandler]) {
    narrationExporter.build(a, w, in, errorCont)
    // find all modules in documents at path 'in'
    val doc = controller.getAs(classOf[Document], DPath(a.narrationBase / in))
    val mods = doc.getModules(controller.globalLookup)
    mods.foreach {p =>
      val modPath = Archive.MMTPathToContentPath(p)
      contentExporter.build(a, w, modPath, errorCont)
    }
  }

  def clean(a: Archive, in: FilePath) {
    contentExporter.clean(a, in)
    narrationExporter.clean(a, in)
  }

  def producesFrom(out: FilePath) = contentExporter.producesFrom(out) orElse narrationExporter.producesFrom(out)

  /** the file name for files representing folders, defaults to "", override as needed */
  protected def folderName = ""

  /** the file extension used for generated files, defaults to key, override as needed */
  protected def outExt = key
  
  /** the dimension for storing generated files, defaults to export/key, override as needed */
  protected def outDim = Dim("export", key)

  /** returns the output file that this exporter uses for some module */
  protected def getOutFileForModule(p: MPath): Option[File] = {
    controller.backend.findOwningArchive(p).map {arch =>
      (arch / contentExporter.outDim / archives.Archive.MMTPathToContentPath(p.mainModule)).setExtension(outExt)
    }
  }
  
  /** the common properties of the content and the narration exporter */
  private trait ExportInfo extends TraversingBuildTarget {
    def key = self.key + "_" + inDim.toString

    def includeFile(name: String) = name.endsWith(".omdoc") || name.endsWith(".omdoc.xz")

    def outDim: Dim = self.outDim / inDim.toString

    override def outExt = self.outExt
    
    override protected def getOutFile(a: Archive, inPath: FilePath) = {
      // if we end in .omdoc.xz, we have to strip one more extension than usual
      val inPathNoXz = if (inPath.name.endsWith(".xz"))
        inPath.stripExtension
      else
        inPath
      super.getOutFile(a, inPathNoXz)
    }
    
    
    override protected val folderName = self.folderName

    override def parallel = false

  }

  /**
    * A BuildTarget that traverses the content dimension and applies continuation functions to each module.
    */
  private lazy val contentExporter = new TraversingBuildTarget with ExportInfo {
    val inDim = content

    def buildFile(bf: BuildTask): BuildResult = {
      val mp = Archive.ContentPathToMMTPath(bf.inPath)
      val mod = controller.globalLookup.getModule(mp)
      outputTo(bf.outFile) {
        mod match {
          case t: Theory =>
            exportTheory(t, bf)
          case v: View =>
            exportView(v, bf)
          case dm: DerivedModule =>
            exportDerivedModule(dm, bf)
        }
      }
      BuildResult.empty
    }

    override def buildDir(bd: BuildTask, builtChildren: List[BuildTask]): BuildResult = {
      val dp = Archive.ContentPathToDPath(bd.inPath)
      val (nss, mps) = builtChildren.filter(!_.skipped).partition(_.isDir)
      outputTo(bd.outFile) {
        exportNamespace(dp, bd, nss, mps)
      }
      BuildResult.empty
    }
  }

  /**
    * A BuildTarget that traverses the content dimension and applies continuation functions to each module.
    */
  private lazy val narrationExporter = new TraversingBuildTarget with ExportInfo {
    val inDim = narration

    def buildFile(bf: BuildTask) = {
      val doc = controller.getDocument(bf.narrationDPath)
      outputTo(bf.outFile) {
        exportDocument(doc, bf)
      }
      BuildResult.empty
    }

    override def buildDir(bd: BuildTask, builtChildren: List[BuildTask]): BuildResult = {
      val doc = controller.getDocument(bd.narrationDPath)
      outputTo(bd.outFile) {
        exportDocument(doc, bd)
      }
      BuildResult.empty
    }
  }
  /* object asPresenter
   * Such an object is not needed here.
   * Instead any Exporter that makes sense to use as a Presenter should be implemented as a (Structure)Presenter to begin with
   */
}

/** An Exporter that exports relative to a bifoundation
  * @param meta the syntactic meta-theory, e.g., the logic or specification language
  * @param found the semantic domain, e.g., the foundation or programming language
  */
abstract class FoundedExporter(meta: MPath, found: MPath) extends Exporter {
  protected def covered(m: MPath): Boolean = {
    objects.TheoryExp.metas(OMMOD(m))(controller.globalLookup).exists {
      mt =>
        val vis = controller.library.visibleDirect(OMMOD(mt))
        vis contains OMMOD(meta)
    }
  }

  def exportTheory(t: Theory, bf: BuildTask) {
    if (covered(t.path))
      exportCoveredTheory(t)
    else
      bf.skipped = true
  }

  def exportView(v: View, bf: BuildTask) {
    if (covered(v.from.toMPath)) {
      val to = v.to.toMPath
      if (to == found)
        exportRealization(v) //TODO check if v includes certain fixed morphism
      else if (covered(to))
        exportFunctor(v)
      else
        bf.skipped = true
    } else
      bf.skipped = true
  }

  /** called on covered theories, i.e., theories with meta-theory meta */
  def exportCoveredTheory(t: Theory)

  /** called on views between covered theories */
  def exportFunctor(v: View)

  /** called on realizations, i.e., views from a covered theory to found, e.g., models or implementations */
  def exportRealization(r: View)

}
