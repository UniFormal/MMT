package info.kwarc.mmt.jedit

import javax.swing.tree.DefaultMutableTreeNode
import org.gjt.sp.jedit
import jedit._
import sidekick._
import errorlist._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.gui._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.parser._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils._

class MMTSideKick extends SideKickParser("mmt") with Logger {
   // gets jEdit's instance of MMTPlugin, jEdit will load the plugin if it is not loaded yet
   val mmt : MMTPlugin = jEdit.getPlugin("info.kwarc.mmt.jedit.MMTPlugin", true).asInstanceOf[MMTPlugin]
   val controller = mmt.controller
   val logPrefix = "jedit-sidekick"
   val report = controller.report

   // override def stop()
   // override def getParseTriggers : String = ""

   override def supportsCompletion = true
   override def canCompleteAnywhere = true
   // override def getInstantCompletionTriggers : String = ""
   override def complete(editPane: EditPane, caret : Int) : SideKickCompletion = {
      val textArea = editPane.getTextArea
      val view = editPane.getView
      val asset = MMTSideKick.getAssetAtOffset(view,caret).getOrElse(return null)
      asset match {
         case a: MMTObjAsset =>
            a.obj match {
               case Hole(t) =>
                  // run theorem prover to find options to fill the hole
                  val g = new proving.Goal(a.context, t)
                  val rules = RuleSet.collectRules(controller, a.context) //TODO should be cached
                  val pu = proving.ProvingUnit(None, g.context, g.conc, logPrefix)
                  log("trying to complete for " + pu.toString)
                  val prover = new proving.Searcher(controller, g, rules, pu)
                  log(g.present(2)(prover.presentObj, None,None))
                  val options = prover.interactive(3)
                  log("completion options:" + options.toString)
                  val comp = new ProverCompletion(view, controller, a.region, options)
                  return comp
               case o =>
                 // insert hole for the definiens of a constant whose type is known
                 if (a.parent.component == DefComponent) {
                   controller.globalLookup.getComponent(a.parent) match {
                     case defTc: TermContainer if defTc.read.getOrElse("").trim.isEmpty =>
                       controller.globalLookup.getComponent(a.parent.parent $ TypeComponent) match {
                         case tpTc: TermContainer if tpTc.analyzed.isDefined =>
                           val option = Hole(tpTc.analyzed.get)
                           return new ProverCompletion(view, controller, a.region, List(option))
                         case _ =>
                       }
                     case _ =>
                   }
                 }
            }
         case _ =>
      }
      asset.getScope match {
        case Some(a) =>
           val p = textArea.getCaretPosition
           var l = 0 // number of character to the left of the caret that are id characters
           while (l < p && MMTPlugin.isIDChar(textArea.getText(p - l - 1,1)(0))) {l = l + 1}
           val partialName = textArea.getText(p - l, l)
           val compls = Names.resolve(OMMOD(a), Nil, partialName)(controller.localLookup)
           val paths = compls.map(_.path)
           val symbols = paths.flatMap {p =>
              controller.globalLookup.getO(p) match {
                 case Some(c: Constant) => List(c)
                 case _ => Nil
              }
           }
           val displayed = symbols map {c =>
              c.tp match {
                 case Some(t) => c.name.toPath + " : " + controller.presenter.asString(t)
                 case None => c.name.toPath
              }
           }
           return new IDCompletion(view, controller, symbols, partialName, displayed)
        case None =>
      }
      new IDCompletion(view, controller, Nil, "", Nil)
   }

   def parse(buffer: Buffer, errorSource: DefaultErrorSource) : SideKickParsedData = {
      val path = File(buffer.getPath)
      val errorCont = new ErrorListForwarder(mmt.errorSource, controller, path)
      //TODO there is a synchronization problem with multiple parse calls causing a concurrent access exception in ErrorList
      mmt.errorSource.removeMMTFileErrors(path)
      try {
         val uri = utils.FileURI(path)
         val text = buffer.getText
         val nsMap = controller.getNamespaceMap
         val ps = controller.backend.resolvePhysical(path) orElse controller.backend.resolveAnyPhysicalAndLoad(path) match {
            case None =>
               ParsingStream.fromString(text, DPath(uri), path.getExtension.getOrElse(""), Some(nsMap))
            case Some((a, p)) =>
               ParsingStream.fromSourceFile(a, FilePath(p), Some(ParsingStream.stringToReader(text)), Some(nsMap))
         }
         log("parsing " + path)
         // store the task for cancellation and progress reports
         mmt.progressTracker(buffer) = ps
         //ps.addListener(new LineInvalidator(buffer))   // This causes a mess in jEdit; repainting of the gutter markers must be achieved in some other way.
         // read the document
         val doc = controller.read(ps, true, true)(errorCont) match {
            case d: Document => d
            case _ => throw ImplementationError("document expected")
         }
         // add narrative structure of doc to outline tree
         val tree = new SideKickParsedData(path.toJava.getName)
         val root = tree.root
         TreeBuilder.buildTreeDoc(root, doc)
         tree
      } catch {
        case e: Exception =>
          val msg = e.getClass.toString + ": " + e.getMessage
          val pe = ParseError("unknown error: " + msg).setCausedBy(e)
          log(msg)
          try {errorCont(pe)} catch {case e: Exception => log(Error(e).toStringLong)}
          SideKickParsedData.getParsedData(jEdit.getActiveView)
      } finally {
         // we only track progress during checking (simpler and prevents memory leaks)
         mmt.progressTracker -= buffer
      }
   }

   /** this is called from another tread if the cancel button was pressed; but it seems sidekick force-kills the parsing before calling this
    *  so it doesn't help us much; MMT has to maintain its own smart cancel method instead */
   //override def stop {}
    object TreeBuilder extends NavigationTreeBuilder(controller) {
     override def makeDocument(doc: Document, region: SourceRegion) = new JElemAsset(doc, doc.path.last, region)
     override def makeNRef(uri: Path, region: SourceRegion) = new JURIAsset(uri, region)
     override def makeModule(mod: Module, region: SourceRegion) = new JElemAsset(mod, moduleLabel(mod), region)
     override def makeSection(s: String) = new JAuxAsset(s)
     override def makeDeclaration(dec: Declaration, region: SourceRegion) = new JElemAsset(dec, declarationLabel(dec), region)
     override def makeComponent(t: Term, cont: Context, parent: CPath, region: SourceRegion)
     = new JObjAsset(mmt, t, t, cont, parent, parent.component.toString, region)
     override def makeTerm(t : Term, pragmatic : Term, cont : Context, parent : CPath, label : String, region : SourceRegion)
      = new JObjAsset(mmt, t, pragmatic, cont, parent, label, region)
     override def makeNotation(owner: ContentPath, cont: NotationContainer, comp: NotationComponentKey, region: SourceRegion)
      = new JNotAsset(owner, notationLabel(comp), cont(comp).get, region)
     override def makeVariableInContext(con: Context, vd: VarDecl, parent: CPath, region: SourceRegion)
      = new JObjAsset(mmt, vd, vd, con, parent, vd.feature.map(_+" ").getOrElse("") + vd.name.toString, region)
   }


}

object MMTSideKick {
  /** @return the asset at the specified position */
  def getAssetAtOffset(view: jedit.View, offset: Int): Option[JAsset] = {
      val pd = SideKickParsedData.getParsedData(view)
      pd.getAssetAtOffset(offset) match {
         case ma : JAsset => Some(ma)
         case _ => None
      }
  }

  /** @return the smallest asset covering the specified range (inclusive begin, exclusive end) */
  def getAssetAtRange(view: jedit.View, begin: Int, end: Int): Option[MMTAsset] = {
      val pd = SideKickParsedData.getParsedData(view)
      var path = pd.getTreePathForPosition(begin)
      var asset: Option[MMTAsset] = None
      while ({
         val a = path.getLastPathComponent.asInstanceOf[DefaultMutableTreeNode].getUserObject
         a match {
            case m: JAsset =>
               asset = Some(m)
               m.getEnd.getOffset < end
            case _ => false
         }
      }) {
         path = path.getParentPath
      }
      asset
  }
  
  /**
   * return the smallest asset covering the first selection or (if no selection) the asset at the the cursor; boolean is true if the former case
   * this is useful for button or key interaction, where a selection or cursor position must be used to determine an asset  
   */
  def getCurrentAsset(view: jedit.View): Option[(MMTAsset,Boolean)] = {
    val ta = view.getTextArea
    ta.getSelectionCount match {
      case 0 =>
        getAssetAtOffset(view, ta.getCaretPosition) map {x => (x,false)}
      case _ =>
        val sel = ta.getSelection(0)
        getAssetAtRange(view, sel.getStart, sel.getEnd) map {x => (x,true)}
    }
  }

  /**
   * like getAssetAtOffset, but includes the surrounding selection, if any; the boolean if true if the area is selected
   * this is useful for mouse interaction, where an offset can be computed from the mouse pointer position
   */
  def getSelectedAssetAtOffset(view: jedit.View, offset: Int): Option[(MMTAsset,Boolean)] = {
     val rangeOpt = getSelectedRangeAroundOffset(view.getTextArea, offset)
     rangeOpt match {
       case Some((b,e)) => getAssetAtRange(view, b, e) map {x => (x,true)}
       case None => getAssetAtOffset(view, offset) map {x => (x,false)}
     }
  }
  /** the range of the selection at a position (selection inclusive begin, exclusive end) */
  def getSelectedRangeAroundOffset(ta: textarea.TextArea, offset: Int): Option[(Int,Int)] = {
    if (ta.getSelectionCount != 1) return None
    val sel = ta.getSelection(0)
    if (sel.getStart <= offset && offset < sel.getEnd)
       Some((sel.getStart, sel.getEnd))
    else
       None
  }
}

/** used by sidekick to mark gutter lines for repainting */
class LineInvalidator(buffer: Buffer) extends MMTTaskProgressListener {
  def apply(r: MMTTaskProgress) = r match {
    case r: MMTInterpretationProgress =>
      r.sourceLine foreach {l =>
         jEdit.getViews foreach {v =>
           v.getEditPanes foreach {e =>
             if (e.getBuffer == buffer) {
               e.getTextArea.invalidateLine(l)
             }
           }
         }
      }
    case _ =>
  }
}