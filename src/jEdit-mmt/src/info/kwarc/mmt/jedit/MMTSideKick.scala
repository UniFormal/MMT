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
      errorCont.reset
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
         ps.addListener(new LineInvalidator(buffer))
         // read the document
         val doc = controller.read(ps, true, true)(errorCont) match {
            case d: Document => d
            case _ => throw ImplementationError("document expected")
         }
         // add narrative structure of doc to outline tree
         val tree = new SideKickParsedData(path.toJava.getName)
         val root = tree.root
         buildTreeDoc(root, doc)
         tree
      } catch {
        case e: Throwable =>
          val msg = e.getClass + ": " + e.getMessage
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

   private def getRegion(e: metadata.HasMetaData) : Option[SourceRegion] = SourceRef.get(e).map(_.region)

   /* build the sidekick outline tree: document node */
   private def buildTreeDoc(node: DefaultMutableTreeNode, doc: Document) {
      val reg = getRegion(doc) getOrElse SourceRegion(SourcePosition(0,0,0),SourcePosition(0,0,0))
      val child = new DefaultMutableTreeNode(new MMTElemAsset(doc, doc.path.last, reg))
      node.add(child)
      doc.getDeclarations foreach {
        case d: Document =>
           buildTreeDoc(child, d)
        case r: NRef =>
           val rReg = getRegion(r) getOrElse reg
           val rChild = new DefaultMutableTreeNode(new MMTURIAsset(r.target, rReg))
           r match {
              case d: DRef =>
               child.add(rChild)
              case m: MRef =>
                 try {
                    val mod = controller.localLookup.getModule(m.target)
                    buildTreeMod(child, mod, Context.empty, reg)
                 } catch {case e: Error =>
                    // graceful degradation in case module could not be parsed
                    child.add(rChild)
                 }
              case s: SRef =>
           }
        case oe: opaque.OpaqueElement =>
      }
   }

   private def moduleLabel(m: Module) = m match {
      case _ : Theory => "theory"
      case _: modules.View => "view"
   }

   /* build the sidekick outline tree: module node */
   private def buildTreeMod(node: DefaultMutableTreeNode, mod: Module, context: Context, defaultReg: SourceRegion) {
      val reg = getRegion(mod) getOrElse SourceRegion(defaultReg.start,defaultReg.start)
      val child = new DefaultMutableTreeNode(new MMTElemAsset(mod, moduleLabel(mod) + " " + mod.path.last, reg))
      node.add(child)
      buildTreeComps(child, mod, context, reg)
      mod match {
         case m: DeclaredModule =>
           val defElab = m.getDeclarations.filter(_.getOrigin == ElaborationOfDefinition)
           if (defElab.nonEmpty) {
              val elabChild = new DefaultMutableTreeNode(new MMTAuxAsset("-- flat definition --"))
              child.add(elabChild)
              defElab foreach {d => buildTreeDecl(elabChild, m, d, context ++ m.getInnerContext, reg)}
           }
           val incls = m.getDeclarations.filter {d => Include.unapply(d).isDefined && d.isGenerated}
           if (incls.nonEmpty) {
              val inclsChild = new DefaultMutableTreeNode(new MMTAuxAsset("-- flat includes --"))
              child.add(inclsChild)
              incls foreach {d => buildTreeDecl(inclsChild, m, d, context ++ m.getInnerContext, reg)}
           }
           m.getPrimitiveDeclarations foreach {d => buildTreeDecl(child, m, d, context ++ m.getInnerContext, reg)}
         case m: DefinedModule =>
      }
   }
   /** build the sidekick outline tree: declaration (in a module) node */
   private def buildTreeDecl(node: DefaultMutableTreeNode, parent: ContainerElement[_ <: Declaration], dec: Declaration, context: Context, defaultReg: SourceRegion) {
      val reg = getRegion(dec) getOrElse SourceRegion(defaultReg.start,defaultReg.start)
      dec match {
         case nm: NestedModule if !nm.isInstanceOf[DerivedDeclaration] =>
            buildTreeMod(node, nm.module, context, reg)
            return
         case _ =>
      }
      val label = dec match {
         case Include(_, from,_) => "include " + from.last
         case LinkInclude(_,_,OMMOD(incl)) => "include " + incl.last
         case r: RuleConstant => r.feature
         case d => d.feature + " " + d.name.toStr(true)
      }
      val child = new DefaultMutableTreeNode(new MMTElemAsset(dec, label, reg))
      node.add(child)
      buildTreeComps(child, dec, context, reg)
      dec match {
        case ce: ContainerElement[Declaration]@unchecked => // declarations can only contain declarations
          val contextInner = context ++ controller.getExtraInnerContext(ce)
          dec.getDeclarations foreach {d => buildTreeDecl(child, ce, d, contextInner, reg)}
        case _ =>
      }
      // a child with all declarations elaborated from dec
      val elab = parent.getDeclarations.filter(_.getOrigin == ElaborationOf(dec.path))
      if (elab.nonEmpty) {
        val elabChild = new DefaultMutableTreeNode(new MMTAuxAsset("-- elaboration --"))
        child.add(elabChild)
        elab foreach {e => buildTreeDecl(elabChild, parent, e, context, defaultReg)}
      }
   }

   /** add child nodes for all components of an element */
   private def buildTreeComps(node: DefaultMutableTreeNode, ce: ContentElement, context: Context, defaultReg: SourceRegion) {
      ce.getComponents foreach {
         case DeclarationComponent(comp, cont: AbstractTermContainer) if cont.get.isDefined =>
            buildTreeComp(node, ce.path $ comp, cont.get.get, context, defaultReg)
         case NotationComponent(comp, cont) =>
            buildTreeNot(node, ce.path, cont, comp, defaultReg)
         case _ =>
      }
   }

   /** build the sidekick outline tree: component of a (module or symbol level) declaration */
   private def buildTreeComp(node: DefaultMutableTreeNode, parent: CPath, t: Term, context: Context, defaultReg: SourceRegion) {
      val reg = getRegion(t) getOrElse SourceRegion.none
      val child = new DefaultMutableTreeNode(new MMTObjAsset(mmt, t, t, context, parent, parent.component.toString, reg))
      node.add(child)
      buildTreeTerm(child, parent, t, context, defaultReg)
   }

   /** build the sidekick outline tree: notations */
   private def buildTreeNot(node: DefaultMutableTreeNode, owner: ContentPath, cont: NotationContainer, comp: NotationComponentKey, defaultReg: SourceRegion) {
      val tn = cont(comp).get // always defined here
      val reg = getRegion(tn) getOrElse SourceRegion(defaultReg.start,defaultReg.start)
      val label = comp match {
         case ParsingNotationComponent => "notation (parsing)"
         case PresentationNotationComponent => "notation (presentation)"
         case VerbalizationNotationComponent => "notation (verbalization)"
      }
      val child = new DefaultMutableTreeNode(new MMTNotAsset(owner, label, tn, reg))
      node.add(child)
   }

   /** build the sidekick outline tree: context node (each VarDecl is added individually) */
   private def buildTreeCont(node: DefaultMutableTreeNode, parent: CPath, con: Context, context: Context, defaultReg: SourceRegion) {
      con mapVarDecls {case (previous, vd @ VarDecl(n, f, tp, df, _)) =>
         val reg = getRegion(vd) getOrElse SourceRegion(defaultReg.start,defaultReg.start)
         val currentContext = context ++ previous
         val child = new DefaultMutableTreeNode(new MMTObjAsset(mmt, vd, vd, currentContext, parent, f.map(_+" ").getOrElse("") + n.toString, reg))
         node.add(child)
         (tp.toList:::df.toList) foreach {t =>
            buildTreeTerm(child, parent, t, currentContext, reg)
         }
      }
   }

   /** build the sidekick outline tree: (sub)term node */
   private def buildTreeTerm(node: DefaultMutableTreeNode, parent: CPath, t: Term, context: Context, _unused_defaultReg: SourceRegion) {
      var extraLabel = ""
      val reg = getRegion(t) getOrElse {
        extraLabel = " [not in source]" // lack of source region indicates inferred subterms
        SourceRegion.none
      }
      val tP = controller.pragmatic.mostPragmatic(t)
      val label = tP match {
         case OMV(n) => n.toString
         case OMID(p) => p.name.toString
         case l: OMLITTrait => l.toString
         case OML(nm,_,_,_,_) => nm.toString
         case OMSemiFormal(_) => "unparsed: " + tP.toString
         case ComplexTerm(op, _,_,_) => op.name.last.toStr(true)
         case OMA(OMID(p),_) => p.name.last.toStr(true)
         case OMBINDC(OMID(p),_,_) => p.name.last.toStr(true)
         case OMA(ct,args) => "OMA" // TODO probably shouldn't occur, but throws errors!
      }
      val asset = new MMTObjAsset(mmt, t, tP, context, parent, label+extraLabel, reg)
      val child = new DefaultMutableTreeNode(asset)
      node.add(child)
      tP match {
         case OML(_,tp,df,_,_) =>
            (tp.toList:::df.toList) foreach {t =>
               buildTreeTerm(child, parent, t, context, reg)
            }
         case OMBINDC(binder,cont, scopes) =>
            if (! binder.isInstanceOf[OMID])
               buildTreeTerm(child, parent, binder, context, reg)
            buildTreeCont(child, parent, cont, context, reg)
            scopes foreach {s =>
               buildTreeTerm(child, parent, s, context ++ cont, reg)
            }
         case OMA(fun, args) =>
            if (! fun.isInstanceOf[OMID])
               buildTreeTerm(child, parent, fun, context, reg)
            args.foreach(buildTreeTerm(child, parent, _, context, reg))
         case _ => t.subobjects foreach {
            case (_, o: Term) => buildTreeTerm(child, parent, o, context, reg)
            case _ =>
         }
      }
   }
}

object MMTSideKick {
  /** @return the asset at the specified position */
  def getAssetAtOffset(view: jedit.View, offset: Int): Option[MMTAsset] = {
      val pd = SideKickParsedData.getParsedData(view)
      pd.getAssetAtOffset(offset) match {
         case ma : MMTAsset => Some(ma)
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
            case m: MMTAsset =>
               asset = Some(m)
               m.getEnd.getOffset+1 < end
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