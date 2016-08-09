package info.kwarc.mmt.jedit

import javax.swing.tree.DefaultMutableTreeNode

import scala.util.{Try,Success,Failure}

import org.gjt.sp.jedit._
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
                  val prover = new proving.Searcher(controller, g, rules, logPrefix)
                  log(g.present(2)(prover.presentObj, None,None))
                  val options = prover.interactive(3)
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
         val tree = new SideKickParsedData(path.toJava.getName)
         val root = tree.root
         // read the document in a task that can be cancelled by the stop method
         val task = new CancellableTask(controller.read(ps, true, true)(errorCont) match {
            case d: Document => d
            case _ => throw ImplementationError("document expected")
         })
         currentTask = Some(task)
         // inspect the result of reading, fall back to loading whatever document is in memory
         val doc = task.result match {
           case Success(d) => d
           case Failure(TaskCancelled) =>
             val expectedPath = ps.parentInfo match {
               case IsRootDoc(dp) => dp
               case _ => throw ImplementationError("non document path")
             }
             val dOpt = controller.localLookup.getO(expectedPath)
             controller.clear // try to restore consistent state
             dOpt match {
               case Some(d: Document) =>
                 errorCont(new ParseError("reading was interrupted, returning partial result"))
                 d
               case _ => throw ImplementationError("non document")
             }
           case Failure(e) => throw e
         }
         // add narrative structure of doc to outline tree
         buildTreeDoc(root, doc)
         tree
      } catch {case e: Exception =>
         val msg = e.getClass + ": " + e.getMessage
         val pe = ParseError("unknown error: " + msg).setCausedBy(e)
         log(msg)
         errorCont(pe)
         SideKickParsedData.getParsedData(jEdit.getActiveView)
      }
   }

   /** stores the current parse task so that it can be stopped by the stop methpd */
   private var currentTask: Option[CancellableTask[Document]] = None
   // this is called from another tread and should interrupt parsing
   override def stop {
      // this may cause an inconsistent state but calling clear in parse method should fix most problems  
      currentTask.foreach {_.cancel}
   }

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
            m.getPrimitiveDeclarations foreach {d => buildTreeDecl(child, d, context ++ m.getInnerContext, reg)}
         case m: DefinedModule =>
      }
   }
   /** build the sidekick outline tree: declaration (in a module) node */
   private def buildTreeDecl(node: DefaultMutableTreeNode, dec: Declaration, context: Context, defaultReg: SourceRegion) {
      val reg = getRegion(dec) getOrElse SourceRegion(defaultReg.start,defaultReg.start)
      dec match {
         case nm: NestedModule =>
            buildTreeMod(node, nm.module, context, reg)
            return
         case _ =>
      }
      val label = dec match {
         case PlainInclude(from,_) => "include " + from.last
         case PlainViewInclude(_,_,incl) => "include " + incl.last
         case s: Structure => "structure " + s.name.toString
         case c: Constant => c.name.toString
         case d => d.name.toString
      }
      val child = new DefaultMutableTreeNode(new MMTElemAsset(dec, label, reg))
      node.add(child)
      buildTreeComps(child, dec, context, reg)
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
      val reg = getRegion(t) getOrElse SourceRegion(defaultReg.start,defaultReg.start)
      val child = new DefaultMutableTreeNode(new MMTObjAsset(t, context, parent, parent.component.toString, reg))
      node.add(child)
      buildTreeTerm(child, parent, t, context, reg)
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
      val child = new DefaultMutableTreeNode(new MMTNotAsset(owner, label + ": " + tn.toString, tn, reg))
      node.add(child)
   }

   /** build the sidekick outline tree: context node (each VarDecl is added individually) */
   private def buildTreeCont(node: DefaultMutableTreeNode, parent: CPath, con: Context, context: Context, defaultReg: SourceRegion) {
      con mapVarDecls {case (previous, vd @ VarDecl(n, tp, df, _)) =>
         val reg = getRegion(vd) getOrElse SourceRegion(defaultReg.start,defaultReg.start)
         val currentContext = context ++ previous
         val child = new DefaultMutableTreeNode(new MMTObjAsset(vd, currentContext, parent, n.toString, reg))
         node.add(child)
         (tp.toList:::df.toList) foreach {t =>
            buildTreeTerm(child, parent, t, currentContext, reg)
         }
      }
   }

   /** build the sidekick outline tree: (sub)term node */
   private def buildTreeTerm(node: DefaultMutableTreeNode, parent: CPath, t: Term, context: Context, defaultReg: SourceRegion) {
      val reg = getRegion(t) getOrElse SourceRegion(defaultReg.start,defaultReg.start)
      val tP = controller.pragmatic.mostPragmatic(t)
      val label = tP match {
         case OMV(n) => n.toString
         case OMID(p) => p.name.toString
         case l: OMLITTrait => l.toString
         case OML(VarDecl(nm,_,_,_)) => nm.toString
         case OMSemiFormal(_) => "unparsed: " + tP.toString
         case ComplexTerm(op, _,_,_) => op.last.toString
         case _ => ""
      }
      val child = new DefaultMutableTreeNode(new MMTObjAsset(t, context, parent, label, reg))
      node.add(child)
      tP match {
         case OML(VarDecl(_,tp,df,_)) =>
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
   def getAssetAtOffset(view: org.gjt.sp.jedit.View, caret: Int) = {
      val pd = SideKickParsedData.getParsedData(view)
      pd.getAssetAtOffset(caret) match {
         case ma : MMTAsset => Some(ma)
         case _ => None
      }
   }
   /** @return the smallest asset covering the specified range */
   def getAssetAtRange(view: org.gjt.sp.jedit.View, begin: Int, end: Int) = {
      val pd = SideKickParsedData.getParsedData(view)
      var path = pd.getTreePathForPosition(begin)
      var asset: MMTAsset = null
      while ({
         val a = path.getLastPathComponent.asInstanceOf[DefaultMutableTreeNode].getUserObject
         a match {
            case m: MMTAsset =>
               asset = m
               m.getEnd.getOffset < end
            case _ => false
         }
      }) {
         path = path.getParentPath
      }
      asset
   }
}
