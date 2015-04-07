package info.kwarc.mmt.jedit

import org.gjt.sp.jedit._
import textarea._

import errorlist._
import sidekick._

import info.kwarc.mmt.api._
import parser._
import checking._
import notations._
import archives.source
import frontend._
import libraries._
import modules._
import patterns._
import objects._
import symbols._
import documents._
import utils.File
import utils.MyList.fromList

import javax.swing.tree.DefaultMutableTreeNode
import scala.collection.JavaConversions.seqAsJavaList

case class MyPosition(offset : Int) extends javax.swing.text.Position {
   def getOffset = offset
}

/** node in the sidekick outline tree: common ancestor class
 * @param name the label of the asset
 * @param region the source region of the asset
 */ 
abstract class MMTAsset(name: String, val region: SourceRegion)
  extends enhanced.SourceAsset(name, region.start.line, MyPosition(region.start.offset)) {
  setEnd(MyPosition(region.end.offset+1))
  def getScope : Option[Term]
}

/** node in the sidekick outline tree: declarations
 * @param elem the node in the MMT syntax tree
 */ 
class MMTElemAsset(val elem : StructuralElement, name: String, reg: SourceRegion) extends MMTAsset(name, reg) {
   //note: shortDescription == name, shown in tree
   setLongDescription(path.toPath)  // tool tip
   //setIcon
   def path = elem.path
   def getScope : Option[objects.Term] = elem match {
      case _ : NarrativeElement => None
      case c : ContentElement => c match {
        case t: DeclaredTheory => Some(objects.OMMOD(t.path))
        case v: modules.View => None //would have to be parsed to be available
        case d: Declaration => Some(d.home)
        case _ => None
      }
   }
}

/** node in the sidekick outline tree: terms
 * @param term the node in the MMT syntax tree
 * @param parent the component containing the term
 * @param subobjectPosition the position in that term
 */ 
class MMTObjAsset(val obj: Obj, val context: Context, val parent: CPath, name: String, reg: SourceRegion) extends MMTAsset(name, reg) {
  obj.head map {case p =>
    setLongDescription(p.toPath)
  }
  def getTheory = parent.parent.module
  def getScope = Some(getTheory)
}

class MMTNotAsset(owner: ContentPath, label: String, not: TextNotation, reg: SourceRegion) extends MMTAsset(label, reg) {
   def getScope = Some(owner.module)
}

/**
 * @param view the current jEdit view
 * @param controller the current MMT controller
 * @param constants the MMT constants that are applicable here 
 * @param text the partial identifier that is completed
 * @param items the list of completion labels that is displayed
 */
class IDCompletion(view : org.gjt.sp.jedit.View, controller: Controller, constants: List[Constant], text: String, items: List[String])
  extends SideKickCompletion(view, text, items) {
  // this methods modifies the textArea after the user selected a completion
   override def insert(index: Int) {
     val con = constants(index)
     //the text to insert and after how many characters to place the caret
     val (newText,shift): (String,Int) = con.not match {
        case None =>
           val s = con.name.toPath + " "
           (s, s.length + 1)
        case Some(not) =>
           val text = not.parsingMarkers.map {
              case d: Delimiter => d.expand(con.path).text
              case w: WordMarker => " " + w.word + " "
              case SeqArg(_,sep,_) => " " + sep.text + " "
              case a:Arg => " "
              case _:ImplicitArg => ""
              case v: Var => " "
              case p: PresentationMarker => ""
              case AttributedObject => ""
           }.mkString("")
           val sh = not.parsingMarkers.head match {
              case d: Delimiter => d.text.length + 1
              case _ => 0
           }
           (text, sh)
     }
     //replace text with newText and move the caret by shift
     val ta = view.getEditPane.getTextArea
     val caret = ta.getCaretPosition
     ta.setSelection(new Selection.Range(caret-text.length, caret))
     ta.setSelectedText(newText)
     ta.setCaretPosition(caret-text.length+shift)
   }
}

/**
 * @param text the string that is to be completed
 * @param items the list of completions
 */
class ProverCompletion(view : org.gjt.sp.jedit.View, controller: Controller, region: SourceRegion, options: List[Term])
  extends SideKickCompletion(view, "", options.map(o => controller.presenter.asString(o))) {
  // this methods modifies the textArea after the user selected a completion
  override def insert(index: Int) {
     // the new subterm, result of applying the rule
     val newTerm = options(index)
     val newText = controller.presenter.asString(newTerm)
     // replace the old subterm with the new one
     // TODO decide whether to put brackets
     // TODO put cursor in front of first hole, reparse buffer
     val ta = view.getEditPane.getTextArea
     ta.setSelection(new Selection.Range(region.start.offset, region.end.offset+1))
     ta.setSelectedText(newText)
  }
}


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
                  val g = new proving.Goal(a.context, t)
                  val rules = RuleSet.collectRules(controller, a.context) //TODO should be cached
                  val prover = new proving.Prover(controller, g, rules, logPrefix)
                  log(g.present(2)(prover.presentObj, None,None))
                  val options = prover.interactive(3)
                  val comp = new ProverCompletion(view, controller, a.region, options)
                  return comp
               case _ =>
            }
         case _ => 
      }
      asset.getScope match {
        case Some(a) =>
           val p = textArea.getCaretPosition
           var l = 0 // number of character to the left of the caret that are id characters
           while (l < p && MMTPlugin.isIDChar(textArea.getText(p - l - 1,1)(0))) {l = l + 1}
           val partialName = textArea.getText(p - l, l)
           val compls = Names.resolve(a, Nil, partialName)(controller.localLookup)
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
      val dpath = controller.backend.resolvePhysical(path) match {
         case None => DPath(utils.FileURI(path))
         case Some((a,p)) => DPath(a.narrationBase / p)
      }
      log("parsing " + path)
      val tree = new SideKickParsedData(path.toJava.getName)
      val root = tree.root
      implicit val errorCont = new ErrorListForwarder(mmt.errorSource, controller, path)
      errorCont.reset
      val relCont = RelationHandler.ignore
      implicit val env = new CheckingEnvironment(errorCont, relCont)
      try {
         val doc = controller.read(buffer.getText, dpath, path.getExtension.getOrElse(""))
         controller.checker(doc) 
         // add narrative structure of doc to outline tree
         buildTreeDoc(root, doc)
         // register errors with ErrorList plugin
      } catch {case e: java.lang.Exception =>
         val msg = e.getClass + ": " + e.getMessage
         val pe = ParseError("unknown error: " + msg).setCausedBy(e)
         errorCont(pe)
         log(msg)
      }
      tree
   }

   private def getRegion(e: metadata.HasMetaData) : Option[SourceRegion] = SourceRef.get(e).map(_.region)
   /* build the sidekick outline tree: document node */
   private def buildTreeDoc(node: DefaultMutableTreeNode, doc: Document) {
      val reg = getRegion(doc) getOrElse SourceRegion(SourcePosition(0,0,0),SourcePosition(0,0,0))
      val child = new DefaultMutableTreeNode(new MMTElemAsset(doc, doc.path.toPath, reg))
      node.add(child)
      doc.getItems foreach {
        case d: DRef =>
           buildTreeDoc(child, controller.getDocument(d.target))
        case m: MRef =>
           buildTreeMod(child, controller.localLookup.getModule(m.target), Context(), reg)
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
      buildTreeComps(node, mod, context, reg)
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
         case (comp, cont: AbstractTermContainer) if cont.get.isDefined =>
            buildTreeComp(node, ce.path $ comp, cont.get.get, context, defaultReg)
         case (comp: NotationComponent, cont: NotationContainer) =>
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
   private def buildTreeNot(node: DefaultMutableTreeNode, owner: ContentPath, cont: NotationContainer, comp: NotationComponent, defaultReg: SourceRegion) {
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
         case OMSemiFormal(_) => "unparsed: " + tP.toString
         case ComplexTerm(op, _,_,_) => op.last.toString
         case _ => ""
      }
      val child = new DefaultMutableTreeNode(new MMTObjAsset(t, context, parent, label, reg))
      node.add(child)
      tP match {
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