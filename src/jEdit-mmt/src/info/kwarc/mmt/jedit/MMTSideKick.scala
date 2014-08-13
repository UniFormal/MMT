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
              case d: Delimiter => d.text
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
class ProverCompletion(view : org.gjt.sp.jedit.View, controller: Controller, region: SourceRegion, rules: List[ApplicableProvingRule])
  extends SideKickCompletion(view, "", rules.map(_.label)) {
  // this methods modifies the textArea after the user selected a completion
  override def insert(index: Int) {
     // the new subterm, result of applying the rule
     val newTerm = rules(index)()
     val newText = controller.presenter.asString(newTerm)
     // replace the old subterm with the new one
     val ta = view.getEditPane.getTextArea
     ta.setSelection(new Selection.Range(region.start.offset, region.end.offset))
     ta.setSelectedText(newText)
  } 
}


class MMTSideKick extends SideKickParser("mmt") with Logger {
   // gets jEdit's instance of MMTPlugin, jEdit will load the plugin if it is not loaded yet
   val mmt : MMTPlugin = jEdit.getPlugin("info.kwarc.mmt.jedit.MMTPlugin", true).asInstanceOf[MMTPlugin]
   val controller = mmt.controller
   val logPrefix = "jedit-sidekick"
   val report = controller.report
      
   private def getRegion(e: metadata.HasMetaData) : Option[SourceRegion] = SourceRef.get(e).map(_.region)
   /* build the sidekick outline tree: document node */
   private def buildTree(node: DefaultMutableTreeNode, doc: Document) {
      val reg = getRegion(doc) getOrElse SourceRegion(SourcePosition(0,0,0),SourcePosition(0,0,0))
      val child = new DefaultMutableTreeNode(new MMTElemAsset(doc, doc.path.toPath, reg))
      node.add(child)
      doc.getItems foreach {
        case d: DRef =>
           buildTree(child, controller.getDocument(d.target))
        case m: MRef =>
           buildTree(child, controller.localLookup.getModule(m.target), reg)
      }
   }
   
   private def moduleLabel(m: Module) = m match {
      case _ : Theory => "theory"
      case _: modules.View => "view"
   }
   
   /* build the sidekick outline tree: module node */
   private def buildTree(node: DefaultMutableTreeNode, mod: Module, defaultReg: SourceRegion) {
      val reg = getRegion(mod) getOrElse SourceRegion(defaultReg.start,defaultReg.start)
      val child = new DefaultMutableTreeNode(new MMTElemAsset(mod, moduleLabel(mod) + " " + mod.path.last, reg))
      node.add(child)
      mod match {
         case m: DeclaredModule =>
            m.getPrimitiveDeclarations foreach {d => buildTree(child, d, reg)}
         case m: DefinedModule =>
      }
   }
   /* build the sidekick outline tree: declaration (in a module) node */
   private def buildTree(node: DefaultMutableTreeNode, dec: Declaration, defaultReg: SourceRegion) {
      val reg = getRegion(dec) getOrElse SourceRegion(defaultReg.start,defaultReg.start)
      dec match {
         case nm: NestedModule =>
            buildTree(node, nm.module, reg)
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
      dec.getComponents foreach {
         case (comp, cont: AbstractTermContainer) if cont.get.isDefined =>
             buildTree(child, dec.path $ comp, cont.get.get, reg)
         case _ =>
      }
   }
   
   /** build the sidekick outline tree: component of a (module or symbol level) declaration */
   private def buildTree(node: DefaultMutableTreeNode, parent: CPath, t: Term, defaultReg: SourceRegion) {
      val reg = getRegion(t) getOrElse SourceRegion(defaultReg.start,defaultReg.start)
      val child = new DefaultMutableTreeNode(new MMTObjAsset(t, Context(), parent, parent.component.toString, reg))
      node.add(child)
      buildTree(child, parent, t, Context(), reg)
   }
   
   /** build the sidekick outline tree: context node (each VarDecl is added individually) */
   private def buildTree(node: DefaultMutableTreeNode, parent: CPath, con: Context, context: Context, defaultReg: SourceRegion) {
      con mapVarDecls {case (previous, vd @ VarDecl(n, tp, df, _)) =>
         val reg = getRegion(vd) getOrElse SourceRegion(defaultReg.start,defaultReg.start)
         val currentContext = context ++ previous
         val child = new DefaultMutableTreeNode(new MMTObjAsset(vd, currentContext, parent, n.toString, reg))
         node.add(child)
         (tp.toList:::df.toList) foreach {t =>
            buildTree(child, parent, t, currentContext, reg)
         }
      }
   }
   
   /** build the sidekick outline tree: (sub)term node */
   private def buildTree(node: DefaultMutableTreeNode, parent: CPath, t: Term, context: Context, defaultReg: SourceRegion) {
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
               buildTree(child, parent, binder, context, reg)
            buildTree(child, parent, cont, context, reg)
            scopes foreach {s =>
               buildTree(child, parent, s, context ++ cont, reg)
            }
         case OMA(fun, args) =>
            if (! fun.isInstanceOf[OMID])
               buildTree(child, parent, fun, context, reg)
            args.foreach(buildTree(child, parent, _, context, reg))            
         case _ => t.subobjects foreach {
            case (_, o: Term) => buildTree(child, parent, o, context, reg)
            case _ =>
         }
      }
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
      implicit val relCont = RelationHandler.ignore
      try {
         val doc = controller.read(buffer.getText, dpath)
         controller.checker(doc) 
         // add narrative structure of doc to outline tree
         buildTree(root, doc)
         // register errors with ErrorList plugin
      } catch {case e: java.lang.Throwable =>
         val msg = e.getClass + ": " + e.getMessage
         val pe = ParseError("unknown error: " + msg).setCausedBy(e)
         errorCont(pe)
         log(msg)
      }
      tree
   }
   // override def stop() 
   // override def getParseTriggers : String = ""

   override def supportsCompletion = true
   override def canCompleteAnywhere = true
   // override def getInstantCompletionTriggers : String = ""
   private val prover = new Prover(controller)
   override def complete(editPane: EditPane, caret : Int) : SideKickCompletion = {
      val textArea = editPane.getTextArea
      val view = editPane.getView
      val asset = MMTSideKick.getAssetAtOffset(view,caret).getOrElse(return null)
      asset match {
         case a: MMTObjAsset =>
            log(a.obj.toString)
            a.obj match {
               case Hole(t) =>
                  val rules = RuleBasedChecker.collectRules(controller, a.context) //TODO should be cached
                  val applRules = prover.applicable(t)(Stack(a.context), rules)
                  val comp = new ProverCompletion(view, controller, a.region, applRules)
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