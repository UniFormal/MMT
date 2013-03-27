package info.kwarc.mmt.jedit

import org.gjt.sp.jedit._
import textarea._

import errorlist._
import sidekick._

import info.kwarc.mmt.api._
import parser._
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
  setEnd(MyPosition(region.end.offset))
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
      case _ : PresentationElement => None
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
  def getTheory = parent.parent match {
     case p: MPath => OMMOD(p)
     case GlobalName(t, _) => t
  }
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
           val text = not.markers.flatMap {
              case d: Delimiter => d.text
              case SeqArg(_,sep) => " " + sep.text + " "
              case a:Arg => " "
              case v: Var => " "
           }.mkString("")
           val sh = not.markers.head match {
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
   val logPrefix = "jedit-parse"
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
   /* build the sidekick outline tree: module node */
   private def buildTree(node: DefaultMutableTreeNode, mod: Module, defaultReg: SourceRegion) {
      val keyword = mod match {case _ : Theory => "theory"; case _: modules.View => "view"}
      val reg = getRegion(mod) getOrElse SourceRegion(defaultReg.start,defaultReg.start)
      val child = new DefaultMutableTreeNode(new MMTElemAsset(mod, keyword + " " + mod.path.last, reg))
      node.add(child)
      mod match {
         case m: DeclaredModule[_] =>
            m.getPrimitiveDeclarations foreach {d => buildTree(child, d, reg)}
         case m: DefinedModule =>
      }
   }
   /* build the sidekick outline tree: declaration (in a module) node */
   private def buildTree(node: DefaultMutableTreeNode, dec: Declaration, defaultReg: SourceRegion) {
      val label = dec match {
         case PlainInclude(from,_) => "include " + from.last
         case s: Structure => "structure " + s.name.toString
         case a: DefLinkAssignment => "include " + a.name.toString
         case d: Declaration => d.role.toString + " " + d.name.toString
      }
      val reg = getRegion(dec) getOrElse SourceRegion(defaultReg.start,defaultReg.start)
      val child = new DefaultMutableTreeNode(new MMTElemAsset(dec, label, reg))
      node.add(child)
      dec match {
         //TODO: should be done with a generic function that returns the list of components
         case c: Constant =>
             c.tp foreach {t => buildTree(child, dec.path $ TypeComponent, t, reg)}
             c.df foreach {t => buildTree(child, dec.path $ DefComponent, t, reg)}
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
      con mapVarDecls {case (previous, vd @ VarDecl(n, tp, df)) =>
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
      val tp = controller.pragmatic.pragmaticHead(t)
      val label = tp match {
         case OMV(n) => n.toString
         case OMS(p) => p.name.toString
         case OMSemiFormal(_) => "unparsed"
         case _ => tp.head.map(_.last.toString).getOrElse(t.role.toString)
      }
      val child = new DefaultMutableTreeNode(new MMTObjAsset(t, context, parent, label, reg))
      node.add(child)
      tp match {
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
         case _ => t.components foreach {
            case o: Term => buildTree(child, parent, o, context, reg)
            case _ =>
         }
      }
   }
   
   def parse(buffer: Buffer, errorSource: DefaultErrorSource) : SideKickParsedData = {
      val path = File(buffer.getPath)
      val tree = new SideKickParsedData(path.toJava.getName)
      val root = tree.root
      try {
         val (doc,errors) = controller.read(path, None)
         val checker = controller.checker //new StructureChecker(controller) 
         val errors2 = checker(doc)
         // add narrative structure of doc to outline tree
         buildTree(root, doc)
         // register errors with ErrorList plugin
         (errors ::: errors2) foreach {
            case s: SourceError =>
               //generated by StructureParser or TextReader
               val tp = if (s.warning) ErrorSource.WARNING else ErrorSource.ERROR
               val pos = s.ref.region.start
               val file = controller.backend.resolveLogical(s.ref.container) match {
                  case Some((a,p)) => (a.sourceDir / p).toString
                  case None => s.ref.container.toString
               }
               val error = new DefaultErrorSource.DefaultError(errorSource, tp, file, pos.line, pos.column, pos.column + 1, s.mainMessage)
               s.extraMessages foreach {m => error.addExtraMessage(m)}
               errorSource.addError(error)
            case e: Invalid =>
               //generated by StructureChecker
               val cause = e match {
                  case e: InvalidObject => e.obj
                  case e: InvalidElement => e.elem
               }
               val ref = SourceRef.get(cause) getOrElse SourceRef(utils.FileURI(path), SourceRegion(SourcePosition(0,0,0), SourcePosition(0,0,0))) 
               val reg = ref.region
               val error = new DefaultErrorSource.DefaultError(errorSource, ErrorSource.ERROR, buffer.getPath,
                     reg.start.line, reg.start.column, reg.start.column + reg.length, e.getMessage)
               errorSource.addError(error)
            case e: Error =>
               // other error, should not happen
               val error = new DefaultErrorSource.DefaultError(
                   errorSource, ErrorSource.ERROR, path.toString, 0, 0, 1, e.getMessage
               )
               errorSource.addError(error)
         }
      } catch {case e: java.lang.Throwable =>
         // other error, e.g., by the get methods in buildTree
         val error = new DefaultErrorSource.DefaultError(errorSource, ErrorSource.ERROR, path.toString, 0,0,0, e.getMessage)
         e.getStackTrace foreach {m => error.addExtraMessage(m.toString)}
         errorSource.addError(error)
         log(e.getMessage)
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
      val asset = MMTSideKick.getAssetAtOffset(view,caret)
      asset match {
         case a: MMTObjAsset =>
            a.obj match {
               case Hole(t) =>
                  val rules = prover.applicable(t)(Stack(Frame(a.getTheory, a.context)))
                  val comp = new ProverCompletion(view, controller, a.region, rules)
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
   def getAssetAtOffset(view: org.gjt.sp.jedit.View, caret: Int) = {
      val pd = SideKickParsedData.getParsedData(view)
      pd.getAssetAtOffset(caret).asInstanceOf[MMTAsset]
   }
}