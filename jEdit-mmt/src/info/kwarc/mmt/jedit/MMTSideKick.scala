package info.kwarc.mmt.jedit
import org.gjt.sp.jedit._
import errorlist._
import sidekick._

import info.kwarc.mmt.api._
import parser._
import frontend._
import libraries._
import modules._
import symbols._
import documents._

import javax.swing.tree.DefaultMutableTreeNode
import scala.collection.JavaConversions.seqAsJavaList

case class MyPosition(offset : Int) extends javax.swing.text.Position {
   def getOffset = offset
}

class MMTAsset(elem : StructuralElement, name: String, reg: SourceRegion) extends enhanced.SourceAsset(name, reg.start.line, MyPosition(reg.start.offset)) {
   setEnd(MyPosition(reg.end.offset))
   setLongDescription(path.toPath)
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

// text is the string that is to be completed, items is the list of completions
class MyCompletion(view : org.gjt.sp.jedit.View, text: String, items: List[String])
  extends SideKickCompletion(view, text, items) {
   // override def insert(index: Int) // this methods modifies the textArea after the user selected a completion
}

class MMTSideKick extends SideKickParser("mmt") {
   // gets jEdit's instance of MMTPlugin, jEdit will load the plugin if it is not loaded yet
   val mmt : MMTPlugin = jEdit.getPlugin("info.kwarc.mmt.jedit.MMTPlugin", true).asInstanceOf[MMTPlugin]
   val controller = mmt.controller
   private def log(msg: => String) {
      controller.report("jedit-parse", msg)
   }
   private def getRegion(e: metadata.HasMetaData) : SourceRegion = MMTPlugin.getSourceRef(e) match {
      case None => SourceRegion(SourcePosition(0,0,0), SourcePosition(0,0,0))
      case Some(r) => r.region
   }
   private def buildTree(node: DefaultMutableTreeNode, doc: Document) {
      val child = new DefaultMutableTreeNode(new MMTAsset(doc, doc.path.last, getRegion(doc)))
      node.add(child)
      doc.getItems foreach {
        case d: DRef =>
           buildTree(child, controller.getDocument(d.target))
        case m: MRef =>
           buildTree(child, controller.localLookup.getModule(m.target))
      }
   }
   private def buildTree(node: DefaultMutableTreeNode, mod: Module) {
      val keyword = mod match {case _ : Theory => "theory"; case _: modules.View => "view"}
      val child = new DefaultMutableTreeNode(new MMTAsset(mod, keyword + " " + mod.path.last, getRegion(mod)))
      node.add(child)
      mod match {
         case m: DeclaredModule[_] =>
            m.valueListNG foreach {case d =>
               val label = d match {  
                  case PlainInclude(from,_) => "include " + from.last
                  case i: Include => "include"
                  case s: Structure => "structure " + s.name.flat + " "
                  case d: Declaration => d.name.flat
               }
               val grandchild = new DefaultMutableTreeNode(new MMTAsset(d, label, getRegion(d)))
               child.add(grandchild)
            }
         case m: DefinedModule =>
      }
   }
   def parse(buffer: Buffer, errorSource: DefaultErrorSource) : SideKickParsedData = {
      val path = utils.File(buffer.getPath)
      val dpath = DPath(path.toJava.toURI)
      val src = scala.io.Source.fromString(buffer.getText)
      controller.delete(dpath)
      val tree = new SideKickParsedData(path.toJava.getName)
      val root = tree.root
      try {
         val (doc,errors) = controller.textReader.readDocument(src, dpath)
         // add narrative structure of doc to outline tree
         buildTree(root, doc)
         // register errors with ErrorList plugin
         errors foreach {
            case s: SourceError =>
               //parse error thrown by TextReader
               val tp = if (s.warning || ! s.fatal) ErrorSource.WARNING else ErrorSource.ERROR
               val pos = s.region.start
               val error = new DefaultErrorSource.DefaultError(errorSource, tp, path.toString, pos.line, pos.column, pos.column + 1, s.mainMessage)
               errorSource.addError(error)
      }
      tree
      } catch {case e =>
         // other error, e.g., by the get methods in buildTree
         val error = new DefaultErrorSource.DefaultError(errorSource, ErrorSource.ERROR, path.toString, 0,0,0, e.getMessage)
         errorSource.addError(error)
         log(e.getMessage);
         tree
      }
   }
   // override def stop() 
   // override def getParseTriggers : String = ""

   override def supportsCompletion = true
   override def canCompleteAnywhere = true
   // override def getInstantCompletionTriggers : String = ""
   override def complete(editPane: EditPane, caret : Int) : SideKickCompletion = {
      val textArea = editPane.getTextArea
      val view = editPane.getView
      val pd = SideKickParsedData.getParsedData(view)
      val asset = pd.getAssetAtOffset(caret).asInstanceOf[MMTAsset]
      asset.getScope match {
        case Some(a) =>
           val p = textArea.getCaretPosition
           var l = 0 // number of character to the left of the caret that are id characters
           while (l < p && MMTPlugin.isIDChar(textArea.getText(p - l - 1,1)(0))) {l = l + 1}
           val partialName = textArea.getText(p - l, l)
           val compls = Names.resolve(a, Nil, partialName)(controller.localLookup)
           new MyCompletion(view, partialName, compls.map(_.completion.flat))
        case None => new MyCompletion(view, "", Nil)
      }
      
   }
}