package info.kwarc.mmt.jedit
import org.gjt.sp.jedit._
import errorlist._
import sidekick._

import info.kwarc.mmt.api._
import frontend._
import libraries._

import javax.swing.tree.DefaultMutableTreeNode
import scala.collection.JavaConversions.seqAsJavaList

case class MyPosition(offset : Int) extends javax.swing.text.Position {
   def getOffset = offset
}

object MyNode {
   // not sure if line is ever used for anything interesting
   def apply(name: String, line: Int, startOffset: Int, endOffset: Int) : DefaultMutableTreeNode = {
      val asset = new enhanced.SourceAsset(name, line, MyPosition(startOffset))
      asset.setEnd(MyPosition(endOffset))
      new DefaultMutableTreeNode(asset)
   }
}

// text is the string that is to be completed, items is the list of completions
class MyCompletion(view : View, text: String, items: List[String])
  extends SideKickCompletion(view, text, items) {
   // override def insert(index: Int) // this methods modifies the textArea after the user selected a completion
}

class MMTSideKick extends SideKickParser("mmt") {
   // gets jEdit's instance of MMTPlugin, jEdit will load the plugin if it is not loaded yet
   val mmt : MMTPlugin = jEdit.getPlugin("info.kwarc.mmt.jedit.MMTPlugin", true).asInstanceOf[MMTPlugin]
   def parse(buffer: Buffer, errorSource: DefaultErrorSource) : SideKickParsedData = {
      val path = buffer.getPath
      val src = scala.io.Source.fromString(buffer.getText)
      controller.clear
      val (doc,errors) = controller.textReader.readDocument(src, DPath(path.toURI))
      val tree = new SideKickParsedData(path)
      val root = tree.root
      doc.getItems foreach {
        case d: DRef =>
           val child = MyNode(d.target.last, 0, 0, 0)
           root.add(child)
        case m: MRef =>
           val child = MyNode(d.target.last, 0, 0, 0)
           root.add(child)
      }
      
      // register some errors
      errors foreach {e =>
         //val error = new DefaultErrorSource.DefaultError(errorSource, ErrorSource.WARNING, path, 3,0,0,"a full line warning")
         val error = new DefaultErrorSource.DefaultError(errorSource, ErrorSource.ERROR, path, 4,4,15, e.msg)
         errorSource.addError(error)
      }      
      tree
   }
   // override def stop() 
   // override def getParseTriggers : String = ""

   override def supportsCompletion = true
   // override def canCompleteAnywhere = true
   // override def getInstantCompletionTriggers : String = ""
   override def complete(editPane: EditPane, caret : Int) : SideKickCompletion = {
      val view = editPane.getView
      val pd = SideKickParsedData.getParsedData(view) // we have access to the result of the parser
      val asset = pd.getAssetAtOffset(caret)
      // return a dummy completion popup
      new MyCompletion(view, "test", List("compl1", "compl2", asset.getName))
   }
}