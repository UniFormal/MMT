package info.kwarc.mmt.jedit
import org.gjt.sp.jedit._
import errorlist._
import sidekick._

import info.kwarc.mmt.api._
import frontend._
import libraries._
import javax.swing.tree.DefaultMutableTreeNode

import scala.collection.JavaConversions.asJavaList

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
   def parse(buffer: Buffer, errorSource: DefaultErrorSource) : SideKickParsedData = {
      val path = buffer.getPath()
      val tree = new SideKickParsedData(path)
      val root = tree.root
      // build a tree with some random data for testing 
      val child1 = MyNode("child1", 0, 0, 9)
      root.add(child1)
      val child2 = MyNode("child2", 0, 10, 20)
      root.add(child2)
      val child21 = MyNode("child21", 1, 21, 30)
      child2.add(child21)
      val child22 = MyNode("child22", 2, 31, 40)
      child2.add(child22)
      
      // register some errors
      val error1 = new DefaultErrorSource.DefaultError(errorSource, ErrorSource.WARNING, path, 3,0,0,"a full line warning")
      val error2 = new DefaultErrorSource.DefaultError(errorSource, ErrorSource.ERROR, path, 4,4,15,"a partial line error")
      errorSource.addError(error1)
      errorSource.addError(error2)
      
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