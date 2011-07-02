package info.kwarc.mmt.jedit
import org.gjt.sp.jedit._
import errorlist._
import sidekick._

import info.kwarc.mmt.api._
import frontend._
import libraries._

class MMTSideKick extends SideKickParser("mmt") {
   // override def getPanel()
   def parse(buffer: Buffer, errorSource: DefaultErrorSource) : SideKickParsedData = {
      null
   }
   // override def stop() 
   // override def getParseTriggers : String = ""

   override def supportsCompletion = true
   // override def canCompleteAnywhere = true
   // override def getInstantCompletionTriggers : String = ""
   override def complete(editPane: EditPane, caret : Int) : SideKickCompletion = {
      null
   }
}