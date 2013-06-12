package info.kwarc.mmt.api.gui
import info.kwarc.mmt.api._
import frontend._

import javax.swing._
import event._

/*
//remove [String] if you try to compile with Java 1.6 (or update to Java 1.7)
class LogPane(report: Report) extends ListModel[String] {
   def addListDataListener(l: ListDataListener){}
   def removeListDataListener(l: ListDataListener) {} 
   def getElementAt(index: Int) = {
      Report.groups(index)
   }
   def getSize: Int = Report.groups.length
}

object LogPane {
  def apply(controller: Controller) = {
     val report = controller.report
     val list = new JList(new LogPane(report))
     val sl = new ListSelectionListener {
        def valueChanged(e: ListSelectionEvent) {
           Report.groups.zipWithIndex map {case (s,i) =>
           if (list.isSelectedIndex(i))
             report.groups += s
           else
             report.groups -= s
           }
        }
     }
     list.getSelectionModel.addListSelectionListener(sl)
     list
  }
}
*/