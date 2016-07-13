package info.kwarc.mmt.api.gui
import info.kwarc.mmt.api._
import backend._
import archives._

import javax.swing._
import event._

class BackendPane(backend: Backend) extends {val ta = new JTextArea} with JScrollPane(ta) {
   def set = {
      var s : String = ""
      backend.getStores foreach {
         case a: Archive =>
            s += "archive " + a.id + " " + a.rootString + "\n"
         case ra: RealizationStorage =>
            s += ra.toString
         case l: LocalCopy =>
            s += "mathpath fs " + l.localBase + " " + l.base + "\n"
         case a =>
            s += "other storage " + a.toString + "\n"
      }
      ta.setText(s) 
   }
   set
}