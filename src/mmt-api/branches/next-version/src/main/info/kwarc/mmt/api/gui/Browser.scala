package info.kwarc.mmt.api.gui

import javax.swing._
import java.awt.event._

import info.kwarc.mmt.api._
import frontend._

class Browser(wm: WindowManager) extends JFrame("MMT Browser") {
   private val controller = wm.controller
   private val textArea = new JTextArea()
   add(textArea)
   /* private val paintButton = new JButton("repaint")
   paintButton.addEventListener()
   add(paintButton) */
   addWindowListener(new WindowAdapter {
      override def windowClosed(e: WindowEvent) {wm.closeBrowser} 
   })
   setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
   setVisible(true)
   def paint {
      var s : String = ""
      s += "logging\n"
      s += controller.report.groups.toList.mkString("\t", ", ", "\n")
      s += "\narchives\n"
      controller.backend.getArchives foreach {a =>
         s += "\t" + a.id + " " + a.rootString + "\n"
      }
      s += "\ndocuments\n" 
      controller.docstore.getDocuments foreach {d =>
         s += "\t" + d.path.toString + "\n"
         d.getItems foreach {i =>
            s += "\t\t" + i.target.toString + "\n"
         }
      }
      s += "\nmodules\n" 
      controller.library.getModules foreach {m =>
         s += "\t" + m.path.toString + "\n"
      }
      
      textArea.setText(s)
      pack()
   }
}