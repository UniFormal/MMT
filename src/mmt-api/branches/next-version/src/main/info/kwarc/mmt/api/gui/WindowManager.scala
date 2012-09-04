package info.kwarc.mmt.api.gui

import javax.swing._

class Window(val id: String) extends JFrame(id) {
   private val textArea = new JTextArea(20,20)
   add(textArea)
   setVisible(true)
   def set(content: String) {
      textArea.setText(content)
   }
}

class WindowManager {
   private var windows : List[Window] = Nil
   def getWindow(id: String) : Window = {
      windows.find(_.id == id) match {
         case Some(w) => w
         case None => createWindow(id)
      }
   }
   def createWindow(id: String) : Window = {
      val w = new Window(id)
      windows ::= w
      w
   }
}