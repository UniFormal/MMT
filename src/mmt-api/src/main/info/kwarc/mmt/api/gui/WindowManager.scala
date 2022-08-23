package info.kwarc.mmt.api.gui

import javax.swing._
import java.awt.event._

import info.kwarc.mmt.api._
import frontend._

class Window(val id: String, wm: WindowManager) extends JWindow() {
   private val textArea = new JTextArea()
   add(textArea)
   addWindowListener(new WindowAdapter {
      override def windowClosed(e: WindowEvent): Unit = {wm.deleteWindow(id)}
   })
   //setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
   setVisible(true)
   //toFront()
   setAlwaysOnTop(true)
   def set(content: String): Unit = {
      textArea.setText(content)
      pack()
   }
}

class WindowManager(val controller: Controller) {
   UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName()) // standard GUI configuration
   private var windows : List[Window] = Nil
   private var gui: Option[JFrame] = None
   def openBrowser: Unit = {
      val b = new GUIFrame(this)
      //b.paint
      gui = Some(b)
   }
   def closeBrowser: Unit = {
      gui.foreach(_.dispose)
      gui = None
   }
   def getWindow(id: String) : Window = {
      windows.find(_.id == id) match {
         case Some(w) => w
         case None => createWindow(id)
      }
   }
   def deleteWindow(id: String): Unit = {
      windows = windows filter {w =>
         if (w.id == id) {
            w.dispose
            false
         } else true
      }
   }
   def createWindow(id: String) : Window = {
      val w = new Window(id, this)
      windows ::= w
      w
   }
}
