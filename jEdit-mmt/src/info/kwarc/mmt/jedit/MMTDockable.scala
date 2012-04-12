package info.kwarc.mmt.jedit
import info.kwarc.mmt.api._
import org.gjt.sp.jedit._
import javax.swing._

class MMTDockable(view: View, position: String) extends JTextArea() {
   setEditable(false)
}