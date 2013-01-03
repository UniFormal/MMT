package info.kwarc.mmt.api.gui
import javax.swing._

import java.awt.event.{ActionListener,ActionEvent}

case class Item(label: String, id: String)

class RadioButtonPanel(val items: Item*)(action: String => Unit) extends JPanel {
   setLayout(new BoxLayout(this, BoxLayout.X_AXIS))
   val al = new ActionListener {
      def actionPerformed(e: ActionEvent) {
         action(e.getActionCommand)
      }
   }
   val bg = new ButtonGroup
   items foreach {case Item(l,i) =>
      val b = new JRadioButton(l)
      b.setActionCommand(i)
      b.addActionListener(al)
      add(b)
      bg.add(b)
   }
   
}
