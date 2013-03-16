package info.kwarc.mmt.api.gui
import javax.swing._

import java.awt.event.{ActionListener,ActionEvent}

/** a label-id pair, used e.g., to label and identify a GUI item */
case class Item(label: String, id: String)

object Swing {
   /** a JPanel containing a list of horizontal radio buttons
    * that hides the boilerplate Java code
    * @param items the label-id pairs of the buttons
    * @param action the function to be called when a button is selected, takes the button's id as its argument
    * @return the JPanel
    */
   def RadioButtonPanel(items: Item*)(action: String => Unit) : JPanel = {
      val jp = new JPanel 
      jp.setLayout(new BoxLayout(jp, BoxLayout.X_AXIS))
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
         jp.add(b)
         bg.add(b)
      }
      jp
   }
}
