package info.kwarc.mmt.api.gui
import javax.swing._

import java.awt.event.{ActionListener,ActionEvent}

/** a label-id pair, used e.g., to label and identify a GUI item */
case class Item(label: String, id: String)

object Swing {

   /** run code on the Swing thread */
   def invokeLater[A](a: => A): Unit = {
      val r = new Runnable() {
        def run: Unit = {
          a
        }
      }
      SwingUtilities.invokeLater(r)
   }

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
         def actionPerformed(e: ActionEvent): Unit = {
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

   def Button(label: String, tooltip: String = "")(action: => Unit): JButton = {
      val b = new JButton(label)
      val al = new ActionListener {
         def actionPerformed(e: ActionEvent): Unit = {
            action
         }
      }
      b.addActionListener(al)
      b.setToolTipText(tooltip)
      b
   }

   def HorizontalPanel: java.awt.Container = {
      val panel = new JPanel()
      val layout = new BoxLayout(panel, BoxLayout.LINE_AXIS)
      panel.setLayout(layout)
      panel
   }
   def VerticalPanel: java.awt.Container = {
      val panel = new JPanel
      val layout = new BoxLayout(panel, BoxLayout.PAGE_AXIS)
      panel.setLayout(layout)
      panel
   }
   def centeredLabel(s: String) = {
      val l = new JLabel(s)
      l.setAlignmentX(java.awt.Component.CENTER_ALIGNMENT)
      l
   }
}

import java.awt._

/**
 *  Modified FlowLayout that wraps lines properly
 *
 *  This follows the ideas of WrapLayout.java but adds a defaultWidth
 */
class WrapLayout(defaultWidth: Int, align: Int = FlowLayout.CENTER, hgap: Int = 5, vgap: Int = 5)
      extends FlowLayout(align, hgap, vgap) {
   private var preferredLayoutSize: Dimension = null

   override def preferredLayoutSize(target: Container) = layoutSize(target, true)
   override def minimumLayoutSize(target: Container) = {
      val minimum = layoutSize(target, false)
      minimum.width -= getHgap + 1
      minimum
   }
   private def layoutSize(target: Container, preferred: Boolean) = {
      var targetWidth = target.getSize.width
      if (targetWidth == 0) targetWidth = defaultWidth
      val hgap = getHgap
      val vgap = getVgap
      val insets = target.getInsets
      val horizontalInsetsAndGap = insets.left + insets.right + (hgap * 2)
      val maxWidth = targetWidth - horizontalInsetsAndGap
      val dim = new Dimension(0, 0)
      var rowWidth = 0
      var rowHeight = 0
      val nmembers = target.getComponents.foreach {m =>
         if (m.isVisible) {
            val d = if (preferred) m.getPreferredSize else m.getMinimumSize
            if (rowWidth + d.width > maxWidth) {
               addRow(dim, rowWidth, rowHeight)
               rowWidth = 0
               rowHeight = 0
            } else
               rowWidth += hgap
            rowWidth += d.width;
            rowHeight = Math.max(rowHeight, d.height)
         }
      }
      addRow(dim, rowWidth, rowHeight)
      dim.width += horizontalInsetsAndGap
      dim.height += insets.top + insets.bottom + vgap * 2
      if (SwingUtilities.getAncestorOfClass(classOf[JScrollPane], target) != null && target.isValid)
         dim.width -= (hgap + 1)
      dim
   }

   private def addRow(dim: Dimension, rowWidth: Int, rowHeight: Int): Unit = {
      dim.width = Math.max(dim.width, rowWidth)
      if (dim.height > 0)
         dim.height += getVgap
      dim.height += rowHeight
   }
}
