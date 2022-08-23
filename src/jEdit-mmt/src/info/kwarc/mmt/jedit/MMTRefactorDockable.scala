package info.kwarc.mmt.jedit

import javax.swing.{JPanel, BoxLayout}

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.Module
import info.kwarc.mmt.api.presentation
import info.kwarc.mmt.api.presentation.MMTSyntaxPresenter
import info.kwarc.mmt.api.refactoring.RefactorPanel
import org.gjt.sp.jedit.{jEdit, View}


class MMTRefactorDockable(jview: View, position: String) extends JPanel {
  setLayout(new BoxLayout(this,BoxLayout.X_AXIS))
  try {
    add(new RefactorPanel(Publisher.controller, Publisher.publish(jview)))
  } catch {
    case e: Throwable => throw e
  }
  revalidate()
}

object Publisher {
  def publish(jview:View)(s:List[Module]): Unit = {
    implicit val rh = new presentation.StringBuilder
    val presenter = new MMTSyntaxPresenter
    controller.extman.addExtension(presenter)

    for (o <- s) {
      presenter(o)
    }
    jview.getTextArea.setText(jview.getTextArea.getText+"\n\n"+rh.get)
    controller.extman.removeExtension(presenter)
  }

  val controller = jEdit.getPlugin("info.kwarc.mmt.jedit.MMTPlugin", true).asInstanceOf[MMTPlugin].controller
}
