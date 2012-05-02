package info.kwarc.mmt.jedit
import org.gjt.sp.jedit._

object MMTProperty {
   def apply(s: String) = "info.kwarc.mmt.jedit.MMTPlugin" + s
}

class MMTOptions extends AbstractOptionPane("info.kwarc.mmt.jedit.MMTPlugin")  {
   private var startupComponent = new java.awt.TextField
   override def _init {
      val startup = jEdit.getProperty(MMTProperty("startup"))
      startupComponent.setText(if (startup != null) startup else "") 
      addComponent("startup file (relative to MMT plugin home)", startupComponent)
   }
   override def _save {
      val startup = startupComponent.getText
      jEdit.setProperty(MMTProperty("startup"), startup)
   }
}