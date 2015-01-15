
package info.kwarc.mmt.api.gui
/*
import javafx.application.Platform
import javafx.scene.Scene
import javafx.stage.Stage
import javafx.scene.web._
import javafx.event._
import javafx.scene.input._
import javafx.embed.swing._

import javax.swing._

/** Scala sugar for FX EventHandler */
//e.g., fxframe.scene.onKeyPressed = (e => println(e.getText()))
case class Handler[E <: Event](f : E => Unit) extends EventHandler[E] {
   def handle(e: E) {f(e)}
}

/** Scala wrapper for FX Scene with some smart functions */
case class MyScene(scene: Scene) {
   def onKeyPressed = scene.getOnKeyPressed
   def onKeyPressed_= (f: KeyEvent => Unit) {
      scene.setOnKeyPressed(Handler(f))
   }
}

/** implicit conversion from Scene to MyScene */
object MyScene {
   implicit def sToMyS(s: Scene) = MyScene(s)
}
import MyScene._

/** FX helper functionality */
object FXHelpers {
   /** evaluates its arguments via Platform.runLater */
   def wrap[A](a : => A) {
      val runnable = new Runnable {
         def run {a}
      }
      Platform.runLater(runnable)
   }
}
import FXHelpers._

/** a JFXPanel inside a swing JFrame containing only a WebView
 * Note that access to the scene and engine must be from the FX application thread (see FXHelpers.wrap).
 * scene and engine are initialized on the FX application thread
 * and may not be valid yet (null) immediately after object creation.
 */
class FXPanel extends JFXPanel {
   private var _scene: MyScene = null  // valid only after calling init
   /** the scene in the FX component */
   def scene = _scene

   private var _engine: WebEngine = null  // valid only after calling init
   /** the FX web engine */
   def engine = _engine

   /** initializes the FXPanel */
   private def init {
     wrap {
       val wv = new WebView
       setScene(new Scene(wv))
       _engine = wv.getEngine
       _scene = MyScene(getScene)
     }
   }
   
   /** a wrapper around engine.loadContent, runs on the FX application thread */
   def load(html: scala.xml.Node) {
      wrap {
         engine.loadContent(html.toString)
      }
   }
   def loadBody(html: scala.xml.Node) {
      val page = <html><body>{html}</body></html>
      load(page)
   }
   def loadBody(s: String) {
      loadBody(scala.xml.Text(s))
   }
   
   // initialize the instance
   init
}

/** a JFrame containing a single FXPanel
 */
class FXFrame extends JFrame("MMT-FX") {
   val panel = new FXPanel
   add(panel)
   setVisible(true)
}
*/