/*
package info.kwarc.mmt.api.gui

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
   private var sceneVar: MyScene = null  // valid only after calling init
   /** the scene in the FX component */
   def scene = sceneVar

   private var engineVar: WebEngine = null  // valid only after calling init
   /** the FX web engine */
   def engine = engineVar

   /** initializes the FXFrame */
   private def init {
     wrap {
       val wv = new WebView
       setScene(new Scene(wv))
       engineVar = wv.getEngine
       sceneVar = MyScene(getScene)
     }
     //setVisible(true)
   }
   
   /** a wrapper around engine.loadContent, runs on the FX application thread */
   def loadContent(html: scala.xml.Node) {
      val page = <html><body>{html}</body></html>
      //println(page.toString)
      wrap {
         engine.loadContent(page.toString)
      }
   }
   
   // initialize the instance
   init
}

*/