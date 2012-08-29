package info.kwarc.mmt.jedit
import info.kwarc.mmt.api._
import org.gjt.sp.jedit._
import javax.swing._

class MMTDockable(view: View, position: String) extends JPanel {
   def init {}
}

// commented out for now to avoid dependency on jfxrt.jar
/*
import javafx.application.Platform
import javafx.scene.Scene
import javafx.stage.Stage
import javafx.scene.web._
import javafx.event._
import javafx.scene.input._
import javafx.embed.swing._

import javax.swing._

case class Handler[E <: Event](f : E => Unit) extends EventHandler[E] {
   def handle(e: E) {f(e)}
}

case class MyScene(scene: Scene) {
   def onKeyPressed = scene.getOnKeyPressed
   def onKeyPressed_= (f: KeyEvent => Unit) {
      scene.setOnKeyPressed(Handler(f))
   }
}

object MyScene {
   implicit def sToMyS(s: Scene) = MyScene(s)
}

import MyScene._

object FXHelpers {
   def wrap[A](a : => A) {
      val runnable = new Runnable {
         def run {a}
      }
      Platform.runLater(runnable)
   }
}

import FXHelpers._


class MMTDockable(view: View, position: String) extends JPanel {
   var scene: Scene = null
   def init {
     val jfx = new JFXPanel
     add(jfx)
     add(new JTextArea(20,20))
     wrap {
        val wv = new WebView
        jfx.setScene(new Scene(wv))
        val webEngine = wv.getEngine
        webEngine.load("http://www.google.de")
     }
     scene = jfx.getScene
  }
}
*/