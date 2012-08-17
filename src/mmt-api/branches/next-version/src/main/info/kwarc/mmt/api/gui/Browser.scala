package info.kwarc.mmt.api.gui

import javafx.application.Platform
import javafx.scene.Scene
import javafx.stage.Stage
import javafx.scene.web._
import javafx.event._
import javafx.scene.input._
import javafx.embed.swing._

import javax.swing._

/*
class Browser extends Application {
   def start(stage: Stage) {
      stage.setTitle("Hello World!")
      val wv = new WebView
      val webEngine = wv.getEngine
      stage.setScene(new Scene(wv))
      webEngine.load("http://cds.omdoc.org:8080")
      stage.show()
   }
}
*/

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

object Browser {
   def test {
     val frame = new JFrame("test")
     val jfx = new JFXPanel
     frame.add(jfx)
     wrap {
       val wv = new WebView
       jfx.setScene(new Scene(wv))
       val webEngine = wv.getEngine
       webEngine.load("http://cds.omdoc.org:8080")
     }
     frame.setVisible(true)
     val scene = jfx.getScene
     scene.onKeyPressed = (e => println(e.getText()))
   }
}