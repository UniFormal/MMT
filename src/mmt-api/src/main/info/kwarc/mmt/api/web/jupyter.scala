package info.kwarc.mmt.api.web

object Jupyter {
  private var counter = -1
  private def getName = {
    counter += 1
    "name" + counter.toString
  }
  
  abstract class Python {
    def toPython: String
  }
  
  abstract class Widget extends Python {
    def wd: String = "widgets"
    def named = {
      Named(this)
    }
  }
  
  /**
   * in python, we have
   * widgets = {}
   * def makeNamedWidget(n,w):
   *    widgets[n] = w
   *    return w
   * def getNamedWidget(n):
   *    return widgets[n]
   * 
   */
  
  case class Named(widget: Widget) extends Widget {
    val name = getName
    def getByName = Command(s"getWidget($name)")
    def apply(method: String, args: Python*) = Command(getByName.toPython + "." + method + args.map(_.toPython).mkString("(",",",")"))
    def toPython = s"makeNamedWidget($name, ${widget.toPython})"
  }
  
  case class Command(p: String) extends Python {
    def toPython = p
  }

  case class Text(s: String) extends Widget {
    def toPython = s"$wd.Text($s)"
  }
  
  case class HBox(content: Widget*) extends Widget {
    def toPython = s"$wd.HBox(${content.map(_.toPython).mkString("[",",","]")})"
  }
  
  case class Button(onclick: () => Command) extends Widget {
    def toPython = s"$wd.button" // TODO set the onclick handler
  }
}

import Jupyter._

object JupyterTest {
  def apply() {
    val t = Text("hello").named
    val b = Button(() => t("setText", Command("world")))
    HBox(t,b).toPython
  }  
}