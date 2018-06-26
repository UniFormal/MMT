package info.kwarc.mmt.python

import info.kwarc.mmt.api._
import web._
import frontend._

/** interface to the Python side of the Jupyter kernel (will be implemented in Python) */
trait WidgetPython {
  def name: String
  def get(key: String): String
  def set(key: String, value: String): Unit
}

/** interface to the Python side of the Jupyter kernel (will be implemented in Python) */
trait JupyterKernelPython {
  def getWidgets: List[WidgetPython]
  def addWidget(w: WidgetPython)
  def removeWidget(name: String)
}

class JupyterKernel extends Extension {
  private var repl: REPLServer = null
  
  override def start(args: List[String]) {
    super.start(args)
    val extman = controller.extman
    repl = extman.get(classOf[REPLServer]).headOption getOrElse {
      val r = new REPLServer
      extman.addExtension(r,Nil)
      r
    }
  }
  
  def processRequest(kernel: JupyterKernelPython, req: String): String = {
    // example code, delete eventually
    val w = kernel.getWidgets.find(_.name == "hello").get
    val before = w.get("hello")
    w.set("hello", "bla")
    // beginnings of the real code
    val command = REPLServer.Command.parse(req)
    val resp = repl(None, command)
    resp.toString
  }
}