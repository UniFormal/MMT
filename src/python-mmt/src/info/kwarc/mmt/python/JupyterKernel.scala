package info.kwarc.mmt.python

import info.kwarc.mmt.api._
import web._
import frontend._

import utils._

/** interface to the Python side of the Jupyter kernel (will be implemented in Python) */
trait WidgetPython {
  def name: String
  def get(key: String): String
  def set(key: String, value: String): Unit
  def setCallbackKeys(keys: List[String]): Unit
  def observe(key: String, callback: (JupyterKernelPython,Any) => Unit)
  def toString: String
}

/** interface to the Python side of the Jupyter kernel (will be implemented in Python) */
trait JupyterKernelPython {
  def getWidgets: List[WidgetPython]
  def addWidget(w: WidgetPython)
  def removeWidget(name: String)
  def makeWidget(kind: String): WidgetPython
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
  
  def processRequest(kernel: JupyterKernelPython, session: String, req: String): JSON = {
    val comm = REPLServer.Command.parse(req)
    val resp = repl(Some(session), comm)
    resp.toJSON
    
    // example code, delete eventually
    //val w = kernel.getWidgets.find(_.name == "hello").get
    //val before = w.get("hello")
    //w.set("hello", "bla")
    // beginnings of the real code
    //val command = REPLServer.Command.parse(req)
    //val resp = repl(None, command)
    //val wid = kernel.makeWidget("test")
    //wid.set("text", "init")
    //wid.observe("text", (k,a) => wid.set("text", "changed"))
    //kernel.addWidget(wid)
    // "ok"
    //resp.toString
  }
}