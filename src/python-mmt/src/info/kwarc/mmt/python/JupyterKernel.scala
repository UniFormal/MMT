package info.kwarc.mmt.python


import info.kwarc.mmt.api._
import web._
import frontend._
import presentation._
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
  private lazy val presenter = controller.extman.get(classOf[Presenter], "html").getOrElse(controller.presenter)
  
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
    import REPLServer._
    val comm = REPLServer.Command.parse(req)
    val resp = repl(Some(session), comm)
    resp match {
      case AdminResponse(s) =>
        JSONObject("message" -> JSONString(s))
      case e: ElementResponse =>
        val h = presenter.asString(e.element)
        JSONObject("element" -> JSONString(h))
    }
    
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

/* 
 * TODO add %%mmt in standard Python kernel, map it to a method 'parse' here
 * then using a special MMT kernel becomes redundant
 * moreover, widgets can be constructed easily by evaluating a Python expression that returns the widget
 * if the object returned by 'parse' implements the magic methods for dot notation,
 * the SageExplorer widget for dynamic and interactive documentation/introspection works immediately for that object
 
 * to register a magic command like %%mmt, see https://ipython.org/ipython-doc/3/config/custommagics.html
 * 
 */
 