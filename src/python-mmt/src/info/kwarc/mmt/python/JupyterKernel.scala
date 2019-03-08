package info.kwarc.mmt.python


import info.kwarc.mmt.api._
import objects.{Sub, Substitution, Term, Text}
import web._
import frontend._
import presentation._

import scala.collection.JavaConverters._


case class PythonParamDict (map: List[(String,Any)]){
  def items = map
  def iterator = map.iterator.map(_._1)
  def contains(s: String): Boolean = map.map(_._1).contains(s)
  def apply(s: String) : Any = map.find(_._1 == s).map(_._2).get
}

object PythonParamDict {
  def apply(cases: (String,Any)*): PythonParamDict = PythonParamDict(cases.toList)
  def fromMap(map: Map[String,Any]): PythonParamDict = PythonParamDict(map.toList)
  def fromJavaHashMap(map: java.util.HashMap[String,Any]): PythonParamDict = PythonParamDict.fromMap(map.asScala.toMap)
}


/** interface to the Python side of the Jupyter kernel (is implemented in Python) */
trait JupyterKernelPython {
  def getWidgets: List[WidgetPython]
  def makeWidget(kind: String): WidgetPython

  def makePWidget(kind: String, args: PythonParamDict): WidgetPython

  def Text(args: PythonParamDict): Text
  def display(widgets: List[WidgetPython])

}

/** interface to the Python side of the Jupyter kernel (will be implemented in Python) */
// we probably should have a Class for every Widget type,
// since not all widgets support all the methods mentioned below... this also would
// make clear which kind of widgets are useable
trait WidgetPython {
  def set(key: String, value: Any): WidgetPython
  def get(key: String): Any
  def getAsFloat(key: String) : Float
  def getAsBool(key: String) : Boolean
  def getAsString(key: String): String
  def getID: String
  def observe(callback: (JupyterKernelPython,java.util.HashMap[String,Any]) => Unit, key: String) : WidgetPython
  def on_click(callback: (JupyterKernelPython,WidgetPython) => Unit) : WidgetPython
  def display: WidgetPython
  def close
  def toString: String
}

// for more convenient widget construction
object  Widget {
  def apply(kernel: JupyterKernelPython,kind: String, cases: (String,Any)*) = kernel.makePWidget(kind,PythonParamDict(cases.toList))
}



class JupyterKernel extends Extension {
  private var repl: REPLServer = null
  private lazy val presenter = new InNotebookHTMLPresenter(new MathMLPresenter)
  private val logFile = utils.File("mmt-jupyter-kernel").addExtension("log")
  logFile.createNewFile()
  private val errorCont = new ErrorWriter(logFile, None)

  override def logPrefix: String = "jupyter"
  
  override def start(args: List[String]) {
    super.start(args)
    initOther(presenter)
    val extman = controller.extman
    repl = extman.get(classOf[REPLServer]).headOption getOrElse {
      errorCont.open
      val r = new REPLServer
      extman.addExtension(r,Nil)
      r
    }
  }

  override def destroy: Unit = {
    errorCont.close
    super.destroy
  }

  // private def returnError(e: Exception): PythonParamDict = returnError(Error(e).toStringLong)
  private def returnError(msg: String): List[(String, Any)] = List("element" -> msg)

  def processRequest(kernel: JupyterKernelPython, sessionId: String, req: String): PythonParamDict = {
    PythonParamDict(processRequestInt(kernel, sessionId, req))
  }

  private def processRequestInt(kernel: JupyterKernelPython, sessionId: String, req: String): List[(String, Any)] = try {
    import REPLServer._

    // lazily find the repl session
    lazy val session = repl.getSessionOpt(sessionId).getOrElse {
      return returnError("session " + sessionId + " not found")
    }

    req match {
      case s if s.startsWith("active computation ") =>
        val tail = s.substring("active computation ".length).split(" ", 2)
        activeComputation(kernel, session, tail(0).split(",").toList, tail(1))
        List()
      case s if s.startsWith("mitm ") =>
        // mitm EXP evaluates EXP using a MitM computation
        val rest = s.substring(5)
        val con = session.parseObject(rest)
        val df = con.df.getOrElse {
          return returnError("epxression was processed but no definiens was found afterwards")
        }
        import info.kwarc.mmt.MitM.VRESystem._
        val mitm = new MitMComputation(controller)
        val trace = new MitMComputationTrace(None)
        // log(df.toString)
        val dfN = mitm.simplify(df, Some(objects.Context(con.parent)))(trace)
        // log("Result: " + dfN)
        con.dfC.analyzed = Some(dfN)
        controller.add(con)
        val dfNP = presenter.asString(dfN)
        val result = if (controller.report.checkDebug) {
          dfNP + "\n" + trace.toString(t => t.toString)
        } else
          dfNP
        List("element" -> result)
      case _ => {
          val comm = REPLServer.Command.parse(req)
          val resp = repl(Some(sessionId), comm, Some(errorCont))
          resp match {
            case m: MultiTypedResponse => m.messages.toList
            case e: ElementResponse =>
              val h = presenter.asString(e.element)
              List("element" -> h)
          }
      }
    }

  } catch {
    case e: info.kwarc.mmt.api.SourceError => List("element" -> presenter.exceptionAsHTML(e))
    case e: Exception => List("element" -> presenter.exceptionAsHTML(e))
  }

  // simple example for e = mc²§session
  def activeComputation(kernel: JupyterKernelPython, session: REPLSession, variables: List[String], termS: String) = {
    val term = session.parseTerm(termS)

    // the top-most formula to display
    val formula = Widget(kernel, "HTML", "value" -> presenter.asString(term))

    // create the labels and inputs
    val labels = variables.map(v => Widget(kernel, "Label", "value"->v))
    val inputs = variables.map(v => Widget(kernel, "Text"))
    val inputrows = (labels zip inputs).map(lt => Widget(kernel,"HBox","children"->List(lt._1, lt._2)))
    val contextboxes = Widget(kernel, "VBox", "children"->inputrows)

    // add a button and output widget
    val button = Widget(kernel, "Button", "description" -> "Simplify")
    val output = Widget(kernel, "HTML", "value" -> "<i>Click simplify to start</i>")

    // a function to perform the actual computation
    def compute(sub: Substitution): Term = {
      term.^?(sub)
    }

    // on clicking the button, extract the context
    // and then compute
    button.on_click((kernel: JupyterKernelPython, dict: WidgetPython) => {
      val userInput: Map[String, String] = Map((variables zip inputs).map(vi => (vi._1, vi._2.getAsString("value"))):_*)

      val result = try {
        // parse the user context
        val ctx = userInput.map(kv => (LocalName.parse(kv._1), session.parseTerm(kv._2)))
        // build a substiution
        val subst = Substitution(ctx.toList.map(lt => Sub(lt._1, lt._2)):_*)

        // and do the computation
        presenter.asString(compute(subst))
      } catch {
        case e: Error => e.toHTML
        case e: Exception => Error(e).toHTML
      }
      output.set("value", result)
    })

    val vertical: List[WidgetPython] = List(formula, contextboxes, button, output)
    Widget(kernel, "VBox", "children" -> vertical).display
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
 
