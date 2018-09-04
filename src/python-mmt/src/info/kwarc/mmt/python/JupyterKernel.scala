package info.kwarc.mmt.python


import info.kwarc.mmt.api._
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

  def processRequest(kernel: JupyterKernelPython, session: String, req: String): PythonParamDict = {
    import REPLServer._
    req match{
      case "active computation" => {
        activeComputation(kernel)
        PythonParamDict()
      }
      case _ => {
        val comm = REPLServer.Command.parse(req)
        try {
          val resp = repl(Some(session), comm)
          resp match {
            case AdminResponse(s) => PythonParamDict("message" -> s)
            case e: ElementResponse =>
              val h = presenter.asString(e.element)
              PythonParamDict("element" -> h)

          }
        }
        catch {
          case e: info.kwarc.mmt.api.SourceError => PythonParamDict("element" ->e.mainMessage)
          case e: Exception => PythonParamDict("element" ->(e.getClass.toString+e.getMessage))//(e.getStackTrace.foldRight(""){ (ste,s) => (s +"\n"+ ste.toString)}))

        }
      }
    }

  }



//  def example(kernel: JupyterKernelPython) = {
//    // we can create Widgets like this:
//    Widget(kernel,"Text","value" -> "Hi").display
//
//    val slider = Widget(kernel,"IntRangeSlider","value" -> List(1,2), "description" -> "Hi")
//    val button = Widget(kernel,"Button","description" -> "Hi")
//
//    def buttonCallback(kernel: JupyterKernelPython, JO : Any) = button.set("description","Bye")
//    button.on_click(buttonCallback)
//
//    // or like this:
//    val boxArgs = PythonParamDict(
//      "children" -> List(slider,button)
//    )
//    kernel.makePWidget("HBox",boxArgs).display
//  }

  // simple example for e = mc²
  def activeComputation(kernel: JupyterKernelPython) = {
    val formula = kernel.makeWidget("Label").set("value",raw"$$ E = m \cdot c²$$").display

    val EButton = Widget(kernel,"ToggleButton","description"->"E","tooltip"->"the energy in J")
    val mButton = Widget(kernel,"ToggleButton","description"->"m","tooltip"->"the mass in kg")
    val cButton = Widget(kernel,"ToggleButton","description"->"c","tooltip"->"the speed of light in m/s")

    val E = Widget(kernel, "FloatText")
    val m = Widget(kernel, "FloatText")
    val c = Widget(kernel, "FloatText")

    val buttonList = List(EButton,mButton,cButton)
    val buttons = Widget(kernel,"VBox","children"->buttonList)
    val boxes = Widget(kernel,"VBox","children"->List(E,m,c))
    Widget(kernel,"HBox","children"->List(buttons,boxes)).display

    def toggle(kernel: JupyterKernelPython, dict: java.util.HashMap[String,Any]) = {
      val ppd = PythonParamDict.fromJavaHashMap(dict)
      val owner = ppd("owner").asInstanceOf[WidgetPython]
      val desc = owner.get("description")
      val value = owner.get("value")
      desc match {
        case "E" => {
          if(value == true){
            formula.set("value", raw"$$ E = m \cdot c²$$")
            E.set("disabled",true)
            m.set("disabled",false)
            c.set("disabled",false)
            mButton.set("value",false)
            cButton.set("value",false)
          }
        }
        case "m" => {
          if(value == true){
            formula.set("value", raw"$$ m = \frac{E}{c²}$$")
            E.set("disabled",false)
            m.set("disabled",true)
            c.set("disabled",false)
            EButton.set("value",false)
            cButton.set("value",false)
          }
        }
        case "c" => {
          if(value == true){
            formula.set("value", raw"$$ c = \sqrt{\frac{E}{m}}$$")
            E.set("disabled",false)
            m.set("disabled",false)
            c.set("disabled",true)
            EButton.set("value",false)
            mButton.set("value",false)
          }
        }
      }
    }

    EButton.observe(toggle,"value")
    mButton.observe(toggle,"value")
    cButton.observe(toggle,"value")

    def compute(kernel: JupyterKernelPython, dict: WidgetPython): Unit = {
      val toCompute = buttonList.map(_.getAsBool("value"))
      val mval = m.getAsFloat("value")
      val cval = c.getAsFloat("value")
      val Eval = E.getAsFloat("value")
      toCompute match {
          // this has to be untlimately done with MMT
        case List(true,false,false) => E.set("value",mval*(cval*cval))
        case List(false,true,false) => m.set("value",Eval/(cval*cval))
        case List(false,false,true) => c.set("value",Math.sqrt(Eval/mval))
        case _ =>
      }

    }
    Widget(kernel,"Button","description"->"Compute","tooltip"->"Compute the selected variable").on_click(compute).display
  }


}