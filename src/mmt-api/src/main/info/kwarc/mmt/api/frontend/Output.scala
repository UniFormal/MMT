package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api.documents.{Document, FolderLevel, MRef}
import info.kwarc.mmt.api.objects.Obj
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.presentation.{ConsoleWriter, RenderingHandler, StringBuilder}

/** represents the final post-processing phase, which outputs concrete syntax */
abstract class Output {
  def make(controller: Controller): Unit
}

/** prints to STDOUT */
case class Print(pres: MakeConcrete) extends Output {
  def make(controller: Controller): Unit = {
    pres.make(controller, ConsoleWriter)
    ConsoleWriter("\n")
  }

  override def toString: String = pres.toString
}

/** writes to a file */
case class ToFile(pres: MakeConcrete, file: java.io.File) extends Output {
  def make(controller: Controller): Unit = {
    val rb = new presentation.FileWriter(file)
    pres.make(controller, rb)
    rb.done
  }

  override def toString: String = pres.toString + " write " + file
}

/** displays content in a window */
case class ToWindow(pres: MakeConcrete, window: String) extends Output {
  def make(controller: Controller): Unit = {
    val rb = new StringBuilder
    pres.make(controller, rb)
    val res = rb.get
    controller.winman.getWindow(window).set(res)
  }

  override def toString: String = pres.toString + " window " + window
}

/** produces the result and throws it away
  *
  * call get to keep it in memory and retrieve it
  */
case class Respond(pres: MakeConcrete) extends Output {
  def get(controller: Controller): String = {
    val rb = new StringBuilder
    pres.make(controller, rb)
    rb.get
  }

  def make(controller: Controller): Unit = {
    get(controller)
  }

  override def toString: String = pres.toString + " respond"
}

/** represent retrieval operations that return content elements
  *
  * These objects become Action commands by wrapping them in post-processing steps.
  * see the children of MakePresentation
  */
abstract class MakeAbstract {
  def make(controller: Controller): Content
}

/** retrieves a knowledge item */
case class Get(p: Path) extends MakeAbstract {
  def make(controller: Controller): StructuralElement = {
    controller.get(p)
  }
  override def toString = p.toString
}

/** retrieves a component of a knowledge item */
case class Component(p: Path, comp: ComponentKey) extends MakeAbstract {
  def make(controller: Controller): Content = {
    val o = controller.get(p)
    val tOpt = o.getComponent(comp) flatMap {
      case c: AbstractTermContainer => c.get
      case _ => throw ParseError("component name " + comp + " not a term component in " + o)
    }
    tOpt.getOrElse {
      throw ParseError("component name " + comp + " illegal for element " + o)
    }
  }
  override def toString: String = p.toString + " component " + comp
}

/** retrieves the closure of a knowledge item */
case class Closure(p: Path) extends MakeAbstract {
  def make(controller: Controller): Document = p match {
    case doc ? name =>
      controller.get(doc ? name) // retrieve once to make sure it's in memory
    val cl = controller.depstore.theoryClosure(doc ? name)
      val clp = cl.map { p => MRef(doc, p) }
      new Document(doc, FolderLevel, inititems = clp)
  }

  override def toString = p.toString + " closure"
}

/** retrieves the elaboration of an instance */
case class Elaboration(p: Path) extends MakeAbstract {
  def make(controller: Controller) = {
    controller.get(p) match {
      //case i : Instance => new TGroup(i.parent,p,Instance.elaborate(i)) //TODO
      case _ => throw ImplementationError("Non-instance element at " + p)
    }
  }

  override def toString: String = p.toString + " elaboration"
}



/** takes a content element and renders it using notations */
case class Present(c: MakeAbstract, param: String) extends MakeConcrete {
  def make(controller: Controller, rb: RenderingHandler): Unit = {
    val presenter = controller.extman.get(classOf[presentation.Presenter], param).getOrElse {
      throw PresentationError("no presenter found: " + param)
    }
    c.make(controller) match {
      case s: StructuralElement => presenter(s)(rb)
      case o: Obj => presenter(o, None)(rb)
    }
  }

  override def toString: String = c.toString + " present " + param
}

/** represents the first post-processing phase
  *
  * These produce concrete syntax from the abstract syntax.
  */
abstract class MakeConcrete {
  /** takes a Controller, executes the rendering and passes it to a RenderingHandler */
  def make(controller: Controller, rb: RenderingHandler): Unit
}

/** retrieves all relational elements about a certain path and renders them as XML */
case class Deps(path: Path) extends MakeConcrete {
  def make(controller: Controller, rb: RenderingHandler): Unit = {
    rb.elem("mmtabox", "xmlns" -> "http://omdoc.org/abox") {
      (controller.depstore.getInds ++ controller.depstore.getDeps).foreach(
        (d: ontology.RelationalElement) => if (path <= d.path) rb(d.toNode)
      )
    }
  }

  override def toString: String = path.toString + " deps"
}

