package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api.ComponentKey
import info.kwarc.mmt.api.frontend._


/** Objects of type GetAction represent commands
  *
  * that
  * - retrieve knowledge items in abstract/internal syntax
  * - post-process them to obtain concrete/external syntax
  * - then output the concrete form in various ways.
  *
  * The concrete syntax is described by the following grammar:
  * get ABSTRACT [CONCRETE] [OUTPUT]
  * where
  * ABSTRACT ::= URI | URI component STRING | URI closure | URI elaboration
  * CONCRETE ::= present param | deps
  * OUTPUT   ::= write FILE | window | respond | CONCRETE
  * The productions for ABSTRACT, CONCRETE, OUTPUT correspond to the instances of MakeAbstract, MakeConcrete, and Output.
  *
  * @param o the instance of Output that executes all three steps.
  */
case class GetAction(o: Output) extends ActionImpl {
  /** implement the Action using the provided Controller */
  def apply(implicit controller: Controller): Unit = o.make(controller)

  def toParseString = o.toString
}
object GetActionCompanion extends ActionCompanionImpl[GetAction]("retrieve knowledge from the controller", "get") {
  import Action._
  override val addKeywords = false
  def parserActual(implicit state: ActionState) = tofile | towindow | respond | print

  // print is default
  private def tofile(implicit state: ActionState) = presentation ~ ("write" ~> file) ^^ { case p ~ f => GetAction(ToFile(p, f)) }
  private def towindow(implicit state: ActionState) = presentation ~ ("window" ~> str) ^^ { case p ~ w => GetAction(ToWindow(p, w)) }
  private def respond(implicit state: ActionState) = (presentation <~ "respond") ^^ { case p => GetAction(Respond(p)) }
  private def print(implicit state: ActionState) = presentation ^^ { p => GetAction(Print(p)) }
  // all the different presentations
  private def presentation(implicit state: ActionState) = present | deps | defaultPresent
  private def present(implicit state: ActionState) = content ~ ("present" ~> str) ^^ { case c ~ p => Present(c, p) }
  private def deps(implicit state: ActionState) = path <~ "deps" ^^ { case p => Deps(p) }
  private def defaultPresent(implicit state: ActionState) = content ^^ { c => Present(c, "text") }
  private def content(implicit state: ActionState) = "get" ~> (closure | elaboration | component | get)
  private def closure(implicit state: ActionState) = path <~ "closure" ^^ { p => Closure(p) }
  private def elaboration(implicit state: ActionState) = path <~ "elaboration" ^^ { p => Elaboration(p) }
  private def component(implicit state: ActionState) = (path <~ "component") ~ str ^^ { case p ~ s => Component(p, ComponentKey.parse(s)) }
  private def get(implicit state: ActionState) = path ^^ {p => Get(p)}
}
