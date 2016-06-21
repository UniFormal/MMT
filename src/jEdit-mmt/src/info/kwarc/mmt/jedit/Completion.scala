package info.kwarc.mmt.jedit

import org.gjt.sp.jedit._
import textarea._

import sidekick._

import info.kwarc.mmt.api._
import parser._
import checking._
import notations._
import frontend._
import libraries._
import modules._
import objects._
import symbols._

import scala.collection.JavaConversions.seqAsJavaList

/**
 * @param view the current jEdit view
 * @param controller the current MMT controller
 * @param constants the MMT constants that are applicable here 
 * @param text the partial identifier that is completed
 * @param items the list of completion labels that is displayed
 */
class IDCompletion(view : org.gjt.sp.jedit.View, controller: Controller, constants: List[Constant], text: String, items: List[String])
  extends SideKickCompletion(view, text, items) {
  // this methods modifies the textArea after the user selected a completion
   override def insert(index: Int) {
     val con = constants(index)
     //the text to insert and after how many characters to place the caret
     val (newText,shift): (String,Int) = con.not match {
        case None =>
           val s = con.name.toPath + " "
           (s, s.length + 1)
        case Some(not) =>
           val text = not.parsingMarkers.map {
              case d: Delimiter => d.expand(con.path, con.alias).text
              case w: WordMarker => " " + w.word + " "
              case sa: SeqArg => " " + sa.sep.text + " "
              case a:Arg => " "
              case _:ImplicitArg => ""
              case v: Var => " "
              case p: PresentationMarker => ""
              case AttributedObject => ""
           }.mkString("")
           val sh = not.parsingMarkers.head match {
              case d: Delimiter => d.text.length + 1
              case _ => 0
           }
           (text, sh)
     }
     //replace text with newText and move the caret by shift
     val ta = view.getEditPane.getTextArea
     val caret = ta.getCaretPosition
     ta.setSelection(new Selection.Range(caret-text.length, caret))
     ta.setSelectedText(newText)
     ta.setCaretPosition(caret-text.length+shift)
   }
}

/**
 *
 */
class ProverCompletion(view : org.gjt.sp.jedit.View, controller: Controller, region: SourceRegion, options: List[Term])
  extends SideKickCompletion(view, "", options.map(o => controller.presenter.asString(o))) {
  // this methods modifies the textArea after the user selected a completion
  override def insert(index: Int) {
     // the new subterm, result of applying the rule
     val newTerm = options(index)
     val newText = controller.presenter.asString(newTerm)
     // replace the old subterm with the new one
     // TODO decide whether to put brackets
     // TODO put cursor in front of first hole, reparse buffer
     val ta = view.getEditPane.getTextArea
     ta.setSelection(new Selection.Range(region.start.offset, region.end.offset+1))
     ta.setSelectedText(newText)
  }
}