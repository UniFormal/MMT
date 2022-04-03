package info.kwarc.mmt.jedit

import info.kwarc.mmt.api._
import frontend._
import objects._
import symbols._
import gui._

import org.gjt.sp.jedit._
import textarea._
import syntax._
import javax.swing.text.Segment
import java.awt.Font
import java.awt.font.TextAttribute

import javax.swing.ImageIcon

/** A TextAreaExtension that is added to every EditPane
 *  it can be used for custom painting, e.g., background highlighting, tooltips
 *  Currently it allows highlighting certain characters by painting over the ones painted by jEdit.
 */
class MMTTextHighlighting(controller: Controller, editPane: EditPane) extends MMTTextAreaExtension(editPane) {
   private def log(msg: String) {controller.report("jedit-painter", msg)}
   private val painter = textArea.getPainter

  import java.awt.{Point,Color}

  // imperatively reusing the same variables for efficiency
  private val segment = new Segment
  private val startPoint = new Point

  /*
    jEdit 5.6 modes cannot highlight strings that start with high Unicode characters.
    Therefore, we use this extension to highlight manually.
   */
  private val characterStyles = List(
    // LF
    '⟶' -> Token.KEYWORD2,
    // delims
    '❘' -> Token.LITERAL4, '❙' -> Token.LITERAL3, '❚' -> Token.LITERAL4
  )
  private val characterClassStyles = List(
    // typical logical operators
    Character.MATH_SYMBOL -> Token.KEYWORD3,
    Character.NON_SPACING_MARK -> Token.KEYWORD3
  )

  override def paintValidLine(gfx: java.awt.Graphics2D, screenLine: Int, physicalLine: Int, startOffset: Int, endOffset: Int, y: Int) {
    if (editPane.getBuffer.getMode.getName != "mmt") return
    // the current default font
    val font = painter.getFont
    val height = painter.getFontHeight
    val width = painter.getFontMetrics.getWidths()(29)
    /** the background color if we paint the current line and the current line is to be highlighted */
    val lineHighlightBgColor : Option[Color] = {
      val caret = textArea.getCaretPosition
      if (painter.isLineHighlightEnabled && startOffset <= caret && caret < endOffset) {
        Some(painter.getLineHighlightColor)
      } else
        None
    }
    // the background color of selected text
    val selectionColor = if (textArea.isMultipleSelectionEnabled) painter.getMultipleSelectionColor else painter.getSelectionColor
    /** get the background color that jedit already painted */
    def fixedBgColor(offset: Int): Option[Color] = {
      if (textArea.getSelectionCount == 0) {
         lineHighlightBgColor
      } else if (textArea.getSelectionAtOffset(offset) != null)
         Some(selectionColor)
       else
         //TODO if there is a single selection in the current line, jedit is still putting the lineHighlightColor
         None
    }
    /** paints other character 'c' at 'startPoint' using 'style' */
    def paintChar(offset: Int, startPoint: Point, style: SyntaxStyle, c: Char) {
       /* We have to erase the text painted by jEdit. We can only do that by painting a rectangle over it.
        * To avoid undoing an intended background color and because we cannot look up the color of a pixel,
        * we have to reimplement some low level functionality of the TextAreaPainter.
        * Specifically, fixedBgColor returns the selection or lineHighlight color (if any). We are currently not using the collapsedFold color.
        * Even so, this does not work well, e.g., any change that does not trigger MMT parsing messes up everything.
        * Alternatively, we could call removeExtension on paintText and paintTextBackground and then add our own variants.
        * But those extension do various additional things that would have to be reimplemented;
        * this is difficult because some of those things happen in private methods or even private classes.
        * Even keeping the background and repainting the text in background color is difficult because the style that was used is private.
        */
       val bgColor = fixedBgColor(offset) orElse Option(style.getBackgroundColor) getOrElse painter.getBackground
       gfx.setColor(bgColor)
       //gfx.fillRect(startPoint.x, startPoint.y, width, height)
       gfx.setColor(style.getForegroundColor)
       gfx.setFont(style.getFont)
       val fm = painter.getFontMetrics
       val baseLine = startPoint.y + painter.getFontHeight - (fm.getLeading()+1) - fm.getDescent() // taken from TextAreaPainter#PaintText
       gfx.drawString(c.toString, startPoint.x, baseLine)
    }
    val styles = painter.getStyles
    /* chooses a syntax style based on the MMT syntax
     *  @return Some(tc) if the token class returned by the MMT mode should be overridden with tc
     */
    def semanticHighlighting(offset: Int): Option[SyntaxStyle] = {
      val as = MMTSideKick.getAssetAtOffset(editPane.getView, offset)
      // TODO: make this smarter (see jEdit's syntax highlighting preferences for the available token classes)
      /* TODO some objects are missing source references and are marked like their super-term, i.e., as a ComplexTerm
       * some OMV's, some OMID's, some top level lambdas in definitions (even though subobjects have source references)
       * it's unclear why these objects lose or never acquire their source references
       */
      val tcOpt = as match {
        case Some(oa: MMTObjAsset) =>
          val tc = oa.pragmatic match {
            case _: OMV => Token.KEYWORD1
            case _: VarDecl => Token.KEYWORD2  // variable in binding
            case OMID(op) =>
              // unapplied operator
              if (oa.getScope == Some(op.module))
                Token.KEYWORD3  // operator from current theory
              else
                Token.KEYWORD4  // operator from meta-theory or included theory
            case ComplexTerm(op,_,_,_) =>
              // applied operator and delimiters
              if (oa.getScope == Some(op.module))
                Token.FUNCTION
              else
                Token.OPERATOR
            case _: OMLITTrait => Token.LITERAL1
            case _ => Token.INVALID
          }
          Some(tc)
        case _ => None
      }
      tcOpt.map(tc => styles(tc))
    }
    textArea.getLineText(physicalLine, segment) // stores the line in 'segment'
    try {
       (0 to segment.length) foreach {localOffset =>
         val res = textArea.offsetToXY(physicalLine, localOffset, startPoint)
         // if res != null: XY-coordinates of global offset assigned to startPoint (relative to the upper left corner of text area)
         // if res == null, point not visible (which sometimes happens)
         if (res != null) {
           val c = segment.charAt(localOffset)
           val globalOffset = segment.offset + localOffset
           def pC(c: Char, si: Int) = paintChar(globalOffset, startPoint, styles(si), c)
           val iO = utils.listmap(characterStyles, c) orElse
                    utils.listmap(characterClassStyles, Character.getType(c))
           iO.foreach {i =>
             pC(c,i)
           }

           /* if (MMTOptions.semantichighlighting.get.getOrElse(false)) {
             // repaint c if semanticHighlighting returns a result
             val globalOffset = segment.offset + localOffset
             val styleOpt = semanticHighlighting(globalOffset)
             styleOpt foreach {style =>
                 paintChar(globalOffset, startPoint, style, c.toChar)
             }
           }*/
         }
       }
    } catch {
       // need to hide all exceptions because jEdit removes our painter if it throws exceptions
       // e.g., sometimes txtsegm(globalOffset) throws ArrayIndexOutOfBounds
       // maybe because the buffer changes while we're painting?
       case e:Exception =>
          // log(e.getClass.toString + ": " + e.getMessage)
    }
  }

}

/** shows tooltips such as type inference (separate from the other TextAreaExtension so that it can be put on a different layer) */
class MMTToolTips(controller: Controller, editPane: EditPane) extends TextAreaExtension {
   private def log(msg: String) {controller.report("tooltips", msg)}
   private val textArea = editPane.getTextArea
   private val view = editPane.getView
   
   private def asString(o: Obj) = controller.presenter.asString(o)
   
   override def getToolTipText(xCoord: Int, yCoord: Int): String = {
      val offset = textArea.xyToOffset(xCoord, yCoord, false)
      if (offset == -1) return null
      val (asset,selected) = MMTSideKick.getSelectedAssetAtOffset(view,offset) getOrElse{return null}
      try {
        asset match {
          case da: MMTElemAsset =>
            da.elem match {
              case c: Constant =>
                c.tp match {
                  case Some(t) =>
                    asString(t)
                  case _ =>
                    null
                }
              case _ =>
                null
            }
          case ta: MMTObjAsset =>
            lazy val tpS = ta.inferType.map(asString).getOrElse(null)
            if (selected) {
              tpS
            } else {
              ta.obj match {
                case _:OMV | _:VarDecl =>
                  val vd = ta.obj match {
                    case OMV(n) => ta.context(n)
                    case vd: VarDecl => vd
                  }
                  vd.tp match {
                    case None => null
                    case Some(tp) =>
                      vd.name + ": " + asString(tp)
                  }
                case OMA(OMID(p), args) =>
                  val opWithImplicits = args.takeWhile(a => parser.SourceRef.get(a).isEmpty)
                  val opWithImplicitsT = OMA(OMID(p), opWithImplicits)
                  val opS = opWithImplicits.map(asString).mkString("  ")
                  val opTp = checking.Solver.infer(controller, ta.getFullContext, opWithImplicitsT, None)
                  val opTpS = opTp.map(t => "  :  " + asString(t)).getOrElse("")
                  opS + opTpS
                case _ =>
                  tpS
              }
            }
          case _ => null
        }
      } catch {case e: Exception =>
        e.getMessage
      }
   }
}

/* old code for tweaking the style, did not work well but kept to document jEdit API functions

//to highlight the current expression implement this
//class MMTMatcher extends org.gjt.sp.jedit.textarea.StructureMatcher
//call editPane.getTextArea().addStructureMatcher, e.g., in sidekick parser's activate method, to register it

object StyleChanger {
   def hidden(style: SyntaxStyle) = {
      val newFont = style.getFont.deriveFont(0f)
      new SyntaxStyle(style.getForegroundColor, style.getBackgroundColor, newFont)
   }
   def subscript(style: SyntaxStyle) = {
      val attributes = new java.util.Hashtable[TextAttribute,Int]
      attributes.put(TextAttribute.SUPERSCRIPT, TextAttribute.SUPERSCRIPT_SUB)
      val newFont = style.getFont.deriveFont(attributes)
      new SyntaxStyle(style.getForegroundColor, style.getBackgroundColor, newFont)
   }
}

/** A TextAreaExtension that is added to a layer below TEXT_LAYER in order to change the painter's styles for a certain mode
 *  The painter's styles are set by the EditPane according to SyntaxUtilities.loadStyles.
 *  This class will override that setting.
 *  Tokens with type COMMENT3 and COMMENT4 will be subscripted resp. hidden, regardless of their style settings.
 */
class StyleChanger(editPane: EditPane, modeName: String) extends TextAreaExtension {
   private val textArea = editPane.getTextArea
   private val painter = textArea.getPainter
   override def paintValidLine(gfx: java.awt.Graphics2D, screenLine: Int, physicalLine: Int, startOffset: Int, endOffset: Int, y: Int) {
       if (editPane.getBuffer.getMode.getName == modeName) {
          val styles = painter.getStyles
          styles(Token.COMMENT4) = StyleChanger.hidden(styles(Token.COMMENT4))
          styles(Token.COMMENT3) = StyleChanger.subscript(styles(Token.COMMENT3))
       }
   }
}
*/
