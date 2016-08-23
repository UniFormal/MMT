package info.kwarc.mmt.jedit

import info.kwarc.mmt.api._
import frontend._
import objects._

import org.gjt.sp.jedit._
import textarea._
import syntax._
import javax.swing.text.Segment
import java.awt.Font
import java.awt.font.TextAttribute
import javax.swing.ImageIcon

/** A TextAreaExtension that is added to every EditPane
 *  it can be used for custom painting, e.g., background highlighting, tooltips
 *  Currently it highlights terminator characters
 */
class MMTTextAreaExtension(controller: Controller, editPane: EditPane) extends TextAreaExtension {
   private def log(msg: String) {controller.report("jedit-painter", msg)}
   private val textArea = editPane.getTextArea
   private val view = editPane.getView
   private val painter = textArea.getPainter

   //val oIMG = new ImageIcon(this.getClass().getResource("/images/object_t.png")).getImage()
   //val dIMG = new ImageIcon(this.getClass().getResource("/images/clear_button.png")).getImage()
   //val mIMG = new ImageIcon(this.getClass().getResource("/images/clear_button.png")).getImage()

  import java.awt.{Point,Color}

  // imperatively reusing the same variables for efficiency
  private val segment = new Segment
  private val startPoint = new Point
  // parameters for coloring the delimiters
  private val high = 255
  private val low = 192

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
    /** paints MMT delimiter 'letter' at 'startPoint' with background 'color' */
    def paintDelim(startPoint: Point, color: Color, letter: String) {
       gfx.setColor(color)
       // left and right edges of rectangle: x and x + width - 1; top and bottom edges: y and y + height - 1
       gfx.fillRect(startPoint.x, startPoint.y, width, height)
       gfx.setColor(Color.WHITE)
       gfx.setFont(new Font(font.getName(), Font.PLAIN, font.getSize()*2/3))
       gfx.drawString(letter, startPoint.x + width/6, startPoint.y + height*2/3)
    }
    /** paints other character 'letter' at 'startPoint' using 'style' */
    def paintChar(offset: Int, startPoint: Point, style: SyntaxStyle, c: Char) {
       /* We have to erase the text painted by jEdit. We can only do that by painting a rectangle over it.
        * To avoid undoing an intended background color and because we cannot look up the color of a pixel,
        * we have to reimplement some low level functionality of the TextAreaPainter.
        * Specifically, fixedBgColor returns the selection or lineHighlight color (if any). We are currently not using the collapsedFold color.  
        */
       val bgColor = fixedBgColor(offset) orElse Option(style.getBackgroundColor) getOrElse painter.getBackground
       gfx.setColor(bgColor)
       gfx.fillRect(startPoint.x, startPoint.y, width, height)
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
         if (res != null) segment.charAt(localOffset).toInt match {
           case 28 =>
             paintDelim(startPoint, new Color(high, high, low), "S")
           case 29 =>
             paintDelim(startPoint, new Color(high, low, low), "M")
           case 30 =>
             paintDelim(startPoint, new Color(low, high, low), "D")
           case 31 =>
             paintDelim(startPoint, new Color(low, low, high), "O")
           case 32 =>
           case c =>
             // repaint c if semanticHighlighting returns a result
             val globalOffset = segment.offset + localOffset 
             val styleOpt = semanticHighlighting(globalOffset)
             styleOpt foreach {style =>
                if (MMTOptions.semantichighlighting.get.getOrElse(false)) paintChar(globalOffset, startPoint, style, c.toChar)
             }
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
  
   private def asString(o: Obj) = controller.presenter.asString(o)
   private def onSelection(ta: TextArea, offset: Int): Option[(Int,Int)] = {
      if (textArea.getSelectionCount != 1) return None
      val sel = textArea.getSelection(0)
      if (sel.getStart <= offset && offset < sel.getEnd)
         Some((sel.getStart, sel.getEnd))
      else
         None
   }
   override def getToolTipText(xCoord: Int, yCoord: Int): String = {
      val offset = textArea.xyToOffset(xCoord, yCoord, false)
      if (offset == -1) return null
      onSelection(textArea,offset) match {
         case Some((b,e)) =>
            val as = try {MMTSideKick.getAssetAtRange(view, b, e)} catch {case ex: Exception => return ex.getClass.toString+ex.getMessage+" " + b + " " + e}
            as match {
               case ta: MMTObjAsset =>
                  ta.obj match {
                     case t: Term =>
                        val thy = ta.getScope.getOrElse(return null)
                        val found = controller.extman.getFoundation(thy).getOrElse(return "no foundation")
                        val tp = try {found.inference(t, ta.context)(controller.globalLookup)}
                        catch {case e : Exception => return e.getMessage}
                        asString(tp)
                     case _ => return null
                  }
               case _ => return null
            }
         case None => 
            val as = MMTSideKick.getAssetAtOffset(view,offset)
            as match {
               case Some(ta: MMTObjAsset) =>
                  ta.obj match {
                    case OMV(n) =>
                      asString(ta.context(n))
                    case vd : VarDecl =>
                        asString(vd)
                    case OMA(OMID(p), args) =>
                        val implicits = args.filter(a => parser.SourceRef.get(a).isEmpty)
                        if (implicits.isEmpty) null
                        else implicits.map(asString).mkString("   ")
                    case _ => null
                  }
               case _ => null
            }
      }
   }
}

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
