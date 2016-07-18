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
    val height = painter.getFontHeight()
    val width = painter.getFontMetrics().getWidths()(29)
    textArea.getLineText(physicalLine, segment) // stores the line in 'segment'
    val font = painter.getFont
    def paintDelim(startPoint: Point, color: Color, letter: String) {
       //gfx.setColor(Color.WHITE)
       //gfx.fillRect(startPoint.x, startPoint.y, width, height)
       gfx.setColor(color)
       gfx.fillRect(startPoint.x, startPoint.y, width, height)
       gfx.setColor(Color.WHITE)
       gfx.setFont(new Font(font.getName(), Font.PLAIN, font.getSize()*2/3))
       gfx.drawString(letter, startPoint.x + width/4, startPoint.y + height*3/4)
    }
    var globalOffset = 0
    try {
       val txtsegm = segment.array
       (0 to segment.count) foreach {localOffset =>
         globalOffset = localOffset + segment.offset
         // assigns to startPoint; or returns null if line not visible (which sometimes happens)
         val res = textArea.offsetToXY(physicalLine, localOffset, startPoint)
         if (res != null) txtsegm(globalOffset).toInt match {
           case 28 =>
             paintDelim(startPoint, new Color(high, high, low), "S")
           case 29 =>
             paintDelim(startPoint, new Color(high, low, low), "M")
           case 30 =>
             paintDelim(startPoint, new Color(low, high, low), "D")
           case 31 =>
             paintDelim(startPoint, new Color(low, low, high), "O")
           case _ =>
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
      if (sel.getStart <= offset && offset <= sel.getEnd)
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
 *  Tokens with type COMMENT4 will be hidden, regardless of its style settings. 
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
