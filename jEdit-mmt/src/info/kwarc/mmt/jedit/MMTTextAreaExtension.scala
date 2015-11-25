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

   val oIMG = new ImageIcon(
     this.getClass().getResource(
       "/images/object_t.png")).getImage()
   val dIMG = new ImageIcon(
     this.getClass().getResource(
       "/images/clear_button.png")).getImage()
   val mIMG = new ImageIcon(
     this.getClass().getResource(
       "/images/clear_button.png")).getImage()

   private val segment = new Segment

  override def paintValidLine(gfx: java.awt.Graphics2D, screenLine: Int, physicalLine: Int, startOffset: Int, endOffset: Int, y: Int) {
    val height = painter.getFontHeight()
    val width = painter.getFontMetrics().getWidths()(29)

    val s = segment
    textArea.getLineText(physicalLine, s)
    val linelen = s.count
    val txtsegm = s.array

    val font = painter.getFont

    var globalOffset = 0
    var localOffset = 0
    var startPoint = new java.awt.Point

    for (localOffset <- 0 to linelen-1) {
      globalOffset = localOffset + s.offset
      
      textArea.offsetToXY(physicalLine,
        localOffset, startPoint)

      txtsegm(globalOffset).toInt match {
        case 29 =>
          gfx.setColor(java.awt.Color.WHITE)
          gfx.fillRect(startPoint.x, startPoint.y,
            width, height)

          gfx.setColor(new java.awt.Color(24, 28, 114))
          gfx.fillRect(startPoint.x,
            startPoint.y + height/2 - width/2,
            width, width)

          gfx.setColor(java.awt.Color.WHITE)
          gfx.setFont(new Font(font.getName(),
            Font.PLAIN, 2*font.getSize()/3))
          gfx.drawString("M",
            startPoint.x + width/6,
            startPoint.y + height/2 + 3*width/8)
        case 30 =>
          gfx.setColor(java.awt.Color.WHITE)
          gfx.fillRect(startPoint.x, startPoint.y,
            width, height)

          gfx.setColor(new java.awt.Color(24, 28, 114))
          gfx.fillRect(startPoint.x,
            startPoint.y + height/2 - width/2,
            width, width)

          gfx.setColor(java.awt.Color.WHITE)
          gfx.setFont(new Font(font.getName(),
            Font.PLAIN, 2*font.getSize()/3))
          gfx.drawString("D",
            startPoint.x + width/6,
            startPoint.y + height/2 + 3*width/8)
        case 31 =>
          gfx.setColor(java.awt.Color.WHITE)
          gfx.fillRect(startPoint.x, startPoint.y,
            width, height)

          gfx.setColor(new java.awt.Color(24, 28, 114))
          gfx.fillRect(startPoint.x,
            startPoint.y + height/2 - width/2,
            width, width)

          gfx.setColor(java.awt.Color.WHITE)
          gfx.setFont(new Font(font.getName(),
            Font.PLAIN, 2*font.getSize()/3))
          gfx.drawString("O",
            startPoint.x + width/6,
            startPoint.y + height/2 + 3*width/8)
        case _ =>
      }

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
                        val found = controller.extman.getFoundation(ta.getTheory).getOrElse(return "no foundation")
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
