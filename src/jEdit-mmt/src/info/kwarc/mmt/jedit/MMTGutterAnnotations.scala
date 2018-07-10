package info.kwarc.mmt.jedit

import info.kwarc.mmt.api._
import ontology._

import org.gjt.sp.jedit._
import org.gjt.sp.jedit.textarea._

/**
 * adds markers in the gutter whenever an annotation for a declaration in made in that line is known to MMT
 */
class MMTGutterAnnotations(mmt: MMTPlugin, editPane: EditPane) extends MMTTextAreaExtension(editPane) {
  private val extman = mmt.controller.extman
  
  import parser._
  private def getAnnotations(start: Int, end: Int): Seq[Annotation] = {
    val assetOs = (start until end) map {o => MMTSideKick.getAssetAtOffset(view, o)} //TODO check if this is too slow
    val uris = assetOs.distinct flatMap {
      case Some(a: MMTElemAsset) =>
        val o = a.region.start.offset
        if (start <= o && o < end)
          List(a.path)
        else
          Nil
      case _ => Nil
    }
    val providers = extman.get(classOf[AnnotationProvider])
    uris flatMap {u =>
      providers.flatMap {ap => ap(u)}
    }
  }
  
  // called on every visible line every time we scroll or edit
  override def paintValidLine(gfx: java.awt.Graphics2D, screenLine: Int, physicalLine: Int, startOffset: Int, endOffset: Int, y: Int) {
    val annotations = getAnnotations(startOffset, endOffset) 
    if (annotations.isEmpty) return // optimization
    drawMarker(gfx, java.awt.Color.YELLOW, y, true)
    // TODO if not 1, write number of annotations into the oval, e.g., somehow like below
    // gfx.setFont(style.getFont)
    // val fm = painter.getFontMetrics
    // val baseLine = startPoint.y + painter.getFontHeight - (fm.getLeading()+1) - fm.getDescent() // taken from TextAreaPainter#PaintText
    // gfx.drawString(c.toString, startPoint.x, baseLine)
  }
  
  override def getToolTipText(x: Int, y: Int): String = {
    if (!editPane.getBuffer.isLoaded)
			 return null
    val textArea = editPane.getTextArea
		val offset = textArea.xyToOffset(x,y)
		if (offset == -1)
			return null
    val line = textArea.getLineOfOffset(offset)
    val start = textArea.getLineStartOffset(line)
    val end   = textArea.getLineEndOffset(line)
    val annotations = getAnnotations(start, end)
    annotations.map {a => a.getTooltip}.mkString("<html>","<br/>","</html>")
  }
}