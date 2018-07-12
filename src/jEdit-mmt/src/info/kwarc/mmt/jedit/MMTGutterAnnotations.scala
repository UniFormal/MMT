package info.kwarc.mmt.jedit

import java.awt.Font

import info.kwarc.mmt.api._
import ontology._
import org.gjt.sp.jedit._

/**
 * adds markers in the gutter whenever an annotation for a declaration in made in that line is known to MMT
 */
class MMTGutterAnnotations(mmt: MMTPlugin, editPane: EditPane) extends MMTTextAreaExtension(editPane) {
  private val extman = mmt.controller.extman

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
    val fontcolor = java.awt.Color.GRAY
    val annotations = getAnnotations(startOffset, endOffset) 
    if (annotations.isEmpty) return // optimization
    drawMarker(gfx, java.awt.Color.YELLOW, y, true)
    // TODO if not 1, write number of annotations into the oval, e.g., somehow like below
    var oldFont = gfx.getFont

    gfx.setFont(new Font(oldFont.getName, oldFont.getStyle, oldFont.getSize-2))
    if (annotations.size==1)
      drawChar (gfx, fontcolor, y-1, annotations.head.getMarker)
    else if (annotations.size<10)
      drawChar (gfx, fontcolor, y-1, annotations.size.toString.charAt(0))
    else
      drawChar (gfx, fontcolor, y-1, '+')
    gfx.setFont(oldFont)
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
    if (annotations.isEmpty) return null
    annotations.map(a => a.getTooltip).mkString("<html>","<br>","</html>")
  }
}