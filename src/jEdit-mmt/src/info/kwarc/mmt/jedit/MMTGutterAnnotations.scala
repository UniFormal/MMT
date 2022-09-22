package info.kwarc.mmt.jedit

import java.awt.{Dimension, Font, GridBagConstraints, GridBagLayout, ScrollPane}
import java.awt.event.{ActionEvent, ActionListener, MouseAdapter, MouseEvent}

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.gui.MMTElemAsset
import javax.swing._
import ontology._
import org.gjt.sp.jedit._

/**
 * adds markers in the gutter whenever an annotation for a declaration in made in that line is known to MMT
 */
class MMTGutterAnnotations(mmt: MMTPlugin, editPane: EditPane) extends MMTTextAreaExtension(editPane) {
  private val extman = mmt.controller.extman

  object mouseAdapter extends MouseAdapter {
    override def mouseClicked(e: MouseEvent): Unit = {
      val x = e.getX
      val y = e.getY
      if (!editPane.getBuffer.isLoaded) //not loaded
        return
      val textArea = editPane.getTextArea
      val offset = textArea.xyToOffset(x,y)
      if (offset == -1)//out of bounds
        return
      //gather annotations for line
      val line = textArea.getLineOfOffset(offset)
      val start = textArea.getLineStartOffset(line)
      val end   = textArea.getLineEndOffset(line)
      val annotations = getAnnotations(start, end)
      if (annotations.isEmpty) return
      new AnnotationDialogue(annotations)
    }
  }

  /** gets Annotations for assets at specified positions
    *
    * @param start start of area to annotate
    * @param end end of area to annotate
    * @return annotations as Seq[Annotation]
    */
  private def getAnnotations(start: Int, end: Int): Seq[Annotation] = {
    //gather URIs of all assets in area
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
    //check all loaded AnnotationProviders for annotations
    val providers = extman.get(classOf[AnnotationProvider])
    uris flatMap {u =>
      providers.flatMap {ap => ap(u)}
    }
  }

  /** paints markers for lines that have associated annotations
    *
    * @param gfx The graphics context
    * @param screenLine The screen line number
    * @param physicalLine The physical line number
    * @param startOffset The offset where the screen line begins, from the start of the buffer
    * @param endOffset The offset where the screen line ends, from the start of the buffer
    * @param y The y co-ordinate of the top of the line's bounding box
    */
  // called on every visible line every time we scroll or edit
  override def paintValidLine(gfx: java.awt.Graphics2D, screenLine: Int, physicalLine: Int, startOffset: Int, endOffset: Int, y: Int): Unit = {
    val fontcolor = java.awt.Color.GRAY
    //find annotations
    val annotations = getAnnotations(startOffset, endOffset)
    if (annotations.isEmpty) return // no annotations in line
    //mark line
    drawMarker(gfx, java.awt.Color.YELLOW, y, true)
    //draw marker character
    val oldFont = gfx.getFont
    gfx.setFont(new Font(oldFont.getName, oldFont.getStyle, oldFont.getSize-2))
    if (annotations.size==1) {
      //if only one annotation display it's designated Marker character (defaults to ' ')
      if (annotations.head.getMarker != ' ') drawChar(gfx, fontcolor, y - 1, annotations.head.getMarker) //draw char, skip if ' '
    }
    else if (annotations.size<10) //only display number if single character, to fit Marker
      drawChar (gfx, fontcolor, y-1, annotations.size.toString.charAt(0))
    else //display '+' if 10 or more annotations
      drawChar (gfx, fontcolor, y-1, '+')
    gfx.setFont(oldFont)
  }

  /** provides tooltip if hovering over annotation markers
    *
    * @param x The x co-ordinate as used by jEdit
    * @param y The y co-ordinate as used by jEdit
    * @return
    */
  override def getToolTipText(x: Int, y: Int): String = {
    if (!editPane.getBuffer.isLoaded) //not loaded
			 return null
    val textArea = editPane.getTextArea
		val offset = textArea.xyToOffset(x,y)
		if (offset == -1)//out of bounds
			return null
    //gather annotations for line
    val line = textArea.getLineOfOffset(offset)
    val start = textArea.getLineStartOffset(line)
    val end   = textArea.getLineEndOffset(line)
    val annotations = getAnnotations(start, end)
    if (annotations.isEmpty) return null //no annotations found
    //convert annotations to html string
    annotations.map(a => a.getTooltip).mkString("<html>","<br>","</html>")
  }
}

class AnnotationDialogue(annotations : Seq[Annotation]) extends JFrame("Annotations") {
  frame =>

  //Content
  var current = 0
  setLocation(java.awt.MouseInfo.getPointerInfo.getLocation)
  setLayout(new GridBagLayout)
  val contentpane = new ScrollPane()
  val contentConstraints = new GridBagConstraints()
  contentConstraints.gridx = 0
  contentConstraints.gridy = 0
  contentConstraints.weightx = 1.0
  contentConstraints.weighty = 0.5
  contentConstraints.fill = GridBagConstraints.BOTH
  add(contentpane, contentConstraints)
  display(current)

  //Buttons
  val buttonpane = new JPanel()
  val buttonConstraints = new GridBagConstraints()
  buttonConstraints.gridx = 0
  buttonConstraints.gridy = 1
  buttonConstraints.weightx = 1.0
  buttonConstraints.weighty = 0
  buttonConstraints.fill = GridBagConstraints.HORIZONTAL
  add(buttonpane, buttonConstraints)
  val prevButton = new JButton("prev")
  prevButton.addActionListener(new ActionListener(){
    def actionPerformed(e : ActionEvent): Unit ={
      if (current>0){
        current-=1
        display(current)
      }
      if (current==0){
        prevButton.setEnabled(false)
      }
      nextButton.setEnabled(true)
    }
  })
  prevButton.setEnabled(false)
  buttonpane.add(prevButton)

  val nextButton = new JButton("next")
  nextButton.addActionListener(new ActionListener(){
    def actionPerformed(e : ActionEvent): Unit ={
      if (current+1<annotations.size){
        current+=1
        display(current)
      }
      if (current+1>=annotations.size){
        nextButton.setEnabled(false)
      }
      prevButton.setEnabled(true)
    }
  })
  if (current+1>=annotations.size){
    nextButton.setEnabled(false)
  }
  buttonpane.add(nextButton)

  val closeButton = new JButton("close")
  closeButton.addActionListener(new ActionListener(){
    def actionPerformed(e : ActionEvent): Unit ={
      frame.dispose()
    }
  })
  buttonpane.add(closeButton)

  //show frame
  pack()
  setVisible(true)

  /** displays annotation at position i
    *
    * @param i index of annotation
    */
  def display(i : Int) : Unit = {
    contentpane.add(annotations(i).dialogueContent)
    contentpane.setPreferredSize(
      new Dimension(
        math.max(contentpane.getComponent(0).getPreferredSize.width+contentpane.getVScrollbarWidth+2, 500),
        math.max(contentpane.getComponent(0).getPreferredSize.height+contentpane.getHScrollbarHeight+2, 150)
      )
    )
  }
}