package info.kwarc.mmt.jedit

import info.kwarc.mmt.api._
import org.gjt.sp.jedit._
import gatchan.jedit.hyperlinks._

class MyHyperlink(start: Int, end: Int, startLine: Int, path: Path, ref: SourceRef) extends AbstractHyperlink(start, end, startLine, path.toPath) {
   def click(view: View) {
      
   }
}

class MMTHyperlinkSource extends HyperlinkSource {
   private var currentLink : Hyperlink = null
   def getHyperlink(buffer: Buffer, caretPosition: Int) : Hyperlink = {
      // return a previously found link if it is still applicable
      if (currentLink != null && currentLink.getStartOffset() <= caretPosition && currentLink.getEndOffset() >= caretPosition)
         return currentLink
      //get information about the current line
      val line = buffer.getLineOfOffset(caretPosition)
      val lineStart = buffer.getLineStartOffset(line)
      val lineLength = buffer.getLineLength(line)
      if (lineLength == 0)
         return null
      var offset = caretPosition - lineStart
      val lineText = buffer.getLineText(line)
      if (offset == lineLength)
         offset = offset - 1
      // find the identifier at the caret 
      val wordStart = 1 //TextUtilities.findWordStart(lineText, offset, NO_WORD_SEP, true, false, false)
      val wordEnd = 2 //TextUtilities.findWordEnd(lineText, offset + 1, NO_WORD_SEP, true, false, false)
      val id = lineText.substring(wordStart, wordEnd);
      //resolve and locate the identifier
      val path : Path = null
      val ref : SourceRef = null
      //create a hyper link and return it
      currentLink = new MyHyperlink(lineStart + wordStart, lineStart + wordEnd, line, path, ref)
      currentLink
   }
}

