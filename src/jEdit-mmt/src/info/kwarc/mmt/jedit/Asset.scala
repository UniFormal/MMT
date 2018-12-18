package info.kwarc.mmt.jedit

import sidekick._
import org.gjt.sp.jedit._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.gui._
import parser._
import objects._
import modules._
import symbols._
import notations._

case class MyPosition(offset : Int) extends javax.swing.text.Position {
   def getOffset = offset
}

/** node in the sidekick outline tree: common ancestor class
 * @param name the label of the asset
 * @param region the source region of the asset
 */
sealed abstract class JAsset(protected val label: String, val region: SourceRegion) extends enhanced.SourceAsset(label, region.start.line, MyPosition(region.start.offset))
  with MMTAsset {
  setEnd(MyPosition(region.end.offset+1))

  /** can be used with TextArea.setSelection to select this asset */
  def toSelection = new textarea.Selection.Range(region.start.offset, region.end.offset+1)

  // this line is helpful for debugging: it shows the source regions in the sidekick tree
  // setShort(name + " [" + region.toString + "]")
}

/**
 * a node for URIs
 */
class JURIAsset(val path: Path, r: SourceRegion) extends JAsset(path.last, r) with MMTURIAsset {
  override protected val label: String = path.last
}

/** node for structural elements
 * @param elem the node in the MMT syntax tree
 */
class JElemAsset(val elem : StructuralElement, name: String, reg: SourceRegion) extends JAsset(name, reg) with MMTElemAsset {
   setLongDescription(path.toPath)  // tool tip
}

/** node for objects
 * @param term the node in the MMT syntax tree
 * @param parent the component containing the term
 * @param subobjectPosition the position in that term
 */
class JObjAsset(mmtplugin: MMTPlugin, val obj: Obj, val pragmatic: Obj, val context: Context, val parent: CPath, name: String, reg: SourceRegion)
  extends JAsset(name, reg) with MMTObjAsset {
  protected val controller = mmtplugin.controller
  private lazy val longDescription = mmtplugin.asString(obj)
  override def getLongString = longDescription
}

class JNotAsset(protected val owner: ContentPath, label: String, protected val not: TextNotation, reg: SourceRegion) extends JAsset(label, reg) with MMTNotAsset {
   setLongDescription(not.toText)
}

/** a dummy asset for structuring the tree */
class JAuxAsset(protected val label: String) extends enhanced.SourceAsset(label, -1, MyPosition(-1)) with MMTAuxAsset
