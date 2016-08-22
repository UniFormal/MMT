package info.kwarc.mmt.jedit

import sidekick._

import info.kwarc.mmt.api._
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
sealed abstract class MMTAsset(name: String, val region: SourceRegion)
  extends enhanced.SourceAsset(name, region.start.line, MyPosition(region.start.offset)) {
  setEnd(MyPosition(region.end.offset+1))
  /** the base URIto use in the context of this asset */
  def getScope : Option[MPath]
  
  // this line is helpful for debugging: it shows the source regions in the sidekick tree
  // setShort(name + " [" + region.toString + "]")
}

/**
 * a node for URIs
 */
class MMTURIAsset(val path: Path, r: SourceRegion) extends MMTAsset(path.last, r) {
  def getScope = path match {
    case p: MPath => Some(p)
    case _ => None
  }
}

/** node for structural elements
 * @param elem the node in the MMT syntax tree
 */ 
class MMTElemAsset(val elem : StructuralElement, name: String, reg: SourceRegion) extends MMTAsset(name, reg) {
   //note: shortDescription == name, shown in tree
   setLongDescription(path.toPath)  // tool tip
   //setIcon
   def path = elem.path
   def getScope : Option[MPath] = elem match {
      case _ : NarrativeElement => None
      case c : ContentElement => c match {
        case t: DeclaredTheory => Some(t.path)
        case v: modules.View => None //would have to be parsed to be available
        case d: Declaration => Some(d.path.module)
        case _ => None
      }
   }
}

/** node for objects
 * @param term the node in the MMT syntax tree
 * @param parent the component containing the term
 * @param subobjectPosition the position in that term
 */ 
class MMTObjAsset(val obj: Obj, val pragmatic: Obj, val context: Context, val parent: CPath, name: String, reg: SourceRegion) extends MMTAsset(name, reg) {
  obj.head map {case p =>
    setLongDescription(p.toPath)
  }
  def getScope = parent.parent match {
     case cp: ContentPath => Some(cp.module)
     case _ => None
  }
}

class MMTNotAsset(owner: ContentPath, label: String, not: TextNotation, reg: SourceRegion) extends MMTAsset(label, reg) {
   def getScope = Some(owner.module)
}