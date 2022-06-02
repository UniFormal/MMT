package info.kwarc.mmt.jedit.proofgui.util

import info.kwarc.mmt.api.ComponentKey
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.jedit.{JElemAsset, MMTPlugin}
import info.kwarc.mmt.lf.itp.{InteractiveProof, ProofGenerator}
import org.gjt.sp.jedit.textarea.JEditTextArea
import org.gjt.sp.jedit.{View, jEdit}
import sidekick.SideKickParsedData

import javax.swing.tree.{DefaultMutableTreeNode, TreePath}

/**
  * object for creating a proof manager ([[InteractiveProof]])
  */
object GenInteractiveProof {

  /**
    * a function that searches for the `type` of a declaration
    * @param arr the array representation of the source path in the tree representation the caret is currently on
    * @return the `type`. None, if no type exists
    */
  def findConstant(arr : Array[DefaultMutableTreeNode]): Option[DefaultMutableTreeNode] = arr match {
    case Array() => None
    case Array(x , xs @ _* ) =>{
      x.getUserObject match {
        case y: JElemAsset => y.elem match {
          case _ : FinalConstant => Some(x)
          case _ => findConstant(arr.tail)
        }
        case _ => findConstant(arr.tail)
      }
    }
  }

  /** generates a new proof manager ([[InteractiveProof]]).
    * This will be done by looking at the mmt source interpreted as a tree of tokens. The created proof sould be the one the caret indicates.
    * The path the caret is on will have a type wich in turn will be the goal for the created proof.
    *
    * @param v the jedit view
    * @return a new [[InteractiveProof]]-instance, None if no provable object was encountered near the caret position
    */
  def genInteractiveProof(v : View ): Option[InteractiveProof] ={
    // get the tree representation for the source in the textare of the current jedit view
    val pd : SideKickParsedData = SideKickParsedData.getParsedData(v)
    val ta : JEditTextArea = v.getTextArea
    val caretpos : Int = ta.getCaretPosition
    // the path from the root of the source tree to where the caret currently is
    val tmp : TreePath = pd.getTreePathForPosition(caretpos)
    // explicitely type the TreePath as Array[DefaultMutableTreeNode] otherwise (because scala can't figure it out on its own)
    val pth : Array[DefaultMutableTreeNode] = tmp.getPath.map(x => x.asInstanceOf[DefaultMutableTreeNode])
    // find the type
    val pth0 = findConstant(pth)
    val (tp,mp,cp)  = pth0 match {
      case None => return None
      case Some(v) => {
        val cst = v.getUserObject.asInstanceOf[JElemAsset].elem.asInstanceOf[FinalConstant]
        val tp = cst.tp.getOrElse(return None)
        // get the mpath the type belongs to
        val mp = cst.home.toMPath
        val k : ComponentKey = ComponentKey.parse("type")
        // get the componet path the type belongs to
        val cp = Some(mp ? cst.name $ k)
        (tp,mp,cp)

      }
    }

    val mmtp : MMTPlugin =  jEdit.getPlugin("info.kwarc.mmt.jedit.MMTPlugin", true).asInstanceOf[MMTPlugin]
    val c : Controller = mmtp.controller
    // generate a new InteractiveProof with the data gathered
    ProofGenerator.setupNewProof(c , Some(mp), cp , tp)

  }



}