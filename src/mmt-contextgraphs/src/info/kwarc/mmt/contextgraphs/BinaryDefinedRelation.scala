package info.kwarc.mmt.contextgraphs
import info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api.modules
import info.kwarc.mmt.api._
import objects._
import symbols._
import utils.xml.addAttrOrChild

import info.kwarc.mmt.api._
import modules._
import frontend._
import checking._
import uom.ElaboratedElement
import objects._
import notations._

import scala.xml.Elem
import Theory._
import info.kwarc.mmt.api.utils.MMT_TODO

/*class BinaryDefinedRelation(feature: String, p : DPath, n : LocalName, meta: Option[MPath], tpC: TermContainer, dfC : TermContainer, notC: NotationContainer, val fromC : TermContainer, val toC : TermContainer, val isImplicit : Boolean)
  extends DerivedModule(feature, p, n, meta, tpC, dfC, notC) with Link {

  def namePrefix = LocalName(path)
} */

class BinaryDefinedRelation extends ModuleLevelFeature("attack"){
  // def getHeaderNotation = Nil
  def getHeaderNotation = List(LabelArg(1,LabelInfo.none), Delim(":"), SimpArg(2), Delim("→"), SimpArg(3), Delim("with"), SimpArg(4))

  def getInnerContext(dd: DerivedModule) = {
    deconstructHeader(dd) match {
      case Some((f,t,_)) => Context(f) ++ Context(t)
      case _ => Context.empty
    }
  }


  private def deconstructHeader(dd:DerivedModule) : Option[(MPath,MPath,MPath)] = dd.tp match {
    case Some(OMA(OMMOD(`mpath`),List(OMMOD(dom),OMMOD(cod),OMMOD(wit)))) =>
      Some((dom,cod,wit))
    case _ => None
  }

  def check(dd: DerivedModule)(implicit env: ExtendedCheckingEnvironment): Unit = {
    deconstructHeader(dd) match {
      case Some((dom,cod,wit)) =>
      case _ => env.errorCont(ParseError("Not a valid head"))
    }
  }

  override def modules(dd: DerivedModule): List[Module] = deconstructHeader(dd) match {
    case Some((dom,cod,wit)) =>
      val declarations = dd.getDeclarations // dd.getConstants
      // ???
      val union = Theory(???,???,???)
      val constant1 = Constant(union.toTerm,???,Nil,???,???,None)
      union add constant1
      val view = View(???,???,???,???,false)
      List(union,view)
    case _ => Nil
  }
}
