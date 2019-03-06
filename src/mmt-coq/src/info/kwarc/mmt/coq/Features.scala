package info.kwarc.mmt.coq

import info.kwarc.mmt.api.LocalName
import info.kwarc.mmt.api.checking.ExtendedCheckingEnvironment
import info.kwarc.mmt.api.modules.ModuleOrLink
import info.kwarc.mmt.api.notations.{LabelArg, LabelInfo}
import info.kwarc.mmt.api.symbols._

class Section extends StructuralFeature("Section") {
  def getHeaderNotation = List(LabelArg(1, LabelInfo.none))

  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = ???
  override def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration): Elaboration = new Elaboration {
    override def getO(name: LocalName): Option[Declaration] = {
      dd.getO(name).map {
        case c : Constant =>
          Constant(parent.toTerm,c.name,Nil,c.tp,c.df,c.rl) // TODO
      }
    }

    override def domain: List[LocalName] = dd.domain.filter {p =>
      dd.get(p) match {
        case c : Constant if c.rl contains "Variable" => false
        case c : Constant => true
        case _ => false
      }
    }
  }
}