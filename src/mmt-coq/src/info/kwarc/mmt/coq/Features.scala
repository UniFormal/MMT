package info.kwarc.mmt.coq

import info.kwarc.mmt.api.LocalName
import info.kwarc.mmt.api.checking.ExtendedCheckingEnvironment
import info.kwarc.mmt.api.modules.ModuleOrLink
import info.kwarc.mmt.api.notations.{LabelArg, LabelInfo}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.lf.{Lambda, Pi}

class Inductive extends StructuralFeature("Inductive") {
  def getHeaderNotation = List(LabelArg(1, LabelInfo.none))
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = ???
  override def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration): Elaboration = new Elaboration {
    override def getO(name: LocalName): Option[Declaration] = dd.getO(name) match {
      case Some(c: Constant) => Some(Constant(parent.toTerm,c.name,c.alias,c.tp,None,c.rl))
      case None => None
    }

    override def domain: List[LocalName] = dd.domain
  }
}

class CoInductive extends StructuralFeature("CoInductive") {
  def getHeaderNotation = List(LabelArg(1, LabelInfo.none))
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = ???
  override def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration): Elaboration = new Elaboration {
    override def getO(name: LocalName): Option[Declaration] = dd.getO(name) match {
      case Some(c: Constant) => Some(Constant(parent.toTerm,c.name,Nil,c.tp,None,c.rl))
      case None => None
    }

    override def domain: List[LocalName] = dd.domain
  }
}

class Section extends StructuralFeature("Section") {
  def getHeaderNotation = List(LabelArg(1, LabelInfo.none))

  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = ???
  override def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration): Elaboration = new Elaboration {
    override def getO(name: LocalName): Option[Declaration] = {
      dd.getO(name).map {
        case c : Constant =>
          val con = variables.map(p => VarDecl(p._1,None,p._2,None,None))
          val ntp = c.tp.map(Pi(con,_))
          val ndf = c.df.map(Lambda(con,_))
          Constant(parent.toTerm,c.name,Nil,ntp,ndf,c.rl)
      }
    }

    private lazy val variables = names.map { p =>
      (p.name,dd.get(p.name).asInstanceOf[Constant].tp.map(Replace.apply(_,())))
    }

    private lazy val names = {
      dd.getDeclarations collect {
        case c : Constant if c.rl contains "Variable" =>
          c.path
      }
    }

    private object Replace extends StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
        case OMS(p) if names contains p => OMV(p.name)
        case _ => Traverser(this,t)
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