package info.kwarc.mmt.api.symbols

import info.kwarc.mmt.api._
import modules._
import checking._
import info.kwarc.mmt.api.notations.{NotationContainer, TextNotation}
import info.kwarc.mmt.api.objects._

class NamedInductiveTypes extends StructuralFeature("inductive") with ParametricTheoryLike {

  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) = {
     new Elaboration {
       def domain = {
         dd.module.domain
       }
       def getO(name: LocalName) = {
         dd.module.getO(name) map {d =>
           d //TODO
         }
       }
     }
  }
}

/*
abstract class NamedInductiveTypes extends StructuralFeature("inductive") with ParametricTheoryLike {

  val tpsym : GlobalName
  val arrow : GlobalName

  protected object Arrow {
    val term = OMS(arrow)
    def apply(t1 : Term, t2 : Term) = OMA(term,List(t1,t2))
    def apply(in: List[Term], out: Term) = if (in.isEmpty) out else OMA(term, in ::: List(out))
    def unapply(t : Term) : Option[(Term,Term)] = t match {
      case OMA(`term`, hd :: tl) if tl.nonEmpty => Some((hd, apply(tl.init, tl.last)))
      case _ => None
    }
  }

  protected case class Type(c : Constant) {// (path : GlobalName, notation : Option[TextNotation]) {
    val path = c.path
    val notation = c.not
    def notC = c.notC
  }

  protected class Constructor(val c : Constant, val domains : List[Term], val target : Type) {
    val names = c.name :: c.alias
    val notation = c.not
    val notC = c.notC
  }

  protected class InductiveType(val dd : DerivedDeclaration) {
    val constants = dd.module.getDeclarations.collect {
      case c : Constant => c
    }

    object Constructor {
      def tmunapply(tm: Term): Option[(List[Term], Type)] = tm match {
        case Arrow(tpA, OMS(tpB)) if types.exists(_.path == tpB) =>
          val tp = types.find(_.path == tpB)
          tp.map(g => (List(tpA), g))
        case Arrow(tpA, subtm) =>
          val rec = tmunapply(subtm)
          if (rec.isDefined) Some((tpA :: rec.get._1, rec.get._2)) else None
        case OMS(tpA) if types.exists(_.path == tpA) =>
          val tp = types.find(_.path == tpA)
          tp.map(g => (Nil, g))
      }
      def unapply(c : Constant): Option[Constructor] = c.tp match {
        case Some(tp) => tmunapply(tp).map(tr => new Constructor(c,tr._1,tr._2))
        case _ => None
      }
    }

    val types = constants.collect{
      case c if c.tp.contains(OMS(tpsym)) => Type(c)
    }

    val constructors = dd.module.getDeclarations.collect {
      case Constructor(con) =>
        con
    }

    var typeconsts : List[FinalConstant] = Nil
    var constructorconsts : List[FinalConstant] = Nil
  }

  protected def doConstants(implicit dd : InductiveType)

  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) = {
     implicit val id = new InductiveType(dd)
     new Elaboration {
       def domain = {
         (id.typeconsts ::: id.constructorconsts).map(_.name)
       }
       def getO(name: LocalName) = {
         if (id.types.exists(t => t.path.name == name)) id.typeconsts.find(_.name == name)
         else id.constructorconsts.find(_.name == name)
       }
     }

  }
}
*/