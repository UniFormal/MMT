package info.kwarc.mmt.api.symbols

import info.kwarc.mmt.api._
import modules._
import checking._
import info.kwarc.mmt.api.objects.{Context, VarDecl}

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

  protected class InductiveType(dd : DerivedDeclaration) {
    val constants = dd.module.getDeclarations.collect {
      case c : Constant => c
    }
    val typenames = constants.collect{
      case c if c.tp.contains(OMS(tpsym)) => dd.parent ? c.name
    }

    object constructor {
      def tmunapply(tm: Term): Option[(List[Term], GlobalName)] = tm match {
        case Arrow(tpA, OMS(tpB)) if typenames contains tpB => Some((List(tpA), tpB))
        case Arrow(tpA, subtm) =>
          val rec = tmunapply(subtm)
          if (rec.isDefined) Some((tpA :: rec.get._1, rec.get._2)) else None
        case OMS(tpA) if typenames contains tpA => Some((Nil, tpA))
      }
      def unapply(c : Constant): Option[(List[Term], GlobalName)] = c.tp match {
        case Some(tp) => tmunapply(tp)
        case _ => None
      }
    }

    val constructornames = dd.module.getDeclarations.collect {
      case c @ constructor(doms,cod) => (c.name :: c.alias,doms,cod)
    }
  }

  protected def doType(tpname : LocalName)(implicit dd : InductiveType) : FinalConstant

  protected def doConstructor(tpname : LocalName)(implicit dd : InductiveType) : FinalConstant

  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) = {
     implicit val id = new InductiveType(dd)
     new Elaboration {
       def domain = {
         dd.module.domain
       }
       def getO(name: LocalName) = {
         if (id.typenames contains dd.parent ? name) Some(doType(name))
         else if (id.constructornames exists (t => t._1 contains name)) Some(doConstructor(name))
         else None
       }
     }

  }
}


 */