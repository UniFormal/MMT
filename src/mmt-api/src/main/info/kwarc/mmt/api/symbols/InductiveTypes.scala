package info.kwarc.mmt.api.symbols

import info.kwarc.mmt.api._
import modules._
import info.kwarc.mmt.api.objects._


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

  protected case class constructorType(types : List[GlobalName]) {
    def tmunapply(tm: Term): Option[(List[Term], GlobalName)] = tm match {
      case Arrow(tpA, OMS(tpB)) if types contains tpB => Some((List(tpA), tpB))
      case Arrow(tpA, subtm) =>
        val rec = tmunapply(subtm)
        if (rec.isDefined) Some((tpA :: rec.get._1, rec.get._2)) else None
      case OMS(tpA) if types contains tpA => Some((Nil, tpA))
    }
    def unapply(c : Constant): Option[(List[Term], GlobalName)] = c.tp match {
      case Some(tp) => tmunapply(tp)
      case _ => None
    }
  }

  protected def doType(tpname : GlobalName) : FinalConstant

  protected def makeConstructor(tr : (List[LocalName],List[Term],GlobalName)) : FinalConstant

  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) = {
    val constants = dd.module.getDeclarations.collect {
      case c : Constant => c
    }
    val typenames = constants.collect{
      case c if c.tp.contains(OMS(tpsym)) => dd.parent ? c.name
    }
    val const = constructorType(typenames)
    val constructornames = dd.module.getDeclarations.collect {
      case c @ const(doms,cod) => (c.name :: c.alias,doms,cod)
    }

     new Elaboration {
       def domain = {
         dd.module.domain
       }
       lazy val types = typenames.view map doType
       lazy val constructors = constructornames.view map makeConstructor
       def getO(name: LocalName) = {
         if (typenames contains dd.parent ? name) types.find(_.name == name)
         else constructors.find(_.name == name)
       }
     }

  }
}

