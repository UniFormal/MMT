package info.kwarc.mmt.lf.induction

import info.kwarc.mmt.api._
import objects._
import symbols._
import notations._
import checking._
import modules._

import info.kwarc.mmt.lf._
//import scala.collection.parallel.ParIterableLike.Copy

sealed abstract class InductiveDecl {
  def name: LocalName
  def args: List[(Option[LocalName], Term)]
  def ret: Term
  def toTerm = FunType(args,ret)
}
case class TypeLevel(name: LocalName, args: List[(Option[LocalName], Term)]) extends InductiveDecl {
  def ret = Univ(1)
}
case class TermLevel(name: LocalName, args: List[(Option[LocalName], Term)], ret: Term) extends InductiveDecl

class InductiveTypes extends StructuralFeature("inductive") {
  def getHeaderNotation = List(LabelArg(1, LabelInfo.none))

  def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {}

  def injDecl(parent : DeclaredModule, d : TermLevel) : Declaration = {
    val e = d
    val (ds, es) = (d.toTerm, e.toTerm)
    val True = LFEquality(d.toTerm, d.toTerm)
    val p : List[(Term, Term)] = d.args.zip(e.args) map {case ((_ : Option[LocalName], x: Term), (_ : Option[LocalName], y: Term)) => (x,y)}
    val argsEqual = p.foldLeft[Term](True)({case (b :Term, (x : Term, y : Term)) => Lambda(LocalName("_"), LFEquality(x, y), b)})
    val body = Pi(LocalName("_"), LFEquality.apply(ds, es), argsEqual)
    val inj : Term= p.foldLeft[Term](body)({case (l, (a, b)) => Pi(LocalName("_"), Pi(LocalName("_"), l, b), a)})
    Constant(parent.toTerm, LocalName("_"), Nil, Some(inj), None, None)
  }
  
  def noConf(parent : DeclaredModule, d : TermLevel, tmdecls: List[TermLevel]) : List[Declaration] = {
    var resDecls = Nil
    
    
    List(Constant(parent.toTerm, d.name, Nil, Some(d.toTerm), None, None))
  }
  
  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) = {
    var tmdecls : List[TermLevel]= Nil
    val decls = dd.getDeclarations map {
      case c: Constant =>
        val tp = c.tp getOrElse {throw LocalError("missing type")}
          val FunType(args, ret) = tp
          ret match {
            case Univ(1) => TypeLevel(c.name, args)
            case Univ(x) if x != 1 => throw LocalError("unsupported universe")
            case r => {// TODO check that r is a type
              val tmdecl = TermLevel(c.name, args, r)
              tmdecls ::= tmdecl 
              tmdecl
            }
          }
      case _ => throw LocalError("illegal declaration")
    }
    var elabDecls: List[Declaration] = Nil
    decls foreach {d =>
      val tp = d.toTerm
      val c = Constant(parent.toTerm, d.name, Nil, Some(tp), None, None)
      elabDecls ::= c
      d match {
        case TermLevel(loc, args, tm) => {
          elabDecls ++= noConf(parent, TermLevel(loc, args, tm),tmdecls)
        }
        case _ => {}
      }
    }
    new Elaboration {
      def domain = elabDecls map {d => d.name}
      def getO(n: LocalName) = {
        elabDecls.find(_.name == n)
      }
    }
  }
}

object InductiveRule extends StructuralFeatureRule("inductive")
