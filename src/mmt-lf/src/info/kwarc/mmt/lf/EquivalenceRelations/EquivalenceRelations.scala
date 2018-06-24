package info.kwarc.mmt.lf.EquivalenceRelations

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
  def tp : Term = {FunType(args, ret)}
  def toTerm = FunType(args,ret)
}
case class TypeLevel(name: LocalName, args: List[(Option[LocalName], Term)]) extends InductiveDecl {
  def ret = Univ(1)
}
case class TermLevel(name: LocalName, args: List[(Option[LocalName], Term)], ret: Term) extends InductiveDecl
case class StatementLevel(name: LocalName, args: List[(Option[LocalName], Term)]) extends InductiveDecl {
  def ret = Univ(1)
}

class InductiveTypes extends StructuralFeature("EquivalenceRelation") with ParametricTheoryLike {
  def isJudgment(tp: Term): Boolean = tp match {
      case FunType(_, ApplySpine(OMS(s),_)) =>
         //this can throw errors if the implicit graph is not fully loaded
         try {
            controller.globalLookup.getConstant(s).rl.contains("Judgment")
         } catch {case e: Error =>
            false
         }
      case _ => false
   }
  //def getHeaderNotation = List(LabelArg(1, LabelInfo.none))

  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {
    //TODO: check for inhabitability
  }

  def injDecl(parent : DeclaredModule, d : TermLevel) : Declaration = {
    val e : Declaration = VarDecl(LocalName("_"), None, Some(d.tp), Some(d.toTerm), None).toDeclaration(parent.toTerm)
    val FunType(eargs, eret) = e match {
      case c:Constant => c.tp getOrElse {throw LocalError("missing type")}
      case _ => throw LocalError("illegal internal declaration") 
    }
    val (ds, es) = (d.toTerm, e.toTerm)
    val True = LFEquality(d.toTerm, d.toTerm)
    val p : List[(Term, Term)] = d.args.zip(eargs) map {case ((_ : Option[LocalName], x: Term), (_ : Option[LocalName], y: Term)) => (x,y)}
    val argsEqual = p.foldLeft[Term](True)({case (b :Term, (x : Term, y : Term)) => Lambda(LocalName("_"), LFEquality(x, y), b)})
    val body = Pi(LocalName("_"), LFEquality.apply(ds, es), argsEqual)
    val inj : Term= p.foldLeft[Term](body)({case (l, (a, b)) => Pi(LocalName("_"), Pi(LocalName("_"), l, b), a)})
    Constant(parent.toTerm, LocalName("_"), Nil, Some(inj), None, None)
  }
  
  def noConf(parent : DeclaredModule, d : TermLevel, tmdecls: List[TermLevel]) : List[Declaration] = {
    var resDecls = Nil
    tmdecls map { 
      dec => 
      if (dec != d) {
        val (ds, es) = (d.toTerm, dec.toTerm)
        val (dargs, decargs) = (d.args map {case (_ : Option[LocalName], x: Term) => x}, dec.args map {case (_ : Option[LocalName], x: Term) => x})
        val False = LFEquality(d.toTerm, dec.toTerm)
        val body : Term = Pi(LocalName("_"), LFEquality.apply(ds, es), False)
        val quantifiedDec = decargs.foldLeft[Term](body)({(l, a) => Pi(LocalName("_"), a, l)})
        val noConf = dargs.foldLeft[Term](quantifiedDec)({(l, a) => Pi(LocalName("_"), a, l)})
        Constant(parent.toTerm, d.name, Nil, Some(noConf), None, None)
      }
      injDecl(parent, d)
    }
  }
  
  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) = {
    val args = dd.tpC.get match {
      case Some(OMA(_, args)) => args
    }
    val (tpArg, relArg) = args match {
        case x::List(y) => x match {
          case Univ(1) => (x,y)
        }
      case _ => throw LocalError("bad Arguments")
    }
    val relTp = OfType.unapply(relArg).getOrElse(throw LocalError("relation must be typed"))
    val l= LocalName("")
    val Lambda(_, Lambda(_, a, b), prop) = relTp
    if (a != b)
      throw LocalError("conflicting argument types: The second argument is not of type A -> A -> Bool")
    if (a != tpArg)
      throw LocalError("conflicting argument types: The second argument is not a relation on the type A specified by the first argument")
    val relDecl = VarDecl(LocalName(parent.name.toString()+relArg.toMPath.last), None, Some(tpArg), Some(relArg), None).toDeclaration(parent.toTerm)
    
    val True : Term = LFEquality(relArg, relArg)
    val (trans, refl, symm) = (True, True, True)     //TODO: Replace by something meaningful
    val predTrans = Lambda(l, relTp, trans)
    val predRefl = Lambda(l, relTp, refl)
    val predSymm = Lambda(l, relTp, symm)
    
    val dedPath : LocalName = LocalName("http://docs.omdoc.org/urtheories/primitive_types/bool.mmt#335.14.2:387.14.54")
    val ded = OMV(dedPath)
    val applyPred = {pred:Term => OMBIND(ded, Context.empty,OMBIND(pred, Context.empty, relArg))}
    val axiomToDecl = {ax:Term => VarDecl(LocalName(parent.name.toString()+ax.toString()), None, Some(Univ(1)),  Some(ax), None).toDeclaration(parent.toTerm)}
    val predToDecl = {pred:Term => axiomToDecl(applyPred(pred))}
    val elabDecls @ _::transDecl::reflDecl::List(symmDecl) = relDecl::(predTrans::predRefl::List(predSymm) map predToDecl)
    new Elaboration {
      def domain = elabDecls map {d => d.name}
      def getO(n: LocalName) = {
        elabDecls.find(_.name == n)
      }
    }
  }
}

object InductiveRule extends StructuralFeatureRule(classOf[InductiveTypes], "EquivalenceRelation")
