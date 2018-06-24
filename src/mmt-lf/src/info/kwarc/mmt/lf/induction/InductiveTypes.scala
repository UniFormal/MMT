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

class InductiveTypes extends StructuralFeature("inductive") with ParametricTheoryLike {
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
  
  def noJunk(parent : DeclaredModule, decls : List[InductiveDecl], tpdecls: List[TypeLevel]) : List[Declaration] = {
    //val quantifyModell = Constant(parent.toTerm, parent.name, Nil, Some(Univ(1)), None, None)
    val definedTypes = tpdecls map {x => x.ret} //TODO: find better heuristic
    val quantifiedTps = definedTypes map {tp => 
      val quantifiedTypeDecl = VarDecl(LocalName("_"), None, Some(Univ(1)), None, None)
      quantifiedTypeDecl.toTerm
    }
    val chainedDeclsTypeList = decls.map {dec => dec.tp}
    val (hd, tl) = (chainedDeclsTypeList.head,chainedDeclsTypeList.tail)
    val chainedDecls = tl.foldLeft[Term](hd)({(l, a) => Lambda(LocalName("_"), l, a)})
    val defTpsDecls = definedTypes map {tp:Term => VarDecl(LocalName(parent.name.toString()+tp.toString()), None, Some(Univ(1)),  Some(tp), None).name}
    val substPairs = defTpsDecls.zip(quantifiedTps)
    val substitutions = substPairs map {case (tpDec:LocalName, target:Term) =>Substitution(Sub(tpDec, target))}
    val (shd, stl) = (substitutions.head, substitutions.tail)
    val substitution = stl.foldLeft[Substitution](shd)({(l, a) => a++l})
    val body = chainedDecls.^(substitution)
    val prepostTpPairs = definedTypes.zip(quantifiedTps)
    
    val noJunks = prepostTpPairs map {case (preTp, postTp) => Lambda(LocalName("_"), preTp, Lambda(LocalName("_"), body, postTp))}
    noJunks map {t => Constant(parent.toTerm, parent.name, Nil, Some(t), None, None)}
  }
  
  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) = {
    var tmdecls : List[TermLevel]= Nil
    var tpdecls : List[TypeLevel]= Nil
    val indArgs = dd.tpC.get match {
      case Some(OMA(_, args)) => args
    }
    val tpArgs = indArgs.foldLeft(Nil:List[Term])({(l:List[Term], tm:Term) => tm match {
      case Univ(1) => tm::l
      case _ => l
      }})
    var elabDecls:List[Declaration] = tpArgs map {tp:Term => VarDecl(LocalName(parent.name.toString()+tp.toString()), None, Some(Univ(1)),  Some(tp), None).toDeclaration(parent.toTerm)}
    tpdecls = elabDecls.zip(tpArgs) map {case (d, tp) => 
      val FunType(args, ret) = tp
      TypeLevel(d.name, args)
    }
    val decls = tpdecls ++ dd.getDeclarations map {
      case c: Constant =>
        val tp = c.tp getOrElse {throw LocalError("missing type")}        
          val FunType(args, ret) = tp
          if (isJudgment(tp)) {
            StatementLevel(c.name, args)
          } else {
          ret match {
            case Univ(1) => {
              val tpdecl = TypeLevel(c.name, args)
              tpdecls ::= tpdecl 
              tpdecl
            }
            case Univ(x) if x != 1 => throw LocalError("unsupported universe")
            case r => {// TODO check that r is a type
              val tmdecl = TermLevel(c.name, args, r)
              tmdecls ::= tmdecl 
              tmdecl
            }
          }
        }
      case _ => throw LocalError("illegal declaration")
    } 
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
    elabDecls ++= noJunk(parent, decls, tpdecls)
    new Elaboration {
      def domain = elabDecls map {d => d.name}
      def getO(n: LocalName) = {
        elabDecls.find(_.name == n)
      }
    }
  }
}

object InductiveRule extends StructuralFeatureRule(classOf[InductiveTypes], "inductive")
