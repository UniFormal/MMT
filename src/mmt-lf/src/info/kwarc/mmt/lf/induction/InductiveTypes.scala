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

class InductiveTypes extends StructuralFeature("inductive") {// with ParametricTheoryLike {
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
  def getHeaderNotation = List(LabelArg(1, LabelInfo.none))

  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {
    //TODO: check for inhabitability
    if (dd.getDeclarations == 0) {
      throw LocalError("no declarations")
    }
  }

  def injDecl(parent : DeclaredModule, d : TermLevel) : Declaration = {
    if (d.args.length <= 0) 
      throw ImplementationError("Trying to assert injectivity of a constant function")
    val dargs : List[(LocalName, Term)]= d.args map {
      case (Some(loc), arg) => (loc, arg)
      case (None, arg) => (LocalName("NameForArgument"+arg.toString()+"OfTerm"+d.name), arg)
    }
    val dargsHd = d.args.head match {case (_, arg) => arg}
    val argTp = FunTerm(dargs.tail , dargsHd)
    val aArgs = dargs map {case (loc, tp) => OML(OMV(parent.toString()+"quantifiedVar1ForArgument"+loc.toString()).name, Some(tp), None, None, None)}
    val bArgs = dargs map {case (loc, tp) => OML(OMV(parent.toString()+"quantifiedVar2ForArgument"+loc.toString()).name, Some(tp), None, None, None)}
    val ded = OMV(LocalName("http://mathhub.info/MitM/Foundation?Logic?ded"))
    def DED(x:Term) = OMA(ded, List(x))
    val andPath : LocalName = LocalName("http://mathhub.info/MitM/Foundation?Logic?and")
    val and = OMV(andPath)
    def AND(x:Term, y:Term) = OMA(and, List(x, y))
    
    // TODO: Replace this hack by something more reasonable
    val True = LFEquality(d.toTerm, d.toTerm)
    val argPairsTl = aArgs.zip(bArgs).tail
    val argEq = argPairsTl.foldLeft(LFEquality(aArgs.head, bArgs.head))({case (sofar:Term, (a:Term, b:Term)) => AND(LFEquality(a,b),sofar)})
    val body:Term = Arrow(DED(LFEquality(OMA(d.toTerm, aArgs), OMA(d.toTerm, bArgs))), DED(argEq))
    val inj = (aArgs++bArgs).foldLeft(body)({case (arg:OML, tm:Term) => Pi(arg.name, arg.tp.get, body)})
    Constant(parent.toTerm, LocalName("injectivityRuleFor"+inj.toString()), Nil, Some(inj), None, None)
  }
  
  def noConf(parent : DeclaredModule, d : TermLevel, tmdecls: List[TermLevel]) : List[Declaration] = {
    var resDecls = Nil
    var decls:List[Declaration] = Nil
    tmdecls foreach { 
      e => 
      if (e != d) {
        val dargs : List[(LocalName, Term)]= d.args map {
          case (Some(loc), arg) => (loc, arg)
          case (None, arg) => (LocalName("NameForArgument"+arg.toString()+"OfTerm"+d.name), arg)
        }
        var dArgs : List[OML]= Nil
        if (dargs.length >0) {
          val dargsHd = d.args.head match {case (_, arg) => arg}      
          val dargTp = FunTerm(dargs.tail , dargsHd)
          dArgs = dargs map {case (loc, tp) => OML(OMV(parent.toString()+"quantifiedVar1ForArgument"+loc.toString()).name, Some(tp), None, None, None)}
        }
        val eargs : List[(LocalName, Term)]= e.args map {
          case (Some(loc), arg) => (loc, arg)
          case (None, arg) => (LocalName("NameForArgument"+arg.toString()+"OfTerm"+e.name), arg)
        }
        
        var eArgs : List[OML] = Nil
        if (eargs.length >0) {
          val eargsHd = e.args.head match {case (_, arg) => arg}
          val eargTp = FunTerm(eargs.tail , eargsHd)
          eArgs = eargs map {case (loc, tp) => OML(OMV(parent.toString()+"quantifiedVar1ForArgument"+loc.toString()).name, Some(tp), None, None, None)}
        }
                
        val False = LFEquality(d.toTerm, e.toTerm)
        val ded = OMV(LocalName("http://mathhub.info/MitM/Foundation?Logic?ded"))
        def DED(x:Term) = OMA(ded, List(x))
        // TODO: Replace this hack by something more reasonable
        val body:Term = Arrow(LFEquality(OMA(d.toTerm, dArgs), OMA(e.toTerm, eArgs)), False)
        if ((dArgs++eArgs).length > 0) {
          val (hd, tl) = ((dArgs++eArgs).head, (dArgs++eArgs).tail)
          val noConf = (dArgs++eArgs).foldLeft(body)({case (arg:OML, tm:Term) => Pi(arg.name, arg.tp.get, body)})
          decls ::= Constant(parent.toTerm, d.name, Nil, Some(noConf), None, None)
        } else {
          decls ::= Constant(parent.toTerm, d.name, Nil, Some(Arrow(DED(LFEquality(d.toTerm, e.toTerm)),False)), None, None)
        }
      }
      if(d.args.length > 0)
        decls::=injDecl(parent, d)
    }
    decls
  }
  
  def noJunk(parent : DeclaredModule, decls : List[InductiveDecl], tpdecls: List[TypeLevel]) : List[Declaration] = {
    //val quantifyModell = Constant(parent.toTerm, parent.name, Nil, Some(Univ(1)), None, None)
    val definedTypes = tpdecls map {x => x.ret} //TODO: find better heuristic
    val quantifiedTps = definedTypes map {tp => 
      val quantifiedTypeDecl = VarDecl(LocalName("QuantifyingTypeMappedOf"+tp.toString()), None, Some(Univ(1)), None, None)
      quantifiedTypeDecl.toTerm
    }
    val chainedDeclsTypeList = decls.map {dec => dec.tp}
    val (hd, tl) = (chainedDeclsTypeList.head,chainedDeclsTypeList.tail)
    val chainedDecls = tl.foldLeft[Term](hd)({(l, a) => Arrow(l, a)})
    val defTpsDecls = definedTypes map {tp:Term => VarDecl(LocalName(parent.name.toString()+tp.toString()), None, Some(Univ(1)),  Some(tp), None).name}
    val substPairs = defTpsDecls.zip(quantifiedTps)
    val substitutions = substPairs map {case (tpDec:LocalName, target:Term) =>Substitution(Sub(tpDec, target))}
    val (shd, stl) = (substitutions.head, substitutions.tail)
    val substitution = stl.foldLeft[Substitution](shd)({(l, a) => a++l})
    val body = chainedDecls.^(substitution)
    val prepostTpPairs = definedTypes.zip(quantifiedTps)
    
    val noJunks = prepostTpPairs map {case (preTp, postTp) => Arrow(preTp, Arrow(body, postTp))}
    noJunks map {t => Constant(parent.toTerm, parent.name, Nil, Some(t), None, None)}
  }
  
  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) = {
    var tmdecls : List[TermLevel]= Nil
    var tpdecls : List[TypeLevel]= Nil
    /* val indArgs = dd.tpC.get match {
      case Some(OMA(_, args)) => args
    }
    val tpArgs = indArgs.foldLeft(Nil:List[Term])({(l:List[Term], tm:Term) => tm match {
      case Univ(1) => tm::l
      case _ => l
      }}) 
    val tpArgs = Nil
    var elabDecls:List[Declaration] = tpArgs map {tp:Term => VarDecl(LocalName(parent.name.toString()+tp.toString()), None, Some(Univ(1)),  Some(tp), None).toDeclaration(parent.toTerm)}
    tpdecls = elabDecls.zip(tpArgs) map {case (d, tp) => 
      val FunType(args, ret) = tp
      TypeLevel(d.name, args)
    } */
    var elabDecls : List[Declaration] = Nil
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
