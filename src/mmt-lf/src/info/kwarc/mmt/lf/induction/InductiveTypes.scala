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
  
  private def uniqueLN(nm:Option[String]) : LocalName = {
    val con : Context = Context(this.mpath)
    var name = LocalName(nm.getOrElse(""))
    Context.pickFresh(con, name) match {
      case (n, s) => s match {
        case (_ / fr) => fr
      }
    }
  }
  
 def newVar(name:Option[String], tp:Option[Term], df: Option[Term], con: Option[Context] = None) : OML = {
    val c = con.getOrElse(Context.empty)
    val n = name.getOrElse("newVar")
    val tm : Term = Context.pickFresh(c, LocalName(n)) match {case (nm:LocalName, s:Substitution) => s.apply(nm).get}
    val vr = tm match {
      case x:OMV => x
      case _ => throw ImplementationError("Unexpected result: Context.pickFresh doesn't generate an OMV")
    }
    OML(vr.name, tp, df, None, None)
  }
  
  private def PI(a:Term, b:Term) = {
    a match {
      case OML(name, _, Some(df), _, _) => Pi(name, df, b)
      case t @ OMV(name) => Pi(name, t, b)
      case _ => {
        val aName = uniqueLN(Some("term_at_"+a.toString()))
        val aOML = OML(OMV(aName).name, Some(Univ(1)), Some(a), None, None)
        Pi(aOML.name, aOML, b)
      }
    }
     
  }     

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
      case (None, arg) => (uniqueLN(Some("name_for_argument"+arg.toString()+"of_term_"+d.name)), arg)
    }
    val dargsHd = d.args.head match {case (_, arg) => arg}
    val argTp = FunTerm(dargs.tail , dargsHd)
    val aArgs = dargs map {case (loc, tp) => newVar(Some(loc.toString()+"1"), Some(tp), None, None)}
    val bArgs = dargs map {case (loc, tp) => newVar(Some(loc.toString()+"2"), Some(tp), None, None)}
    val ded = OMV(LocalName("http://mathhub.info/MitM/Foundation?Logic?ded"))
    def DED(x:Term) = OMA(ded, List(x))
    val and = OMV(LocalName("http://mathhub.info/MitM/Foundation?Logic?and"))
    def AND(x:Term, y:Term) = OMA(and, List(x, y))
    
    // TODO: Replace this hack by something more reasonable
    val True = OMV(LocalName("http://mathhub.info/MMT/urtheories?DHOL?TRUE"))
    val argPairsTl = aArgs.zip(bArgs).tail
    val argEq = argPairsTl.foldLeft(LFEquality(aArgs.head, bArgs.head))({case (sofar:Term, (a:Term, b:Term)) => AND(LFEquality(a,b),sofar)})
    val body:Term = Arrow(DED(LFEquality(OMA(d.toTerm, aArgs), OMA(d.toTerm, bArgs))), DED(argEq))
    val inj = (aArgs++bArgs).foldLeft(body)({case (arg:Term, tm:Term) => PI(arg, body)})
    Constant(parent.toTerm, uniqueLN(Some("injectivity_rule_for"+inj.toString())), Nil, Some(inj), None, None)
  }
  
  def dontConfuse(parent : DeclaredModule, d : TermLevel, e : TermLevel) : Declaration = {
    // Would be a much nicer implementation, but still not quite working
    /* d.args match {
      case Nil => {
        e.args match {
          case Nil => {
            val False = OMV(LocalName("http://mathhub.info/MMT/urtheories?DHOL?FALSE"))
            val ded = OMV(LocalName("http://mathhub.info/MitM/Foundation?Logic?ded"))
            def DED(x:Term) = OMA(ded, List(x))
            // TODO: Replace this hack by something more reasonable
            val body:Term = Arrow(LFEquality(d.toTerm, e.toTerm), False)
            Constant(parent.toTerm, uniqueLN(Some("no_conf_axiom_for_"+d.name.toString()+"_"+e.name.toString())), Nil, Some(body), None, None)
          }
          case hd::tl => {
            val (argNm, tp) = hd match {
              case (op : Option[LocalName], tp:Term) => (uniqueLN(Some(op.getOrElse("").toString())).toString(), tp)
            }
            val quant = newVar(Some(argNm), Some(tp), None, None)
            val eName = newVar(None, None, Some(OMA(e.toTerm, List(quant))), None).name
            PI(quant, dontConfuse(parent, d, TermLevel.apply(eName, tl.m, e.ret)))
          }
        }
      }
      case hd::tl => {
        val (argNm, tp) = hd match {
          case (op : Option[LocalName], tp:Term) => (uniqueLN(Some(op.getOrElse("").toString())).toString(), tp)
        }
        val quant = newVar(Some(argNm), Some(tp), None, None)
        PI(quant, dontConfuse(parent, OMA(d.toTerm, List(quant)), e))
      }
    }
  } */
  val dargs : List[(LocalName, Term)]= d.args map {
      case (Some(loc), arg) => (loc, arg)
      case (None, arg) => (uniqueLN(Some("name_for_argument"+arg.toString()+"of_term"+d.name)), arg)
    }
    var dArgs : List[Term]= Nil
    if (dargs.length >0) {
      val dargsHd = d.args.head match {case (_, arg) => arg}      
      val dargTp = FunTerm(dargs.tail , dargsHd)
      dArgs = dargs map {case (loc, tp) => newVar(Some(loc.toString()+"quantified"), Some(tp), None, None)}
    }
    val eargs : List[(LocalName, Term)]= e.args map {
      case (Some(loc), arg) => (loc, arg)
      case (None, arg) => (uniqueLN(Some("name_for_argument"+arg.toString()+"of_term"+e.name)), arg)
    }
        
    var eArgs : List[Term] = Nil
    if (eargs.length >0) {
      val eargsHd = e.args.head match {case (_, arg) => arg}
      val eargTp = FunTerm(eargs.tail , eargsHd)
      eArgs = eargs map {case (loc, tp) => newVar(Some(loc.toString()+"quantified"), Some(tp), None, None)}
    }

    val False = OMV(LocalName("http://mathhub.info/MMT/urtheories?DHOL?FALSE"))
    val ded = OMV(LocalName("http://mathhub.info/MitM/Foundation?Logic?ded"))
    def DED(x:Term) = OMA(ded, List(x))
    // TODO: Replace this hack by something more reasonable
    val body:Term = Arrow(LFEquality(d.toTerm, e.toTerm), False)
            
    if ((dArgs++eArgs).length > 0) {
      val (hd, tl) = ((dArgs++eArgs).head, (dArgs++eArgs).tail)
      val noConf = (dArgs++eArgs).foldLeft[Term](body)({(a:Term, b:Term)=> (a, b) match {
        case (arg:Term, tm:Term) => PI(arg, body)
        case x => throw ImplementationError("/pattern matching error in line 111 of InductiveTypes.scala: Encountered "+x.toString()+" instead of a tuple (Term, Term)")}
      })
      Constant(parent.toTerm, uniqueLN(Some("no_conf_axiom_for_"+d.name.toString()+"_"+e.name.toString())), Nil, Some(noConf), None, None)
    } else {
      Constant(parent.toTerm, uniqueLN(Some("no_conf_axiom_for_"+d.name.toString()+"_"+e.name.toString())), Nil, Some(body), None, None)
    }
}
  
  def noConf(parent : DeclaredModule, d : TermLevel, tmdecls: List[TermLevel]) : List[Declaration] = {
    var resDecls = Nil
    var decls:List[Declaration] = Nil
    tmdecls foreach { 
      e => 
      if (e == d) {
        if(d.args.length > 0)
          decls::=injDecl(parent, d)  
      } else {
          decls::=dontConfuse(parent, d, e)
      }
    }
    decls
  }
  
  def noJunk(parent : DeclaredModule, decls : List[InductiveDecl], tpdecls: List[TypeLevel], dd: DerivedDeclaration) : List[Declaration] = {
    //val quantifyModell = Constant(parent.toTerm, parent.name, Nil, Some(Univ(1)), None, None)
    val definedTypes = tpdecls map {x => x.ret} //TODO: find better heuristic
    val quantifiedTps = definedTypes map {tp => 
      val quantifiedTypeDecl = VarDecl(uniqueLN(Some("quantifying_type_substituting_"+tp.toString())), None, Some(Univ(1)), None, None)
      quantifiedTypeDecl.toTerm
    }
    val chainedDeclsTypeList = decls.map {dec => dec.tp}
    val (hd, tl) = (chainedDeclsTypeList.head,chainedDeclsTypeList.tail)
    val chainedDecls = tl.foldLeft[Term](hd)({(l, a) => Arrow(l, a)})
    val defTpsDecls = definedTypes map {tp:Term => newVar(Some("free_var_of_type_"+tp.toString()), Some(Univ(1)), None, None).vd}
    val substPairs = defTpsDecls.zip(quantifiedTps)
    val substitutions = substPairs map {case (tpDec:VarDecl, target:Term) =>Substitution(Sub(tpDec.name, target))}
    val (shd, stl) = (substitutions.head, substitutions.tail)
    val substitution = stl.foldLeft[Substitution](shd)({(l, a) => a++l})
    val body = chainedDecls.^(substitution)
    val prepostTpPairs = definedTypes.zip(quantifiedTps)
    
    val noJunks = prepostTpPairs map {case (preTp, postTp) => Arrow(preTp, Arrow(body, postTp))}
    noJunks map {t => Constant(parent.toTerm, uniqueLN(Some("no_junk_axiom_for_"+dd.name.toString())), Nil, Some(t), None, None)}
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
    elabDecls ++= noJunk(parent, decls, tpdecls, dd)
    new Elaboration {
      def domain = elabDecls map {d => d.name}
      def getO(n: LocalName) = {
        elabDecls.find(_.name == n)
      }
    }
  }
}

object InductiveRule extends StructuralFeatureRule(classOf[InductiveTypes], "inductive")
