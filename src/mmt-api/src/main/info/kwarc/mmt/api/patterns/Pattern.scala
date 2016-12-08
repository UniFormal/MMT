package info.kwarc.mmt.api.patterns

import info.kwarc.mmt.api._
import modules._
import objects._
import notations._
import symbols._
import utils._
import checking._


class PatternFeature extends StructuralFeature(Pattern.feature) {

   override def getInnerContext(d: DerivedDeclaration) = Pattern.getParameters(d)
  
   def check(d: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {}
   
   def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) = new Elaboration {
     def domain = Nil
     def getO(n: LocalName) = None
   }

   def modules(d: DerivedDeclaration) = Nil
}

object Pattern {
  val feature = "pattern"
  
  def apply(home: Term, name : LocalName, params: Context, body : Context, notC: NotationContainer) = {
    val comps = DeclarationComponent(ParamsComponent, ContextContainer(params)) :: notC.getComponents
    val dd = new DerivedDeclaration(home, name, "pattern", Nil)
    body.mapVarDecls {case (con, vd) =>
      val c = vd.toConstant(home.toMPath, con)
      dd.module.add(c)
    }
    dd
  }
  
  def unapply(dd: DerivedDeclaration): Option[(Term, LocalName, Context, DeclaredTheory)] = {
    if (dd.feature != feature) return None
    val pars = getParameters(dd)
    val thy = dd.module match {
      case thy: DeclaredTheory => thy
      case _ => throw ImplementationError("pattern must contain theory")
    }
    Some((dd.home,dd.name, pars, thy)) 
  }
  
  /** pre: d.feature == "pattern" */
  def getParameters(d: DerivedDeclaration) = {
     d.getComponent(ParamsComponent).flatMap {
       case cc: ContextContainer => cc.get
       case _ => None
     } getOrElse Context.empty
   }
}

class InstanceFeature extends StructuralFeature(Instance.feature) {
  
   private def getPattern(dd: DerivedDeclaration): Option[(DerivedDeclaration,List[Term])] = {
     val (p,args) = Instance.getPattern(dd).getOrElse {return None}
     val patOpt = controller.globalLookup.getO(p)
     patOpt match {
       case Some(pat: DerivedDeclaration) if pat.feature == "pattern" =>
         Some((pat, args))
       case _ => None
     }
   }
  
   def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {
     val (pat, args) = getPattern(dd).getOrElse {
       env.errorCont(InvalidElement(dd, "no pattern found"))
       return
     }
     val params = Pattern.getParameters(pat)
     if (params.length != args.length) {
       env.errorCont(InvalidElement(dd, "wrong number of arguments for pattern found"))
       return
     }
     /* //TODO check judgment Substitution(args : params); args may contain unknowns that need to be solved
       (params zip args).foreach {case (vd,arg) =>
        vd.tp.foreach {tp =>
          val cu = CheckingUnit(None, Context(dd.parent), Context.empty, Typing(Stack(Context.empty), arg, tp))
          env.objectChecker(cu, env.rules)(env.ce)
        }
     }*/
   }
   
   def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) = new Elaboration {
     private lazy val (pattern,args) = getPattern(dd).getOrElse {
       throw ImplementationError("elaborating ill-formed instance")
     }
     private lazy val params = Pattern.getParameters(pattern)
     lazy val domain = {
       pattern.getDeclarations.map {d =>
         dd.name / d.name
       }
     }
     def getO(n: LocalName): Option[Declaration] = {
       val d = pattern.module.getO(n).getOrElse(return None)
       val subs = (params zip args) map {case (vd,a) => Sub(vd.name, a)}
       val dT = d.translate(dd.home, dd.name, ApplySubs(subs) compose Renamer.prefix(pattern.module.path, dd.path))
       Some(dT)
     }
   }

   def modules(d: DerivedDeclaration) = Nil
}

object Instance {
  val feature = "instance"
  def apply(home : Term, name : LocalName, pattern: GlobalName, args: List[Term], notC: NotationContainer) = {
    val patExp = OMA(OMS(pattern), args) 
    val comps = DeclarationComponent(TypeComponent, TermContainer(patExp)) :: notC.getComponents
    val dd = new DerivedDeclaration(home, name, feature, comps)
    dd
  }
  
  /** returns the pattern of an instance */
  def getPattern(dd: DerivedDeclaration) = {
     val tp = dd.getComponent(TypeComponent).flatMap {
       case tc: AbstractTermContainer => tc.get
       case _ => None
     }
     tp flatMap {
       case OMA(OMS(p), args) => Some((p,args))
       case _ => None
     }
  }
}
