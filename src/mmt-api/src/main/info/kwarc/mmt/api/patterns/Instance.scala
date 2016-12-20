package info.kwarc.mmt.api.patterns

import info.kwarc.mmt.api._
import modules._
import objects._
import notations._
import symbols._
import utils._
import checking._

class InstanceFeature extends StructuralFeature(Instance.feature) {
  
   /** a default notation in case the pattern is not known */
   def getHeaderNotation = List(LabelArg(1,false,false), Delim(":"), SimpArg(1))
  
   override def processHeader(header: Term): (LocalName,Term) = {
     header match {
       case OMA(OMMOD(pat), OML(name, None, None) :: args) =>
         val tp = OMA(OMMOD(pat), args)
         (name, tp)
     }
   }
   
   /** inverse of processHeader */
   override def makeHeader(dd: DerivedDeclaration): Term = {
     dd.tpC.get match {
       case Some(OMA(OMMOD(pat), args)) => OMA(OMMOD(pat), OML(dd.name, None, None) :: args)
     }
   }

   private def getPattern(inst: DerivedDeclaration): Option[(DerivedDeclaration,List[Term])] = {
     val (p,args) = Instance.getPattern(inst).getOrElse {return None}
     val patOpt = controller.globalLookup.getO(p)
     patOpt match {
       case Some(pat: DerivedDeclaration) if pat.feature == Pattern.feature =>
         Some((pat, args))
       case _ => None
     }
   }
  
   def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {
     val (pat, args) = getPattern(dd).getOrElse {
       env.errorCont(InvalidElement(dd, "no pattern found"))
       return
     }
     val params = Pattern.Type.getParameters(pat)
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
       throw InvalidElement(dd, "ill-formed instance")
     }
     private lazy val params = Pattern.Type.getParameters(pattern)
     lazy val domain = {
       pattern.getDeclarations.map {d =>
         dd.name / d.name
       }
     }
     def getO(n: LocalName): Option[Declaration] = {
       val d = pattern.module.getO(n.tail).getOrElse(return None)
       val dN = d match {
         case c : Constant if c.name == OMV.anonymous => Constant(c.home, c.name, c.alias, c.tpC, c.dfC, c.rl, dd.notC)
         case _ => d
       }
       val subs = (params zip args) map {case (vd,a) => Sub(vd.name, a)}
       val dT = dN.translate(dd.home, dd.name, ApplySubs(subs) compose Renamer.prefix(pattern.module.path, dd.path))
       Some(dT)
     }
   }
}

object Instance {
  val feature = "instance"

  object Type {
    def apply(pat: MPath, args: List[Term]) = OMA(OMMOD(pat), args)
    def unapply(t: Term) = t match {
       case OMA(OMMOD(pat), args) => Some(pat, args)
       case _ => None
    }
  }

  def apply(home : Term, name : LocalName, pattern: GlobalName, args: List[Term], notC: NotationContainer): DerivedDeclaration = {
    val patExp = Type(pattern.toMPath, args) 
    apply(home, name, TermContainer(patExp), notC)
  }
  
  def apply(home : Term, name : LocalName, tpC: TermContainer, notC: NotationContainer): DerivedDeclaration = {
    val dd = new DerivedDeclaration(home, name, feature, tpC, notC)
    dd
  }

  /** returns the pattern of an instance */
  def getPattern(dd: DerivedDeclaration) = {
     val tp = dd.tpC.get
     tp flatMap {
       case Type(p,args) => Some((p,args))
       case _ => None
     }
  }
}
