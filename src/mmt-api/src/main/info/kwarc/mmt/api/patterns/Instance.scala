package info.kwarc.mmt.api.patterns

import info.kwarc.mmt.api._
import modules._
import objects._
import notations._
import symbols._
import utils._
import checking._
import info.kwarc.mmt.api.uom.ExtendedSimplificationEnvironment

class InstanceFeature extends StructuralFeature(Instance.feature) {


   /** a default notation in case the pattern is not known */
   def getHeaderNotation = List(LabelArg(1, LabelInfo.none), Delim(":"), SimpArg(2))

   override def processHeader(header: Term): (LocalName,Term) = {
     header match {
       case OMA(OMMOD(pat), OML(name, None, None,_,_) :: args) =>
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
     inst.tpC.get match {
       case Some(Instance.Type(p,args)) =>
         val patOpt = controller.globalLookup.getO(p)
         patOpt match {
           case Some(pat: DerivedDeclaration) if pat.feature == Pattern.feature =>
             Some((pat, args))
           case _ => throw LocalError("pattern not found: instance " + inst.path + " with pattern " + p)
         }
       case Some(_) => throw LocalError("instance has unexpexted type: " + inst.path)
       case None => throw LocalError("instance has no type: " + inst.path)
     }
   }

   def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = {
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

   def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration)(implicit env: Option[ExtendedSimplificationEnvironment] = None) = new Elaboration {
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
         case c: Constant if c.name == OMV.anonymous => Constant(c.home, c.name, c.alias, c.tpC, c.dfC, c.rl, dd.notC)
         case c: Constant if c.rl == Some("mainDecl")=> Constant(c.home, c.name, c.alias, c.tpC, c.dfC, c.rl, c.not map(NotationContainer(_)) getOrElse dd.notC)
         case _ => d
       }
       val subs = (params zip args) map {case (vd,a) => Sub(vd.name, a)}
       val tl = ApplySubs(subs) compose TraversingTranslator(Renamer.prefix(pattern.modulePath, dd.path))
       val dT = dN.translate(dd.home, dd.name, tl, Context.empty)
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
}
