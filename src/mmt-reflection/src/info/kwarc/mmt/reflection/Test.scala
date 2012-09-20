package info.kwarc.mmt.reflection

import info.kwarc.mmt.api._
import frontend.Controller
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.lf._
import modules.DeclaredTheory
import libraries._
import objects._
import objects.Conversions._
import symbols.Constant

object Test {
    val testbasenat = DPath(URI("http://latin.omdoc.org/testnat"))// problem base
    val testbasenat2 = DPath(URI("http://latin.omdoc.org/testnat2"))

    //case class Error(msg : String) extends java.lang.Throwable(msg)

    def main(args : Array[String]) {
      val controller = new Controller
      controller.handleLine("file refl-test.mmt")// run what's written in this file first - add logs, archives etc.
      val rs = controller.extman.ruleStore
      rs.add(PiType,PiTerm,ApplyTerm,LambdaTerm,Beta,Extensionality,Initial,Solve,ExpandArrow,TermReflectionRule,TypeReflectionRule,ReflTermEvalRule,ElimReflectionRule,ComputationReflectionRule,SoundnessReflectionRule,CompletenessReflectionRule,ExtensionalityReflectionRule,SolveEvalReflectionRule,SolveReflReflectionRule,ReflTypingRule)

      //val baseType = new Pattern(OMID(tptpbase ? "THF0"), LocalName("baseType"),Context(), OMV("t") % OMS(tptpbase ? "Types" ? "$tType"))
      //controller.add(baseType)

      //the meta-level theory of natural numbers
      val Nat = new DeclaredTheory(testbasenat, LocalPath(List("nat")), Some (LF.lftheory))
      controller.add(Nat)

      //the reflected theory of natural numbers
      val NatR = new DeclaredTheory(testbasenat2, LocalPath(List("nat2")), Some (LF.lftheory))
      controller.add(NatR)

      //generic type of natural numbers in the theory Nat
      val nat = new Constant(OMID(Nat.path), LocalName("nat"), Some(LF.ktype), None, None, None)
      controller.add(nat)

      //zero constructor in the theory Nat
      val zero = new Constant(OMID(Nat.path), LocalName("zero"), Some(OMID(nat.path)), None, None, None)
      controller.add(zero)

      //successor constructor in the theory Nat
      val succ = new Constant(OMID(Nat.path), LocalName("succ"), Some(Arrow(OMID(nat.path),OMID(nat.path))), None, None, None)
      controller.add(succ)

      //reflected type of natural numbers in the theory NatR
      val nat_refl = new Constant(OMID(NatR.path), LocalName("N"), Some(LF.ktype), Some(ReflType(OMMOD(NatR.path), OMID(nat.path))), None, None)
      controller.add(nat_refl)

      //zero constructor reflected from the theory Nat down to the theory NatR
      val zero_refl = new Constant(OMID(NatR.path),LocalName("0"), Some(OMID(nat_refl.path)), Some(TermRefl(OMMOD(NatR.path), OMID(zero.path))), None, None)
      controller.add(zero_refl)

      //successor constructor reflected from the theory Nat down to the theory NatR
      val succ_refl = new Constant(OMID(NatR.path), LocalName("s"), Some(Arrow(OMID(nat_refl.path),OMID(nat_refl.path))), Some(TermRefl(OMMOD(NatR.path), OMID(succ.path))), None, None))
      controller.add(succ_refl)

      /**
      * auxiliaries for defining binary natural number addition (add)      *
      */

      /**
      * auxiliaries for defining the corresponding morphism as a Record
      *  sigma_n: assignment for the type of natural numbers
      *  sigma_z: assignment for the zero constructor
      *  sigma_s: assignment for the successor constructor
      *  sigma: corresponding record for the addition morphism mapping from Nat to NatR
      */

      val sigma_n = (LocalName("nat"),Arrow(OMID(nat_refl.path),OMID(nat_refl.path)))
      val sigma_z = (LocalName("zero"),Lambda(LocalName("n"),OMID(nat_refl.path),OMV(LocalName("n"))))
      val sigma_s = Record(List((LocalName("succ"),Lambda(LocalName("f"),Arrow(OMID(nat_refl.path),OMID(nat_refl.path)), Lambda(LocalName("n"), OMID(nat_refl.path), ApplyTerm(succ_refl,Apply(LocalName("f"),LocalName("n"))))))))
      val sigma = Record(List(sigma_n,sigma_z))

      val addnat = ExplicitMorph(sigma,OMMOD(Nat2.path))
      //(LocalName("zero"), Lambda(LocalName("lambda"),OMV(LocalName("x")),OMV(LocalName("x")))),(LocalName("succ")...)  not sure about these
      //controller.add(addnat)


      //println(Nat.toString)
      //val unknowns =  "a"% LF.ktype ++ "a'" % LF.ktype ++ "b" % OMV("a") ++ "b'" % OMV("a'")  ++ "c" % LF.ktype  ++
      //  "d" % LF.ktype  ++ "e" % LF.ktype ++ "UO" % LF.ktype ++ "F" % OMV("UO")
      //val sol = new Solver(controller,OMMOD(Nat2.path), unknowns)
      //val tm = TermRefl(OMMOD(Nat.path), Apply(OMID(succ.path), OMID(zero.path)))
      //val tj = Typing(Stack.empty(OMMOD(Nat2.path)), tm, tp)
      //println(tj)
      //println(sol(tj))
      //println(sol)
//
//      implicit def pToOMID(p: GlobalName) = OMID(p)
//
//      def main(args: Array[String]) {
//        val controller = new Controller
//        controller.handleLine("file checker-test.mmt")
//        val rs = controller.extman.ruleStore
//        rs.add(PiType,PiTerm,ApplyTerm,LambdaTerm,Beta,Extensionality,Initial,Solve,ExpandArrow)
//
//
//        val latin = DPath(utils.URI("http", "latin.omdoc.org"))
//        val syn = latin / "logics" / "syntax"
//        val pf = latin / "logics" / "proof_theory"
//        val log = syn ? "Logic"
//        val o = log ? "o"
//        def ded(t: Term) = Apply(log ? "ded", t)
//        val imp = syn ? "IMP" ? "imp"
//        val forall = syn ? "Forall" ? "forall"
//        val tr = syn ? "Truth" ? "true"
//        val impI = pf ? "IMP" ? "impI"
//        val forallI = pf ? "Forall" ? "forallI"
//
//        val x = OMV("x")
//
//        // val tj = Typing(Context(), ApplySpine(impI, "b", "b'", Lambda("x", "c", x)), ded(ApplySpine(imp, tr, tr)))
//        val unknowns = "a" % LF.ktype ++ "a'" % LF.ktype ++ "b" % OMV("a") ++ "b'" % OMV("a'")  ++ "c" % LF.ktype  ++
//          "d" % LF.ktype  ++ "e" % LF.ktype ++ "UO" % LF.ktype ++ "F" % OMV("UO")
//        val sol = new Solver(controller, unknowns)
//        val tj = Typing(Context(),
//          ApplySpine(forallI, "F", Lambda("x", "d", ApplySpine(impI, "b", "b'", Lambda("x", "c", x)))),
//          ded(Apply(forall, Lambda("x", "e", ApplySpine(imp, tr, tr))))
//        )
//        println(tj)
//        sol(tj)
//        println(sol)

//      }
    }
}


