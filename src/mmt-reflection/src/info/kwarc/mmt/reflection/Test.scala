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

      val Nat = new DeclaredTheory(testbasenat, LocalPath(List("nat")), Some (LF.lftheory))
      controller.add(Nat)

      val Nat2 = new DeclaredTheory(testbasenat2, LocalPath(List("nat2")), Some (LF.lftheory))

      val nat = new Constant(OMID(Nat.path), LocalName("nat"), Some(LF.ktype), None, None, None)
      controller.add(nat)

      val zero = new Constant(OMID(Nat.path), LocalName("zero"), Some(OMID(nat.path)), None, None, None)
      controller.add(zero)

      val succ = new Constant(OMID(Nat.path), LocalName("succ"), Some(Arrow(OMID(nat.path),OMID(nat.path))), None, None, None)
      controller.add(succ)

      //val addnat = ExplicitMorph(Record(List((LocalName("nat"),Some(Arrow(OMID(nat.path),OMID(nat.path)))), (LocalName("zero"), Lambda(LocalName("n"), OMID("n".path) )), (LocalName("succ")) )),OMMOD(Nat.path)))
      //controller.add(addnat)

      println(Nat.toString)

      val tm = TermRefl(OMMOD(Nat.path), Apply(OMID(succ.path), OMID(zero.path)))
      val tp = ReflType(OMMOD(Nat.path), OMID(nat.path))

      val unknowns =  "a"% LF.ktype ++ "a'" % LF.ktype ++ "b" % OMV("a") ++ "b'" % OMV("a'")  ++ "c" % LF.ktype  ++
        "d" % LF.ktype  ++ "e" % LF.ktype ++ "UO" % LF.ktype ++ "F" % OMV("UO")
      val sol = new Solver(controller,OMMOD(Nat2.path), unknowns)
      val tj = Typing(Stack.empty(OMMOD(Nat2.path)), tm, tp)
      println(tj)
      println(sol(tj))
      println(sol)
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


