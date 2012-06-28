package info.kwarc.mmt.lf
import info.kwarc.mmt.api._
import objects._
import objects.Conversions._
import frontend._

object Test {
   implicit def pToOMID(p: GlobalName) = OMID(p)
   
   def main(args: Array[String]) {
      val controller = new Controller
      controller.handleLine("file checker-test.mmt")
      val rs = controller.extman.ruleStore
      rs.add(PiType,PiTerm,ApplyTerm,LambdaTerm,Beta,Extensionality,Initial,Solve,ExpandArrow)
      
      val unknowns = "a" % LF.ktype ++ "a'" % LF.ktype ++ "b" % OMV("a") ++ "b'" % OMV("a'")  ++ "c" % LF.ktype  ++
        "d" % LF.ktype  ++ "e" % LF.ktype ++ "UO" % LF.ktype ++ "F" % OMV("UO")
      val sol = new Solver(controller, unknowns)
      
      val latin = DPath(utils.URI("http", "latin.omdoc.org"))
      val syn = latin / "logics" / "syntax"
      val pf = latin / "logics" / "proof_theory"
      val log = syn ? "Logic"
      val o = log ? "o"
      def ded(t: Term) = Apply(log ? "ded", t)
      val imp = syn ? "IMP" ? "imp"
      val forall = syn ? "Forall" ? "forall"
      val tr = syn ? "Truth" ? "true"
      val impI = pf ? "IMP" ? "impI"
      val forallI = pf ? "Forall" ? "forallI"
      
      val x = OMV("x")
      
      // val tj = Typing(Context(), ApplySpine(impI, "b", "b'", Lambda("x", "c", x)), ded(ApplySpine(imp, tr, tr)))
      val tj = Typing(Context(),
            ApplySpine(forallI, "F", Lambda("x", "d", ApplySpine(impI, "b", "b'", Lambda("x", "c", x)))),
            ded(Apply(forall, Lambda("x", "e", ApplySpine(imp, tr, tr))))
      )
      println(tj)
      sol(tj)
      println(sol)
      
   }
}
