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

object TestNat {
    val testbasenat = DPath(URI("http://latin.omdoc.org/testnat"))// problem base

    //case class Error(msg : String) extends java.lang.Throwable(msg)

    def main(args : Array[String]) {
      val controller = new Controller
      controller.handleLine("file refl-test.mmt")// run what's written in this file first - add logs, archives etc.

      //val baseType = new Pattern(OMID(tptpbase ? "THF0"), LocalName("baseType"),Context(), OMV("t") % OMS(tptpbase ? "Types" ? "$tType"))
      //controller.add(baseType)

      val found  = new ReflectionFoundation
      val lup = new PlainLookup(controller.globalLookup)

      val Nat = new DeclaredTheory(testbasenat, LocalPath(List("nat")), Some (LF.lftheory))
      controller.add(Nat)

      val nat = new Constant(OMID(Nat.path), LocalName("nat"), Some(LF.ktype), None, None, None)
      controller.add(nat)

      val zero = new Constant(OMID(Nat.path), LocalName("zero"), Some(OMID(nat.path)), None, None, None)
      controller.add(zero)

      val succ = new Constant(OMID(Nat.path), LocalName("succ"), Some(Arrow(OMID(nat.path),OMID(nat.path))), None, None, None)
      controller.add(succ)

      println(Nat.toString)

      val tm = TermRefl(Nat.path, OMA(OMID(succ.path), List(OMID(zero.path))))
      val tp = TermRefl(Nat.path, OMA(OMID(succ.path), List(OMID(zero.path))))

      found.refltyping(Some(tm), Some(tp), Stack(List(Frame(Nat.path, Context()))))(lup)
    }
}

/*object TestRing {
  val testbasering = DPath(URI("http://latin.omdoc.org/testring"))
  def main(args: Array[String]) {
    val con
  }
}   */