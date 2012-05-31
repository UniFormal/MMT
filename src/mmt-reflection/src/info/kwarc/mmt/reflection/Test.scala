package info.kwarc.mmt.reflection

import info.kwarc.mmt.api.objects.OMID

object Test {
    val testbase = DPath(URI("http://latin.omdoc.org/test"))// problem base

    //case class Error(msg : String) extends java.lang.Throwable(msg)

    def main(args : Array[String]) {
      val controller = new Controller
      controller.handleLine("file refl-test.mmt")// run what's written in this file first - add logs, archives etc.

      //val baseType = new Pattern(OMID(tptpbase ? "THF0"), LocalName("baseType"),Context(), OMV("t") % OMS(tptpbase ? "Types" ? "$tType"))
      //controller.add(baseType)


      val found  = new ReflectionFoundation
      val nat = testbase ? "Nat"
      val succ = OMID(nat ? "succ")
      val zero = OMID(nat ? "zero")

      val tm = TermRefl(nat, OMA(succ, List(zero)))
      val tp =
      found.typing(tm, tp)

  }
}