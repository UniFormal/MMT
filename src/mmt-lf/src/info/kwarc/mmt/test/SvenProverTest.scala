package info.kwarc.mmt.test

import info.kwarc.mmt.api._
import modules._
import symbols._
import objects._
import proving.imperative._

import info.kwarc.mmt.lf._

object SvenProverTest extends MMTIntegrationTest("MMT/urtheories", "MMT/examples")() {
  def main {
     val thyURI = Path.parse("NAMESPACE?THYNAME")
     val thy = controller.globalLookup.getAs(classOf[Theory], thyURI)
     val decls = thy.getDeclarations
     decls(0) match {
       case c: Constant =>
         c.name // name
         c.tp // type
         c.df // definition
         val pf = ImperativeProof(???)
         val pv = new Prover
         pv.prove(Context(), c.tp.get, pf)
    }
  }
}
