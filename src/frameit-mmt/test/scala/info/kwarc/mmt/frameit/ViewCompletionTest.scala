package info.kwarc.mmt.frameit

import info.kwarc.mmt.api.objects.{OMID, Term}
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.api.{GlobalName, LocalName, NamespaceMap, Path}
import info.kwarc.mmt.lf.ApplySpine
import info.kwarc.mmt.test.MMTIntegrationTest

object ViewCompletionTest extends MMTIntegrationTest(
  "FrameIT/frameworld"
)(){
  private val frameworldArchiveNS = Path.parseD("http://mathhub.info/FrameIT/frameworld", NamespaceMap.empty)

  private def assertTermEqual(expected: Term, actual: Term, msg: Option[String] = None): Unit = {
    if (expected != actual) {
      testError(s"Term equality failed${msg.map(" (" + _ + ")").getOrElse("")}:")
      testError("Expected term: " + expected)
      testError("Actual term: " + actual)
    }
  }

  override def main(): Unit = {
    expectedTypeTests()
  }

  def expectedTypeTests() {
    // We test expected type computation on pseudo views ("assignment lists", i.e. views that may fail to contain
    // mappings for arbitrary domain declarations).
    //
    // These pseudo views are all intended to have this domain:
    val domainTheoryP = frameworldArchiveNS ? "OppositeLen_Problem"
    val domainTheory = controller.getTheory(domainTheoryP)

    // and this codomain: (does not really exist and does not even need to do so)
    val codomainTheoryP = (frameworldArchiveNS / "integrationtests") ? "ExpectedTypeTest_Codomain"

    test("ViewCompletion.expectedType can compute easy homomorphic extension with just constants on RHS of assignments", () => {
      val assignments: List[(GlobalName, Term)] = List(
        (domainTheoryP ? "pA", OMID(codomainTheoryP ? "pA")),
        (domainTheoryP ? "pB", OMID(codomainTheoryP ? "pB")),
        (domainTheoryP ? "pC", OMID(codomainTheoryP ? "pC")),
        (domainTheoryP ? "pangleABC_v", OMID(codomainTheoryP ? "pangleABC_v"))
      )

      val expectedExpectedType = ApplySpine(
        OMID(frameworldArchiveNS ? "AngleFact" ? "angleFact"),
        OMID(codomainTheoryP ? "pA"),
        OMID(codomainTheoryP ? "pB"),
        OMID(codomainTheoryP ? "pC"),
        OMID(codomainTheoryP ? "pangleABC_v")
      )

      val actualExpectedType = ViewCompletion.expectedType(
        assignments,
        domainTheory.meta.get,
        domainTheory.get(LocalName("pangleABC")).asInstanceOf[FinalConstant].tp.get
      )(controller)

      assertTermEqual(expectedExpectedType, actualExpectedType.get)
    })

    test("ViewCompletion.expectedType can compute homomorphic extension with more complex expressions", {
      // read off complex expression for assignment from the definiens of an existing constant
      val angleValue = controller.getAs(classOf[FinalConstant], codomainTheoryP ? "pangleComplexExpression").df.get

      val assignments: List[(GlobalName, Term)] = List(
        (domainTheoryP ? "pA", OMID(codomainTheoryP ? "pA")),
        (domainTheoryP ? "pB", OMID(codomainTheoryP ? "pC")), // intentionally C for testing purposes
        (domainTheoryP ? "pC", OMID(codomainTheoryP ? "pB")), // intentionally B
        (domainTheoryP ? "pangleABC_v", angleValue)
      )

      val expectedExpectedType = ApplySpine(
        OMID(frameworldArchiveNS ? "AngleFact" ? "angleFact"),
        OMID(codomainTheoryP ? "A"),
        OMID(codomainTheoryP ? "C"),
        OMID(codomainTheoryP ? "B"),
        angleValue
      )

      val actualExpectedType = ViewCompletion.expectedType(
        assignments,
        domainTheory.meta.get,
        domainTheory.get(LocalName("pangleABC")).asInstanceOf[FinalConstant].tp.get
      )(controller)

      println(actualExpectedType.contains(expectedExpectedType))
    })

    test("ViewCompletion.expectedType rightfully fails if gaps remain", () => {
      // read off complex expression for assignment from the definiens of an existing constant
      val angleValue = controller.getAs(classOf[FinalConstant], codomainTheoryP ? "pangleComplexExpression").df.get

      val assignments: List[(GlobalName, Term)] = List(
        (domainTheoryP ? "pA", OMID(codomainTheoryP ? "pA")),
        // intentionally B left out
        (domainTheoryP ? "pC", OMID(codomainTheoryP ? "pB")),
        (domainTheoryP ? "pangleABC_v", angleValue)
      )

      val expectedType = ViewCompletion.expectedType(
        assignments,
        domainTheory.meta.get,
        domainTheory.get(LocalName("pangleABC")).asInstanceOf[FinalConstant].tp.get
      )(controller)

      if (expectedType.isDefined) {
        testError("expectedType did not fail even though assignments contained gaps")
      }
    })
  }
}
