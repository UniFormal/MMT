package info.kwarc.mmt.frameit

import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects.{OMID, OMV, Term}
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.api.{GlobalName, LocalName, NamespaceMap, Path}
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld
import info.kwarc.mmt.frameit.archives.MitM
import info.kwarc.mmt.frameit.business.ViewCompletion
import info.kwarc.mmt.lf.ApplySpine
import info.kwarc.mmt.odk.LFX.Sigma
import info.kwarc.mmt.test.MMTIntegrationTest

object ViewCompletionTest extends MMTIntegrationTest(
  "FrameIT/frameworld"
)(){
  private val frameworldArchiveNS = Path.parseD("http://mathhub.info/FrameIT/frameworld", NamespaceMap.empty)
  private val integrationtestsNS = frameworldArchiveNS / "integrationtests"

  override def main(): Unit = {
    expectedTypeTests()
    closeGapsTests()
  }

  /**
    * Helper method to quickly get the definiens of a (supposedly) [[FinalConstant]] of a theory.
    * @param thy The theory
    * @param constant The constant's name, will be converted to [[LocalName]] via its constructor
    * @return Upon success, the definiens as a term. Upon failure, an exception.
    */
  private def getConstantDefiniens(thy: Theory, constant: String): Term = {
    thy.get(LocalName(constant)).asInstanceOf[FinalConstant].df.get
  }

  private def expectedTypeTests(): Unit = {
    // We test expected type computation on pseudo views ("assignment lists", i.e. views that may fail to contain
    // mappings for arbitrary domain declarations).
    //
    // These pseudo views are all intended to have this domain:
    val domainTheoryP = integrationtestsNS ? "ExpectedTypeTest_Domain"
    val domainTheory = controller.getTheory(domainTheoryP)

    // and this codomain: (does not really exist and does not even need to do so)
    val codomainTheoryP = (frameworldArchiveNS / "integrationtests") ? "ExpectedTypeTest_Codomain"

    test("ViewCompletion.expectedType can compute easy homomorphic extension with just constants on RHS of assignments", {
      val assignments: List[(GlobalName, Term)] = List(
        (domainTheoryP ? "A", OMID(codomainTheoryP ? "A")),
        (domainTheoryP ? "B", OMID(codomainTheoryP ? "B")),
        (domainTheoryP ? "C", OMID(codomainTheoryP ? "C")),
      )

      val expectedExpectedType = Sigma(
        LocalName("a"),
        OMID(FrameWorld.real),
        ApplySpine(
          OMID(FrameWorld.ded),
          ApplySpine(
            OMID(FrameWorld.eq),
            OMID(FrameWorld.real),
            ApplySpine(
              OMID(Path.parseS("http://mathhub.info/MitM/core/geometry?3DGeometry/Common?angle_between", NamespaceMap.empty)),
              OMID(codomainTheoryP ? "codA"),
              OMID(codomainTheoryP ? "codB"),
              OMID(codomainTheoryP ? "codC")
            ),
            OMV(LocalName("a"))
          )
        )
      )

      val actualExpectedType = ViewCompletion.expectedType(
        assignments,
        domainTheory.meta,
        domainTheory.get(LocalName("angleABC")).asInstanceOf[FinalConstant].tp.get
      )(controller)

      assertTermEqual(expectedExpectedType, actualExpectedType.get)
    })

    test("ViewCompletion.expectedType can compute homomorphic extension with more complex expressions", {
      /*// read off complex expression for assignment from the definiens of an existing constant
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
        domainTheory.meta,
        domainTheory.get(LocalName("pangleABC")).asInstanceOf[FinalConstant].tp.get
      )(controller)

      println(actualExpectedType.contains(expectedExpectedType))*/
    })

    test("ViewCompletion.expectedType rightfully fails if gaps remain", {
      val assignments: List[(GlobalName, Term)] = List(
        (domainTheoryP ? "A", OMID(codomainTheoryP ? "A")),
        // assignment to B intentionally left out
        (domainTheoryP ? "C", OMID(codomainTheoryP ? "C")),
      )

      val expectedType = ViewCompletion.expectedType(
        assignments,
        domainTheory.meta,
        domainTheory.get(LocalName("angleABC")).asInstanceOf[FinalConstant].tp.get
      )(controller)

      if (expectedType.isDefined) {
        testError("expectedType did not fail even though assignments contained gaps")
      }
    })
  }

  private def closeGapsTests(): Unit = {
    /*val integrationtestsNS = frameworldArchiveNS / "integrationtests"
    val codomainTheory = controller.getTheory(integrationtestsNS ? "CloseGapsTest_Codomain")
    val notepadTheory = controller.getTheory(integrationtestsNS ? "CloseGapsTest_TermsNotepad")

    val domainTheoryP = frameworldArchiveNS ? "OppositeLen" / "OppositeLen_Problem"
    val domainTheory = controller.getTheory(domainTheoryP)

    test("ViewCompletion.closeGaps closes simple gaps", {
      val assignments: List[(GlobalName, Term)] = List(
        (domainTheoryP ? "pangleABC", OMID(codomainTheory.path ? "complexAngleFact"))
      )

      val expectedClosedGaps = List(
        (domainTheoryP ? "pA", getConstantDefiniens(notepadTheory, "expected_gap_pA")),
        (domainTheoryP ? "pB", getConstantDefiniens(notepadTheory, "expected_gap_pB")),
        (domainTheoryP ? "pC", getConstantDefiniens(notepadTheory, "expected_gap_pC")),
        (domainTheoryP ? "pangleABC_v", getConstantDefiniens(notepadTheory, "expected_gap_pangleABC_v")),
      ).toSet

      val actualClosedGaps = ViewCompletion.closeGaps(
        assignments,
        domainTheory.meta
      )(controller).toSet

      assertSetEqual(expectedClosedGaps, actualClosedGaps)
    })*/
  }
}
