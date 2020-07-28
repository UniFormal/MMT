package info.kwarc.mmt.api.presentation

import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.objects.{OMA, OMID, OMMOD, OMV}
import info.kwarc.mmt.api.symbols.{FinalConstant, TermContainer, Visibility}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.{DPath, LocalName, presentation}
import info.kwarc.mmt.lf.{ApplySpine, FunTerm, FunType}
import info.kwarc.mmt.test.MMTUnitTest

/**
  * Unit test for [[MMTSyntaxPresenter]]
  *
  * It is located here in the mmt-lf module since we make use of LF function types and lambdas in the sample terms we test.
  */
object MMTSyntaxPresenterTest extends MMTUnitTest {
  override def main(): Unit = {
    val str = presentSampleConstant()

    log("Presented constant is:\n" + str)

    assertEqual(8, str.split("❘").length, "delimiter count of ❘ is unexpected")

    assertEqual(1, "❘\\s*role simplify\\s*[❘|❙]".r.findAllIn(str).length, "role incorrect")
    assertEqual(1, "❘\\s*@ jud\\s*[❘|❙]".r.findAllIn(str).length, "alias incorrect")
    assertEqual(1, "❘\\s*@ judgment\\s*[❘|❙]".r.findAllIn(str).length, "alias incorrect")
    assertEqual(1, "❘\\s*# 1 \\|- 2 ∶ 3 prec 100\\s*[❘|❙]".r.findAllIn(str).length, "notation incorrect")
    assertEqual(1, "❘\\s*meta ?metakey1 ?MMTSyntaxPresenterTestBar\\s*[❘|❙]".r.findAllIn(str).length, "meta datum incorrect")
    assertEqual(1, "❘\\s*meta ?metakey2 (f x)\\s*[❘|❙]".r.findAllIn(str).length, "meta datum incorrect")
  }


  /*
    Builds the following constant and returns the rendering of [[MMTSyntaxPresenter]] on it:

    {{{
    judgement
      : {V:vocabulary} {Γ:Ctx V} Expr Γ ⟶ (Expr Γ ⟶ prop)
      ❘ = [V:vocabulary] [Γ:Ctx V] [e:Expr Γ] [E:Expr Γ] foo bar
      ❘ role simplify
      ❘ @ jud
      ❘ @ judgment
      ❘ # 1 |- 2 ∶ 3 prec 100
      ❘ meta ?metakey1 ?MMTSyntaxPresenterTestBar
      ❘ meta ?metakey2 (f x)
      ❙
    }}}
  */
  private def presentSampleConstant(): String = {
    val theoryPath = DPath(URI("https://MMTUnitTest.test.mmt.kwarc.infoexample.com")) ? "MMTSyntaxPresenterTest"
    val otherDummyTheoryPath = theoryPath.parent ? "MMTSyntaxPresenterTestBar"

    val notationContainer = NotationContainer(TextNotation(
      fixity = Mixfix(List(SimpArg(1), Delim("|-"), SimpArg(2), Delim("∶"), SimpArg(3))),
      precedence = Precedence.integer(100),
      meta = None
    ))

    val tpC = TermContainer.asParsed(FunType(
      in = List(
        (Some(LocalName("V")), OMID(theoryPath ? "vocabulary")),
        (Some(LocalName("Γ")), ApplySpine(OMID(theoryPath ? "Ctx"), OMV("V"))),
        (None, ApplySpine(OMID(theoryPath ? "Expr"), OMV("Γ"))),
        (None, ApplySpine(OMID(theoryPath ? "Expr"), OMV("Γ")))
      ),
      out = OMID(theoryPath ? "prop")
    ))

    val dfC = TermContainer.asParsed(FunTerm(
      in = List(
        (LocalName("V"), OMID(theoryPath ? "vocabulary")),
        (LocalName("Γ"), ApplySpine(OMID(theoryPath ? "Ctx"), OMV("V"))),
        (LocalName("e"), ApplySpine(OMID(theoryPath ? "Expr"), OMV("Γ"))),
        (LocalName("E"), ApplySpine(OMID(theoryPath ? "Expr"), OMV("Γ")))
      ),
      out = ApplySpine(OMID(theoryPath ? "foo"), OMID(theoryPath ? "bar"))
    ))

    val c = new FinalConstant(
      home = OMMOD(theoryPath),
      name = LocalName("judgement"),
      alias = List(LocalName("jud"), LocalName("judgment")),
      tpC = tpC,
      dfC = dfC,
      rl = Some("simplify"),
      notC = notationContainer,
      vs = Visibility.public
    )

    val thy = Theory.empty(theoryPath.parent, theoryPath.name, mt = None)

    c.metadata.add(
      MetaDatum(theoryPath ? "metakey1", OMMOD(otherDummyTheoryPath))
    )
    c.metadata.add(
      MetaDatum(
        theoryPath ? "metakey2",
        OMA(
          OMID(theoryPath ? "f"),
          List(OMID(theoryPath ? "x"))
        )
      )
    )

    controller.add(thy)
    controller.add(c)

    val presenter = new MMTSyntaxPresenter()
    presenter.init(controller)

    val rh = new presentation.StringBuilder
    presenter(c)(rh)

    controller.delete(thy.path)

    rh.get
  }
}
