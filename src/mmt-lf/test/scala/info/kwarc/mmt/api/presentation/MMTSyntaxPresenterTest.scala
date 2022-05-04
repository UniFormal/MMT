package info.kwarc.mmt.api.presentation

import info.kwarc.mmt.api.documents.{Document, MRef}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.objects.{OMA, OMID, OMMOD, OMV, Term}
import info.kwarc.mmt.api.presentation.MMTSyntaxPresenterTest.controller
import info.kwarc.mmt.api.symbols.{Constant, FinalConstant, Structure, TermContainer, Visibility}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.{ComponentParent, DPath, LocalName, NarrativeElement, StructuralElement, presentation}
import info.kwarc.mmt.lf.{ApplySpine, FunTerm, FunType}
import info.kwarc.mmt.test.MMTUnitTest

import scala.xml.Node

/**
  * Unit test for [[MMTSyntaxPresenter]]
  *
  * It is located here in the mmt-lf module since we make use of LF function types and lambdas in the sample terms we test.
  */
object MMTSyntaxPresenterTest extends MMTUnitTest {
  override def main(): Unit = {
    val presenter = new MMTSyntaxPresenter()
    presenter.init(controller)

    richAnnotatedConstantTest()(controller, presenter)
    structureTest()(controller, presenter)
    allLevelMetaTest()(controller, presenter)
  }

  private def richAnnotatedConstantTest()(implicit ctrl: Controller, presenter: MMTSyntaxPresenter): Unit = {
    val (thy, otherThy, c) = MMTSyntaxPresenterTestFixtures.createRichAnnotatedConstantTest()
    val str = presenter.asString(c)

    log("Testing presentation of constant with type, definiens, metadata, and much more:")
    logGroup {
      log("Presented constant is:\n" + str)

      assertEqual(8, str.split("❘").length, "delimiter count of ❘ is unexpected")

      assertEqual(1, "❘\\s*role simplify\\s*[❘|❙]".r.findAllIn(str).length, "role incorrect")
      assertEqual(1, "❘\\s*@ jud\\s*[❘|❙]".r.findAllIn(str).length, "alias incorrect")
      assertEqual(1, "❘\\s*@ judgment\\s*[❘|❙]".r.findAllIn(str).length, "alias incorrect")
      assertEqual(1, "❘\\s*# 1 \\|- 2 ∶ 3 prec 100\\s*[❘|❙]".r.findAllIn(str).length, "notation incorrect")
      assertEqual(1, "❘\\s*meta metakey1 \\?S\\s*[❘|❙]".r.findAllIn(str).length, "meta datum incorrect")
      assertEqual(1, "❘\\s*meta metakey2 \\(f\\s+x\\)\\s*[❘|❙]".r.findAllIn(str).length, "meta datum incorrect")
    }

    controller.delete(thy.path)
    controller.delete(otherThy.path)
  }

  private def structureTest()(implicit ctrl: Controller, presenter: MMTSyntaxPresenter): Unit = {
    val (innerThy, outerThy) = MMTSyntaxPresenterTestFixtures.createStructureTest()
    val str = presenter.asString(outerThy)

    log("Testing presentation of structures and metadata referring to constants within those structures in form of meta keys and meta values:")
    logGroup {
      log("Presented outerThy is:\n" + str)

      assertEqual(1, "structure s1\\s+:\\s+[a-zA-Z.:/]+\\?InnerTheory\\s+❙".r.findAllIn(str).length, "structure declaration incorrect")
      assertEqual(1, "structure s2\\s+:\\s+[a-zA-Z.:/]+\\?InnerTheory\\s+❙".r.findAllIn(str).length, "structure declaration incorrect")
      assertEqual(1, "e\\s+meta s1/c s1/d\\s*?❘\\s*?meta s2/c s2/d".r.findAllIn(str).length, "meta datum incorrect")
    }

    controller.delete(innerThy.path)
    controller.delete(outerThy.path)
  }

  private def allLevelMetaTest()(implicit ctrl: Controller, presenter: MMTSyntaxPresenter): Unit = {
    val doc :: _ = MMTSyntaxPresenterTestFixtures.createAllLevelMetaTest()
    log("Testing presentation of meta data across all levels (documents, modules, declarations):")
    logGroup {
      log(presenter.asString(doc))
    }
  }
}

object MMTSyntaxPresenterTestFixtures {
  private val baseDocument: DPath = DPath(URI("https://MMTSyntaxPresenterTest.MMTUnitTest.test.mmt.kwarc.infoexample.com"))

  private def untypedConstant(home: Term, name: LocalName): FinalConstant = new FinalConstant(
    home,
    name,
    alias = Nil,
    tpC = TermContainer.empty(),
    dfC = TermContainer.empty(),
    rl = None,
    notC = NotationContainer.empty(),
    vs = Visibility.public
  )

  /**
    * Builds the following theories and constants:
    *
    * {{{
    *     theory T =
    *         judgement
    *           : {V:vocabulary} {Γ:Ctx V} Expr Γ ⟶ (Expr Γ ⟶ prop)
    *           ❘ = [V:vocabulary] [Γ:Ctx V] [e:Expr Γ] [E:Expr Γ] foo bar
    *           ❘ role simplify
    *           ❘ @ jud
    *           ❘ @ judgment
    *           ❘ # 1 |- 2 ∶ 3 prec 100
    *           ❘ meta metakey1 ?S
    *           ❘ meta metakey2 (f x)
    *         ❙
    *     ❚
    *
    *     theory S = ❚
    * }}}
    *
    * @return (T, S, judgement), all of them already added to the controller
    */
  def createRichAnnotatedConstantTest()(implicit ctrl: Controller): (Theory, Theory, Constant) = {
    val theoryPath = (baseDocument / "createRichAnnotatedConstant") ? "T"
    val otherTheoryPath = theoryPath.parent ? "S"

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
      MetaDatum(theoryPath ? "metakey1", OMMOD(otherTheoryPath))
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

    val otherThy = Theory.empty(otherTheoryPath.parent, otherTheoryPath.name, mt = None)

    List(thy, c, otherThy).foreach(ctrl.add(_))

    (thy, otherThy, c)
  }


  /*
    Builds the following theories:

    {{{
    theory MMTSyntaxPresenterTestStructure =
        c ❙
        d ❙
    ❚

    theory MMTSyntaxPresenterTestStructure =
        structure s1 : ?Test = ❚
        structure s2 : ?Test = ❚

        e ❘ meta s1/c s1/d ❘ meta s2/c s2/d ❙
    ❚
    }}}
  */
  def createStructureTest()(implicit ctrl: Controller): (Theory, Theory) = {
    val docPath = DPath(URI("https://MMTSyntaxPresenterTest.MMTUnitTest.test.mmt.kwarc.infoexample.com")) / "structuretest"

    val innerTheory: Theory = {
      val innerTheory = Theory.empty(docPath, LocalName("InnerTheory"), mt = None)
      val c = untypedConstant(innerTheory.toTerm, LocalName("c"))
      val d = untypedConstant(innerTheory.toTerm, LocalName("d"))

      List(innerTheory, c, d).foreach(ctrl.add(_))
      innerTheory
    }

    val outerTheory: Theory = {
      val outerTheory = Theory.empty(docPath, LocalName("OuterTheory"), mt = None)
      val s1 = Structure(outerTheory.toTerm, LocalName("s1"), innerTheory.toTerm, df = None, isImplicit = false, isTotal = true)
      val s2 = Structure(outerTheory.toTerm, LocalName("s2"), innerTheory.toTerm, df = None, isImplicit = false, isTotal = true)
      val e = untypedConstant(outerTheory.toTerm, LocalName("e"))
      e.metadata.add(MetaDatum(
        s1.path / LocalName("c"),
        OMID(s1.path / LocalName("d"))
      ))
      e.metadata.add(MetaDatum(
        s2.path / LocalName("c"),
        OMID(s2.path / LocalName("d"))
      ))

      List(outerTheory, s1, s2, e).foreach(ctrl.add(_))
      outerTheory
    }

    (innerTheory, outerTheory)
  }

  def createAllLevelMetaTest()(implicit ctrl: Controller): List[StructuralElement] = {
    val doc = new Document(baseDocument / "allLevelMetaTest")

    val metakey1 = doc.path ? "metatheory" ? "symbol1"
    val metavalue1 = OMID(doc.path ? "metatheory" ? "value1")

    val metakey2 = doc.path ? "metatheory" ? "symbol2"
    val metavalue2 = OMID(doc.path ? "metatheory" ? "value2")

    val metadata = List(MetaDatum(metakey1, metavalue1), MetaDatum(metakey2, metavalue2))

    doc.metadata.add(metadata : _*)
    val thy = Theory.empty(doc.path, LocalName("thy"), mt = None)
    thy.metadata.add(metadata : _*)

    val c = untypedConstant(thy.toTerm, LocalName("c"))
    c.metadata.add(metadata : _*)

    val view = View(doc.path, LocalName("vw"), from = thy.toTerm, to = thy.toTerm, isImplicit = false)
    view.metadata.add(metadata : _*)

    val structuralThings = List(doc, thy, c, view)
    structuralThings.foreach(ctrl.add(_))

    ctrl.add(MRef(doc.path, thy.path))
    ctrl.add(MRef(doc.path, view.path))

    structuralThings
  }
}
