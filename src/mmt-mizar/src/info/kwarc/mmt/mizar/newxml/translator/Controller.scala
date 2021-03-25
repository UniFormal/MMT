package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.presentation._
import info.kwarc.mmt.api.uom.SimplificationUnit
import info.kwarc.mmt.mizar.newxml._
import foundations._
import info.kwarc.mmt.api.checking.CheckingEnvironment
import info.kwarc.mmt.lf.{ApplyGeneral, FunType}
import info.kwarc.mmt.mizar.newxml.mmtwrapper.PatternUtils.{LambdaOrEmpty, PiOrEmpty, lambdaBindArgs, piBindArgs}
import mmtwrapper.MizarPrimitiveConcepts._
import mmtwrapper.{MizarPatternInstance, PatternUtils}

import java.io.PrintStream
import scala.collection._

object TranslationController {

  var controller = {
    val c = new frontend.Controller
    c
  }
  //set during translation
  var outputBase: DPath = null

  private def structChecker = controller.extman.get(classOf[checking.Checker], "mmt").get

  private def structureSimplifier = controller.simplifier

  /**
   * whether to typecheck translated content
   */
  private var typecheckContent: Boolean = true
  /**
   * whether to typecheck constants or just to check the entire theory at the end
   */
  private var checkConstants: Boolean = true
  def setCheckConstants(checkConstants: Boolean, typecheckContent: Boolean): Unit = {
    this.checkConstants = checkConstants; this.typecheckContent = typecheckContent
  }

  private var processDependency: String => Unit = { m: String => }
  def setProcessDependency(processDependency: String => Unit): Unit = {
    this.processDependency = processDependency
  }
  def processDependencyTheory(mpath: MPath) = {
    if (mpath.doc.^! == outputBase) processDependency (mpath.name.toString.toLowerCase)
  }
  private def errFilter(err: Error): Boolean = {
    val badStrings = List(
      "INHABITABLE"
      , "a declaration for the name "
    ) ++ (if (!checkConstants) List(
      "ill-formed constant reference"
      , "is not imported into current context"
    ) else Nil)

    val badTypes: List[String] = List(
      "AddError"
    )
    !(badStrings.exists(err.toStringLong.contains(_)) || badTypes.exists(err.getClass.toString.contains(_)))
  }

  private def typeCheckingErrHandler(logger: Option[frontend.Report]): ErrorHandler = new FilteringErrorHandler(new ErrorContainer(logger), errFilter(_))

  private def checkingEnvironment(logger: Option[frontend.Report]) = new CheckingEnvironment(TranslationController.structureSimplifier, typeCheckingErrHandler(logger), checking.RelationHandler.ignore, MMTTask.generic)

  def typecheckContent(e: StructuralElement, logger: Option[frontend.Report] = None) = structChecker(e)(checkingEnvironment(logger))

  object articleSpecificData {
    //set during translation
    var currentAid: String = null
    var currentDoc: Document = null
    var currentThy: Theory = null

    private var unresolvedDependencies: List[MPath] = Nil

    def addUnresolvedDependency(dep: MPath) = if (unresolvedDependencies.contains(dep)) {} else {
      unresolvedDependencies +:= dep
    }

    def getUnresolvedDependencies() = unresolvedDependencies

    private var withNotation = 0
    private var withoutNotation = 0
    def incrementNotationCounter(withNotation: Boolean): Unit = {
      if (withNotation) this.withNotation += 1 else withoutNotation += 1
    }

    private def withoutNotationProper = withoutNotation - articleStatistics.numRegistrs - articleStatistics.totalNumTheorems

    def notationStatistics = "We translated " + withNotation.toString + " declarations with notation and " + withoutNotation.toString + " declarations without, " + withoutNotationProper.toString + " of which aren't theorems and registrations (which are expected to have no notation)."

    private var anonymousTheoremCount = 0

    def incrementAndGetAnonymousTheoremCount() = {
      anonymousTheoremCount += 1; anonymousTheoremCount
    }

    def getAnonymousTheoremCount() = anonymousTheoremCount

    private var anonymousSchemeCount = 0

    def incrementAndGetAnonymousSchemeCount() = {
      anonymousSchemeCount += 1; anonymousSchemeCount
    }

    def getAnonymousSchemeCount() = anonymousSchemeCount

    private var identifyCount = 0

    def incrementAndGetIdentifyCount() = {
      identifyCount += 1; identifyCount
    }

    def getIdentifyCount() = identifyCount

    private var reduceCount = 0

    def incrementAndGetReduceCount() = {
      reduceCount += 1; reduceCount
    }

    def getReduceCount() = reduceCount

    private var resolvedDependencies: Set[MPath] = Set()

    def getDependencies = resolvedDependencies
    def addDependencies(additionalDependencies: Set[MPath]): Unit = {
      resolvedDependencies ++= additionalDependencies
    }

    object articleStatistics {
      def totalNumDefinitions: Int = functorDefinitions + predicateDefinitions + attributeDefinitions + modeDefinitions + schemeDefinitions + structureDefinitions

      def totalNumTheorems: Int = theorems + anonymousTheorems

      def numRegistrs: Int = registrations

      private var functorDefinitions = 0
      private var predicateDefinitions = 0
      private var attributeDefinitions = 0
      private var modeDefinitions = 0
      private var schemeDefinitions = 0
      private var structureDefinitions = 0
      private var nyms = 0
      private var registrations = 0
      private var theorems = 0

      private def anonymousTheorems = anonymousTheoremCount

      def makeArticleStatistics: String = "Overall we translated " + totalNumDefinitions + " definitions, " + registrations + " registrations and " + totalNumTheorems + " statements from this article."

      def incrementStatisticsCounter(implicit kind: String): Unit = kind match {
        case "func" => functorDefinitions += 1
        case "pred" => predicateDefinitions += 1
        case "attr" => attributeDefinitions += 1
        case "mode" => modeDefinitions += 1
        case "scheme" => schemeDefinitions += 1
        case "struct" => structureDefinitions += 1
        case "nym" => nyms += 1
        case "registr" => registrations += 1
        case "thm" => theorems += 1
        case _ => throw new TranslatingError("unrecognised statistics counter to increment: " + kind)
      }
      def setData(data: this.type): Unit = {
        functorDefinitions = data.functorDefinitions
        predicateDefinitions = data.predicateDefinitions
        attributeDefinitions = data.attributeDefinitions
        modeDefinitions = data.modeDefinitions
        schemeDefinitions = data.schemeDefinitions
        structureDefinitions = data.structureDefinitions
        nyms = data.nyms
        registrations = data.registrations
        theorems = data.theorems
      }
    }
    def setData(data: this.type): Unit = {
      var currentAid = data.currentAid
      var currentDoc = data.currentDoc
      var currentThy = data.currentThy
      unresolvedDependencies = data.unresolvedDependencies++this.unresolvedDependencies
      withNotation = data.withNotation
      withoutNotation = data.withoutNotation
      anonymousTheoremCount = data.anonymousTheoremCount
      anonymousSchemeCount = data.anonymousSchemeCount
      identifyCount = data.identifyCount
      reduceCount = data.reduceCount
      resolvedDependencies = data.resolvedDependencies
    }
  }

  import articleSpecificData._
  def currentBaseThy : Option[MPath] = Some(MizarPatternsTh)
  def currentBaseThyFile = File("/home/user/Erlangen/MMT/content/MathHub/MMT/LATIN2/source/foundations/mizar/"+MizarPatternsTh.name.toString+".mmt")
  def localPath : LocalName = LocalName(currentAid)
  def currentThyBase : DPath = outputBase / localPath
    //DPath(utils.URI(TranslationController.currentOutputBase.toString + localPath.toString))
  def currentTheoryPath : MPath = currentThyBase ? localPath
  def getTheoryPath(aid: String) = (outputBase / aid.toLowerCase()) ? aid.toLowerCase()
  def currentSource : String = mathHubBase + "/source/" + currentAid + ".miz"

  def makeDocument() = {
    currentDoc = new Document(currentThyBase)
    controller.add(currentDoc)
  }
  def makeTheory() = {
    currentThy = new Theory(currentThyBase, localPath, currentBaseThy, Theory.noParams, Theory.noBase)
    articleSpecificData.addDependencies(Set(currentTheoryPath, TranslatorUtils.hiddenArt))
    controller.add(currentThy)
  }
  def getDependencies(decl: Declaration with HasType with HasDefiniens) = {
    def isDependency(p: String) = p contains outputBase.toString
    def getDependencies(tm: Term): Set[MPath] = tm match {
      case OMS(p) => if (isDependency(p.toString)) Set(p.module) else Set()
      case OMBINDC(binder, context, scopes) => (binder::context.flatMap(_.tp):::scopes).toSet flatMap getDependencies
      case OMA(fun, args) => (fun::args).toSet flatMap getDependencies
      case OML(_, tp, df, _, _) => Set(tp, df).flatMap(_ map getDependencies getOrElse Set())
      case _ => Set()
    }
    if (isDependency(decl.toString())) {
      val dependencies = (decl match {
        case MizarPatternInstance(_, _, args) =>
          args flatMap getDependencies
        case c: Constant => List(c.tp, c.df).flatMap(_ map(List(_)) getOrElse(Nil)).flatMap(getDependencies)
        case _ => Nil
      }).filterNot (articleSpecificData.getDependencies contains _).toSet
      articleSpecificData.addDependencies(dependencies)
      dependencies
    } else Nil
  }
  def isBuild(aid: String) = controller.getO(getTheoryPath(aid)) flatMap {
    case t: Theory => Some (t.domain.exists(_.lastOption map (_.toString) map (List("funct", "pred", "attribute", "mode").contains(_)) getOrElse false))
  case _ => None} getOrElse false
  def addDependencies(decl: Declaration with HasType with HasDefiniens): Unit = {
    val deps = getDependencies(decl)
    val includes = deps map(PlainInclude(_, currentTheoryPath))
    includes zip deps foreach {case (inc, dep) =>
      try {
        addFront(inc)
        processDependencyTheory (dep)
      } catch {
        case e: GetError =>
          addUnresolvedDependency(dep)
          throw new TranslatingError("GetError while trying to include the dependent theory "+dep+" of the theory "+inc.to.toMPath+" to be translated: \n"+
            "Please make sure the theory is translated (build with mizarxml-omdoc build target) and try again. ")
        case er: Throwable =>
          addUnresolvedDependency(dep)
          val errClass = er.getClass.toString.split('.').last
          throw new TranslatingError(errClass+" while trying to simplify the included dependent theory "+dep+" of the theory "+inc.to.toMPath+" to be translated: \n"+
          er.getMessage+
          (er.getCause() match { case null => "" case th: Throwable => "\nCaused by "+th.getMessage}))
      }
    }
  }
  def endMake(report: Option[frontend.Report] = None) = {
    controller.endAdd(currentThy)
    currentDoc.add(MRef(currentDoc.path, currentThy.path))
    if (typecheckContent && ! checkConstants) typecheckContent(currentThy, report)
  }

  def add(e: NarrativeElement) : Unit = {
    controller.add(e)
  }
  def add(m: Module) : Unit = {
    controller.add(m)
  }
  def addFront(inc: Structure) : Unit = {
    controller.add(inc, AtBegin)
  }

  def addDeclaration(e: Declaration with HasType with HasDefiniens with HasNotation)(implicit defContext: DefinitionContext) : Unit = {
    val hasNotation = e.notC.isDefined
    try {
      addDependencies(e)
      controller.add(e)
      incrementNotationCounter (hasNotation)
    } catch {
      case e: AddError => throw e
      case er: Throwable =>
        val errClass = er.getClass.toString.split('.').last
        throw new TranslatingError(errClass+" while processing the dependencies of the declaration "+e.path.toPath+":\n"+er.getMessage)
    }
    try {
      if (typecheckContent && checkConstants) typecheckContent(e)
      if (e.feature == "instance") {
        controller.simplifier(e)
        // build the notations
        currentThy.domain.filter(_.init == e.name)
          .filterNot(d => e.not.map(_.toText.contains(d.toString)).getOrElse(true))
          .filter(currentThy.get(_).asInstanceOf[Constant].not == e.not).map({
          n =>
            val toPresent = ApplyGeneral(OMS(currentTheoryPath ? n), defContext.args.map(_.toTerm))
            val not = controller.presenter.asString(toPresent)
            if (not.contains(n.toString)) {
              println("notation for constant "+(currentTheoryPath ? n).toString+" cannot be used.")
            }
        })
      }
    } catch {
      case _: AddError =>
        throw new TranslatingError("error adding declaration "+e.name+", since a declaration of that name is already present. ")
      case er: Throwable =>
        println("Error while typechecking: "+e.toString)
        try{ println(controller.presenter.asString(e)) } catch { case e: Throwable => }
        throw er
    }
  }
  def makeConstant(n: LocalName, t: Term) : Constant = makeConstant(n, Some(t), None)
  def makeConstant(n: LocalName, tO: Option[Term], dO: Option[Term])(implicit notC:NotationContainer = NotationContainer.empty(), role: Option[String] = None) : Constant = {
    Constant(OMMOD(currentTheoryPath), n, Nil, tO, dO, None, notC)
  }
  def makeConstantInContext(n: LocalName, tO: Option[Term], dO: Option[Term], unboundArgs: Context)(implicit notC:NotationContainer) : Constant = {
    implicit val args = unboundArgs.map(vd => vd.copy(tp = vd.tp map (lambdaBindArgs(_)(unboundArgs map (_.toTerm))))) map (_.tp.get)
    makeConstant(n, tO map(piBindArgs(_)), dO map(lambdaBindArgs(_)))
  }
  def makeConstantInContext(n: LocalName, tO: Option[Term], dO: Option[Term] = None)(implicit notC:NotationContainer = NotationContainer.empty(), defContext: DefinitionContext): Constant =
    makeConstantInContext(n, tO, dO, defContext.args)

  def simplifyTerm(tm:Term): Term = {
    val su = SimplificationUnit(Context.empty,true,false)
    val rules = RuleSet.collectRules(controller, su.context)
    controller.simplifier.objectLevel(tm,su, rules)
  }

  def inferType(tm:Term)(implicit defContext: DefinitionContext): Term = {
    try {
      checking.Solver.infer(controller, defContext.args++defContext.getLocalBindingVars, tm, None).getOrElse(any)
    } catch {
      case e: LookupError =>
        println("Variable not declared in context. ")
        throw e
    }
  }
}