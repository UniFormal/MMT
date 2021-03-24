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
import info.kwarc.mmt.mizar.newxml.mmtwrapper.PatternUtils.{LambdaOrEmpty, PiOrEmpty, lambdaBindArgs, piBindArgs}
import mmtwrapper.MizarPrimitiveConcepts._
import mmtwrapper.{MizarPatternInstance, PatternUtils}

import java.io.PrintStream
import scala.collection._

object TranslationController {


  var controller = {
    val c = new frontend.Controller
    //       c.setFileReport(File("mizar.log"))
    //       c.setCheckNone //c.setFoundChecker(new libraries.DefaultFoundation(controller.report))
    c
  }
  private def structChecker = controller.extman.get(classOf[checking.Checker], "mmt").get
  private def structureSimplifier = controller.simplifier
  private def errFilter(err: Error): Boolean = {
      val badStrings = List(
        "INHABITABLE"
        , "ill-formed constant reference"
        , "is not imported into current context"
      )
    !badStrings.exists(err.toStringLong.contains(_))
  }
  private def typeCheckingErrHandler(logger: Option[frontend.Report]): ErrorHandler = new FilteringErrorHandler(new ErrorContainer(logger), errFilter(_))
  private def checkingEnvironment(logger: Option[frontend.Report]) = new CheckingEnvironment(TranslationController.structureSimplifier, typeCheckingErrHandler(logger), checking.RelationHandler.ignore, MMTTask.generic)
  def typecheckContent(e: StructuralElement, logger: Option[frontend.Report] = None) = structChecker(e)(checkingEnvironment(logger))

  var query : Boolean = false

  //new frontend.Controller(libraries.NullChecker, new FileReport(new java.io.File("mizar.log")))

  //set during translation
  var currentAid : String = null
  var currentDoc : Document = null
  var currentThy : Theory = null
  var currentOutputBase : DPath = null


  private var unresolvedDependencies : List[MPath] = Nil
  def addUnresolvedDependency(dep: MPath) = if (unresolvedDependencies.contains(dep)) {} else {unresolvedDependencies +:= dep}
  def getUnresolvedDependencies() = unresolvedDependencies

  private var anonymousTheoremCount = 0
  def incrementAndGetAnonymousTheoremCount() = {anonymousTheoremCount += 1; anonymousTheoremCount}
  def getAnonymousTheoremCount() = anonymousTheoremCount

  private var anonymousSchemeCount = 0
  def incrementAndGetAnonymousSchemeCount() = {anonymousSchemeCount += 1; anonymousSchemeCount}
  def getAnonymousSchemeCount() = anonymousSchemeCount

  private var identifyCount = 0
  def incrementAndGetIdentifyCount() = {identifyCount += 1; identifyCount}
  def getIdentifyCount() = identifyCount

  private var reduceCount = 0
  def incrementAndGetReduceCount() = {reduceCount += 1; reduceCount}
  def getReduceCount() = reduceCount

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
    def makeArticleStatistics: String = "Overall we translated "+totalNumDefinitions+" definitions, "+registrations+" registrations and "+totalNumTheorems+" statements from this article."
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
      case _ => throw new TranslatingError("unrecognised statistics counter to increment: "+kind)
    }
  }

  def currentBaseThy : Option[MPath] = Some(MizarPatternsTh)
  def currentBaseThyFile = File("/home/user/Erlangen/MMT/content/MathHub/MMT/LATIN2/source/foundations/mizar/"+MizarPatternsTh.name.toString+".mmt")
  def localPath : LocalName = LocalName(currentAid)
  def currentThyBase : DPath = currentOutputBase / localPath
    //DPath(utils.URI(TranslationController.currentOutputBase.toString + localPath.toString))
  def currentTheoryPath : MPath = currentThyBase ? localPath
  def getTheoryPath(aid: String) = (TranslationController.currentOutputBase / aid.toLowerCase()) ? aid.toLowerCase()
  def currentSource : String = mathHubBase + "/source/" + currentAid + ".miz"

  def makeDocument() = {
    currentDoc = new Document(currentThyBase)
    controller.add(currentDoc)
  }
  def makeTheory() = {
    currentThy = new Theory(currentThyBase, localPath, currentBaseThy, Theory.noParams, Theory.noBase)
    resolvedDependencies ++= List(currentTheoryPath, TranslatorUtils.hiddenArt)
    controller.add(currentThy)
  }
  private var resolvedDependencies: Set[MPath] = Set()
  def getDependencies = resolvedDependencies
  def getDependencies(decl: Declaration with HasType with HasDefiniens) = {
    def isDependency(p: String) = p contains currentOutputBase.toString
    def getDependencies(tm: Term): List[MPath] = tm match {
      case OMS(p) => if (isDependency(p.toString)) List(p.module) else Nil
      case OMBINDC(binder, context, scopes) => (binder::context.flatMap(_.tp):::scopes) flatMap getDependencies
      case OMA(fun, args) => fun::args flatMap getDependencies
      case OML(_, tp, df, _, _) => List(tp, df).flatMap(_ map getDependencies getOrElse Nil)
      case _ => Nil
    }
    if (isDependency(decl.toString())) {
      val dependencies = (decl match {
        case MizarPatternInstance(_, _, args) =>
          args flatMap getDependencies
        case c: Constant => List(c.tp, c.df).flatMap(_ map(List(_)) getOrElse(Nil)).flatMap(getDependencies)
        case _ => Nil
      }).filterNot (resolvedDependencies contains _) .distinct
      resolvedDependencies ++= dependencies
      dependencies
    } else Nil
  }
  def addDependencies(decl: Declaration with HasType with HasDefiniens): Unit = {
    val deps = getDependencies(decl)
    val includes = deps map(PlainInclude(_, currentTheoryPath))
    includes zip deps foreach {case (inc, dep) =>
      try {
        addFront(inc)
        structureSimplifier(dep)
      } catch {
        case e: GetError =>
          addUnresolvedDependency(inc.from.toMPath)
          throw new TranslatingError("GetError while trying to include the dependent theory "+inc.from.toMPath+" of the translated theory "+inc.to.toMPath+": \n"+
            "Please make sure the theory is translated (build with mizarxml-omdoc build target) and try again. ")
      }
    }
  }
  def endMake() = {
    controller.endAdd(currentThy)
    currentDoc.add(MRef(currentDoc.path, currentThy.path))
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

  private var withNotation = 0
  private var withoutNotation = 0
  private def withoutNotationProper = withoutNotation - articleStatistics.numRegistrs - articleStatistics.totalNumTheorems
  def notationStatistics = "We translated "+withNotation.toString+" declarations with notation and "+withoutNotation.toString+" declarations without, "+withoutNotationProper.toString+" of which aren't theorems and registrations (which are expected to have no notation)."

  def add(e : Declaration with HasType with HasDefiniens with HasNotation) : Unit = {
    try {
      var complificationSucessful = false
      val info = e.toString + "\nwith notations: " + (e.notC.getNotations(dim = Some(1)).map(f => f.toText))

      val eC: Declaration = try {
        //val s = controller.presenter.asString(e)
        val res = complify(e)
        complificationSucessful = true
        if (!e.notC.isDefined) {
          withoutNotation += 1
        } else {
          withNotation += 1
        }
        res
      } catch {
        case ge: GeneralError =>
          println("Uncomplified:" + controller.presenter.asString(e))
          var shouldWork = false
          try {
            controller.presenter.asString(e);
            shouldWork = true
          } catch {
            case _: Throwable =>
          }
          if (shouldWork) e else throw ge
        case parseError: ParseError => println(info + "\n" + parseError.shortMsg); e //; throw parseError
      }
      //      if (complificationSucessful) println("Complified: "+controller.presenter.asString(eC))
      addDependencies(e)
      controller.add(eC)
    } catch {
      case e: AddError => throw e
  }
    try {
      if (e.feature == "instance") controller.simplifier(e)
      typecheckContent(e)
    } catch {
      case ae: AddError =>
        throw new TranslatingError("error adding declaration "+e.name+", since a declaration of that name is already present. ")
      case ge: GeneralError =>
        throw ge
    }
  }
  private def complify(d: Declaration) = {
    val rules = RuleSet.collectRules(controller, Context(MizarPatternsTh))
    foundations.IntroductionRule.allRules.foreach {rules.declares(_)}
    val complifier = controller.complifier(rules).toTranslator()
    try {
      d.translate(complifier,Context.empty)
    } catch {case e: Exception =>
      println("error while complifying instance " + d.path+": ")
      println(d.name.toString+": "+(d match {case c: Constant => c.tp.map(_.toStr(true)).getOrElse("") case _ => ""})+"\n = "+(d match {case c: Constant => c.df.map(_.toStr(true)).getOrElse("") case _ => ""}))
      //println(controller.presenter.asString(d))
      throw e
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