package info.kwarc.mmt.mizar.translator

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.parser.{SourceRef, SourceRegion}
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.mizar._
import info.kwarc.mmt.api.checking.CheckingEnvironment
import info.kwarc.mmt.api.frontend.Report
import info.kwarc.mmt.lf._
import mmtwrapper._
import PatternUtils._
import MizarPrimitiveConcepts._
import info.kwarc.mmt.api.presentation.{MMTSyntaxPresenter, Presenter}
import syntax.Utils._
import translator.TranslationController.printTimeDiff

import java.io.{EOFException, PrintStream}
import scala.collection._

object TranslationController extends frontend.Logger {
  def logPrefix: String = "mizarxml-omdoc"

  def logString(message: String) = { log (message) }
  var controller = {
    val c = new frontend.Controller
    c
  }
  //set during translation
  var outputBase: DPath = null
  private var translatorReport: frontend.Report = null
  def setReport (report: frontend.Report): Unit = { this.translatorReport = report }
  override def report: Report = this.translatorReport
  private def structChecker = controller.extman.get(classOf[checking.Checker], "mmt").get
  private def structureSimplifier = controller.simplifier
  private var buildArticles: immutable.Set[MPath] = immutable.Set()
  def addBuildArticles(mpath: MPath): Unit = { buildArticles += mpath }
  def getBuildArticles = buildArticles

  /**
   * whether to typecheck translated content
   */
  private var typecheckContent: Boolean = true
  private val recurseOnlyWhenNeeded = false//true
  /**
   * whether to typecheck constants or just to check the entire theory at the end
   */
  private var checkConstants: Boolean = true
  def setCheckConstants(checkConstants: Boolean, typecheckContent: Boolean): Unit = {
    this.checkConstants = checkConstants; this.typecheckContent = typecheckContent
  }

  private var processDependency: String => Unit = { (m: String) => }
  def setProcessDependency(processDependency: String => Unit): Unit = { this.processDependency = processDependency }
  def processDependencyTheory(mpath: MPath) = {
    lazy val aid = mpath.name.toString.toLowerCase
    if (mpath.doc.^! == outputBase) {
      if (! (TranslationController.isBuild(aid) || mpath == currentTheoryPath)) processDependency (aid)
    }
  }
  private def errFilter(err: Error): Boolean = {
    val badStrings = List(
      "INHABITABLE"
      , "is not imported into current context"
    ) ++ (if (!checkConstants) List(
      "ill-formed constant reference"
      , "a declaration for the name "
    ) else Nil)
    def filterTypes(error: Error): Boolean = error match {
      case _: InvalidUnit => true
      case _: InvalidObject => true
      case _: AddError => ! checkConstants
      case _ => false
    }
    !(badStrings.exists(err.toStringLong.contains(_)) || filterTypes(err))
  }
  def showErrorInformation(e: Throwable, whileDoingWhat: String): String = {
    val errType = e.getClass.toString.split('.').last
    val causeO = e match {
      case er: Error => er.getCausedBy
      case _ if e.getCause == null => None
      case _ if e.getCause == e => None
      case _ => Some(e.getCause)
    }
    val cause: String = causeO map {
      case e => "\ncaused by: \n"+showErrorInformation(e, "")
    } getOrElse ""
    ("Error of type "+errType+whileDoingWhat+":\n"+e.getMessage+cause)
  }

  private def typeCheckingErrHandler: ErrorHandler = {
    val fe = new FilteringErrorHandler(new ErrorContainer) {
      def filter(e: Error) = errFilter(e)
    }
    MultipleErrorHandler(List(fe), this.translatorReport)
  }

  private def checkingEnvironment = new CheckingEnvironment(TranslationController.structureSimplifier, typeCheckingErrHandler, checking.RelationHandler.ignore, MMTTask.generic)

  def typecheckContent(e: StructuralElement) = structChecker(e)(checkingEnvironment)

  var articleDependencyParents = List[MPath]()
  def presenter = controller.presenter//controller.extman.get(classOf[MMTSyntaxPresenter]).find(_.key.startsWith("present-text-notations")).get

  val beginningTime: Long = System.nanoTime()
  var globalParsingTime: Long = 0
  private var globalTranslatingTime: Long = 0
  def timeSince(beginning: Long): Long = System.nanoTime() - beginning
  def showTimeDiff(nanoDiff: Long): String = {
    val diffTime = math.round(nanoDiff / 1e9d)
    val seconds = diffTime % 60
    val minutesAll = (diffTime - seconds) / 60
    val minutes = minutesAll % 60
    val hours = (minutesAll - minutes) / 60
    (if (hours >0) hours.toString+" hours " else "")+minutes.toString + " minutes and " + seconds.toString + " seconds. "
  }
  def printTimeDiff(nanoDiff: Long, prefix: String = "It took "): Unit = {
    println(prefix + showTimeDiff(nanoDiff))
  }
  def globalTotalTime: Long = timeSince(beginningTime)
  def unaccountedTotalTime: Long = globalTotalTime - (globalParsingTime + globalTranslatingTime)
  var globalTranslatedDeclsCount = 0
  def globalTranslatedArticlesCount = buildArticles.size
  class ArticleSpecificData {
    //set during translation
    var currentAid: String = null
    var currentDoc: Document = null
    var currentThy: Theory = null

    private var currentTranslatingTime: Long = 0
    def getCurrentTranslatingTime = currentTranslatingTime
    private var currentTranslatingTimeBegin = System.nanoTime()
    def resetCurrenTranslatingTimeBegin = { currentTranslatingTimeBegin = System.nanoTime() }
    def addCurrenTranslatingTime = {
      currentTranslatingTime += timeSince(currentTranslatingTimeBegin)
      resetCurrenTranslatingTimeBegin
    }
    def addCurrentToGlobalTranslatingTime = {
      addCurrenTranslatingTime
      globalTranslatingTime += currentTranslatingTime
      currentTranslatingTime = 0
    }

    private var anonymousTheoremCount = 0

    def incrementAndGetAnonymousTheoremCount() = {
      anonymousTheoremCount += 1; anonymousTheoremCount
    }

    def getAnonymousTheoremCount() = anonymousTheoremCount

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

    private var resolvedDependencies: immutable.Set[MPath] = immutable.Set()

    def getDependencies = resolvedDependencies
    def addDependencies(additionalDependencies: immutable.Set[MPath]): Unit = resolvedDependencies ++= additionalDependencies
    def resetDependencies: Unit = {resolvedDependencies = immutable.Set(currentTheoryPath, TranslatorUtils.hiddenArt)}

    object articleStatistics {
      private val showProgress = true
      private var numLegitimateTypingIssues = 0
      def incrementNumLegitimateTypingIssues = { numLegitimateTypingIssues += 1 }
      def totalNumDefinitions: Int = functorDefinitions + predicateDefinitions + attributeDefinitions + modeDefinitions + schemeDefinitions + structureDefinitions
      def grandTotal: Int = totalNumDefinitions + totalNumTheorems + numRegistrs
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

      private def showTime(timeDiff: Long, whatFor: String) = whatFor+showTimeDiff (timeDiff)+"\n"
      private def makeFinalStatistics: String = {
        (showTime (globalParsingTime, "The parsing overall took ")
        +showTime (globalTranslatingTime, "The translating overall took ")
        +showTime (unaccountedTotalTime, "Time spend on anything other than parsing and translating "))
      }
      def makeArticleStatistics: String = {
        val numTotalDecls: Long = 22100000
        val numTotalArticles = 1391
        def estimatedRatioTranslated = (globalTranslatedDeclsCount / numTotalDecls.toDouble + globalTranslatedArticlesCount / numTotalArticles.toFloat) / 2.0
        ("From this article, we translated " + grandTotal + " declarations, namely "+ totalNumDefinitions + " definitions, " + registrations + " registrations and " + totalNumTheorems + " statements.\n"
        +showTime (globalTotalTime, "Overall the entire import took ")
        +"Overall we translated "+globalTranslatedDeclsCount+" declarations from "+globalTranslatedArticlesCount+" articles so far."
        +(if (numLegitimateTypingIssues > 0) "\nThere were "+numLegitimateTypingIssues+" legitimate typing issues found. " else "")
        +(if (globalTranslatedArticlesCount % 10 == 0 && showProgress) "\nEstimated percentage translated: "+(100.0*estimatedRatioTranslated).toString+"%. " else "")
        +(if (globalTranslatedArticlesCount % 10 == 0 && showProgress && globalTranslatedDeclsCount > 0) "\nEstimated time remaining for the import: "+showTimeDiff(globalTotalTime*(1/estimatedRatioTranslated - 1).round) else "")
        +(if (globalTranslatedArticlesCount % 10 ==  0) "\n"+makeFinalStatistics else "")
        +(if (globalTranslatedArticlesCount == numTotalArticles ) "\n\n"+makeFinalStatistics else "")
        )
      }

      def incrementNonDefinitionStatisticsCounter(implicit kind: String): Unit = kind match {
        case "scheme" => schemeDefinitions += 1
        case "nym" => nyms += 1
        case "registr" => registrations += 1
        case "thm" => theorems += 1
        case _ => throw ImplementationError ("unrecognised statistics counter to increment: " + kind)
      }
      def incrementDefinitionStatisticsCounter(implicit kind: DeclarationKinds): Unit = kind match {
        case FunctorKind() => functorDefinitions += 1
        case PredicateKind() => predicateDefinitions += 1
        case AttributeKind() => attributeDefinitions += 1
        case ModeKind() => modeDefinitions += 1
        case StructureKind() => structureDefinitions += 1
      }
    }
  }
  private[mizar] var articleData = new ArticleSpecificData
  def getArticleData = { articleData.addCurrenTranslatingTime; articleData }
  def resetArticleData: Unit = { articleData = new ArticleSpecificData }
  def setArticleData(articleSpecificData: ArticleSpecificData): Unit = {
    articleData = articleSpecificData
    articleData.resetCurrenTranslatingTimeBegin
  }
  def currentTheory = articleData.currentThy
  def locallyDeclared(ln: LocalName): Boolean = currentTheory.declares(ln)
  def locallyDeclared(gn: GlobalName): Boolean = locallyDeclared(gn.name)
  def currentAid = articleData.currentAid

  def currentBaseThy : Option[MPath] = Some(MizarPatternsTh)
  def currentBaseThyFile = File("/home/user/Erlangen/MMT/content/MathHub/MMT/LATIN2/source/foundations/mizar/"+MizarPatternsTh.name.toString+".mmt")
  def localPath = LocalName(articleData.currentAid)
  def currentThyBase : DPath = outputBase / localPath
  def currentTheoryPath : MPath = currentThyBase ? localPath
  def getTheoryPath(aid: String): MPath = (outputBase / aid.toLowerCase()) ? aid.toLowerCase()
  def getPath(aid: String, name: String): GlobalName = getTheoryPath(aid) ? name
  def getLocalPath(name: String): GlobalName = currentTheoryPath ? name
  def currentSource : String = mathHubBase + "/source/" + articleData.currentAid + ".miz"
  def currentSourceUri: URI = URI(currentSource)

  def makeDocument() = {
    articleData.resetDependencies
    articleData.currentDoc = new Document(currentThyBase)
    controller.add(articleData.currentDoc)
  }
  def makeTheory() = {
    articleData.currentThy = new Theory(currentThyBase, localPath, currentBaseThy, Theory.noParams, Theory.noBase)
    controller.add(currentTheory)
  }
  def getDependencies(decl: Declaration with HasType with HasDefiniens) = {
    def isPotentialDependency(p: String) =
      p contains outputBase.toString
    def getDependencies(tm: Term): immutable.Set[MPath] = tm match {
      case OMS(GlobalName(mod, _)) =>
        val aid = mod.name.toString
        if (getTheoryPath(aid) == mod && !(articleData.getDependencies contains mod)) immutable.Set(mod) else immutable.Set()
      case OMBINDC(binder, context, scopes) => (binder::context.flatMap(_.tp):::scopes).toSet flatMap getDependencies
      case OMA(fun, args) => (fun::args).toSet flatMap getDependencies
      case OML(_, tp, df, _, _) => immutable.Set(tp, df).flatMap(_ map getDependencies getOrElse immutable.Set())
      case _ => immutable.Set()
    }
    val dependencies: immutable.Set[MPath] = (decl match {
      case MizarPatternInstance(_, _, args) =>
        args.toSet flatMap getDependencies
      case c: Constant => immutable.Set(c.tp, c.df).flatMap(_ map(immutable.Set(_)) getOrElse(Nil)).flatMap(getDependencies)
      case _ => immutable.Set()
    })
    articleData.addDependencies(dependencies)
    dependencies
  }
  def isBuild(aid: String) = {
    val mpath = getTheoryPath(aid)
    try {
      if (getBuildArticles.contains(mpath)) true else controller.getO(mpath) match {
      case Some(t: Theory) =>
        ((controller.getTheory(currentTheoryPath).parentDoc map (controller.getDocument(_))) match {case Some(_:Document) => true case _ => false}) && t.domain.exists {n=>
          if (List("funct", "pred", "attribute", "mode").contains(n.steps.last.toString)) {
            t.domain.contains(n.init)
          } else false
        }
      case _ => false
    }} catch {
      //in case that the lookups lead to parsing of malformed xml content we have to rebuild
      case _: Throwable => false
    }
  }
  def addDependencies(decl: Declaration with HasType with HasDefiniens): Unit = {
    val deps = getDependencies(decl)
    val includes = deps map(PlainInclude(_, currentTheoryPath))
    includes zip deps foreach {case (inc, dep) =>
      try {
        addFront (inc)
        if (! recurseOnlyWhenNeeded) processDependencyTheory (dep)
      } catch {
        case er: GetError =>
          val mes = showErrorInformation(er, " while trying to include the dependent theory "+dep+" of the theory "+inc.to.toMPath+" to be translated: ")
          throw new TranslatingError(mes)
        case er: Throwable if (!er.isInstanceOf[ImplementationError]) =>
          val mes = showErrorInformation(er, " while trying to translate the included dependent theory "+dep+" of the theory "+inc.to.toMPath+" to be translated:")
          throw new TranslatingError(mes)
      }
    }
  }
  def endMake() = {
    controller.endAdd(articleData.currentThy)
    articleData.currentDoc.add(MRef(articleData.currentDoc.path, articleData.currentThy.path))
    controller.endAdd(articleData.currentDoc)
    if (typecheckContent && ! checkConstants) typecheckContent(articleData.currentThy)
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

  def addDeclaration(e: Declaration with HasType with HasDefiniens with HasNotation, sourceRegion: Option[SourceRegion])(implicit defContext: DefinitionContext) : Unit = {
    try {
      addDependencies(e)
      //should be (and probably is unnecessary)
      if (currentAid != currentTheory.name.toString)
        articleData.currentAid = TranslationController.currentTheory.name.toString.toLowerCase
      if (! locallyDeclared(e.name)) {
        controller.add(e)
        sourceRegion foreach {
          sr => e.metadata.update(e.path, SourceRef(currentSourceUri, sr).toURI)
        }
      } else {
        log ("Trying to add a declaration twice. ")
      }
    } catch {
      case er: Throwable if (!er.isInstanceOf[ImplementationError]) =>
        val mes = showErrorInformation(er, " while processing the dependencies of the declaration "+e.path.toPath)
        throw new TranslatingError(mes)
    }
    try {
      if (typecheckContent && checkConstants) typecheckContent(e)
    } catch {
      case _: AddError =>
        throw new TranslatingError("error adding declaration "+e.name+", since a declaration of that name is already present. ")
      case er: MatchError if er.toString contains "scala.MatchError: (http://cds.omdoc.org/urtheories?LambdaPi?arrow " =>
        //we called one of the pattern without any arguments and the typechecker gets confused about the type of the main declaration, which is
        // term^0 -> ...
        //however this is the legitimate translation and shouldn't be considered an error
      case eofe: EOFException =>
        val mes = showErrorInformation(eofe, " while typechecking: "+e.path.toPath)//(try{ presenter.asString(e) } catch { case e: Throwable => e.toString}))
        println ("This usually means that corrupted translated content files are present. Try deleting them and build again. ")
        println (eofe)
      case er: Error if (showErrorInformation(er, "").toLowerCase.contains("geterror")) =>
        val mes = showErrorInformation(er, " while typechecking: "+e.path.toPath)//(try{ presenter.asString(e) } catch { case e: Throwable => e.toString}))
        println (mes)
      case er: TranslatingError if (showErrorInformation(er, "").toLowerCase.contains("invalid state")) =>
        val mes = showErrorInformation(er, " while typechecking: "+e.path.toPath)//(try{ presenter.asString(e) } catch { case e: Throwable => e.toString}))
        println (mes)
        println ("Not sure what causes these kind of issues, but they seem to be related to issues building theory graphs for translated dependency theories. ")
      case er: Throwable =>
        val mes = showErrorInformation(er, " while typechecking: "+e.path.toPath)//(try{ presenter.asString(e) } catch { case e: Throwable => e.toString}))
        articleData.articleStatistics.incrementNumLegitimateTypingIssues
        println (mes)
        println ("This seems to be a genuine typing issue. ")
    }
    if (e.feature == "instance") {
      try {
        structureSimplifier(e)
        /*
        // unnecessary debugging code checking if notations are build and used correctly
        val externalDecls = currentTheory.domain.filter(_.init == e.name)
        if (externalDecls.isEmpty) {
          println ("No external declarations for the instance "+e.path+" even after calling the simplifier on it. \n"
          +"Subsequent references of them will therefore fail. ")
        }
        // build the notations
        val mainDecls = externalDecls.filterNot(d => e.not.forall(_.toText.contains(d.toString)))
          .filter(currentTheory.get(_).asInstanceOf[Constant].not == e.not)
        lazy val allowedWithoutNotation = List("identify", "reduce", "Reg")
        if (mainDecls.isEmpty && !allowedWithoutNotation.exists(s => e.name.toStr(true).contains(s))) {
          if (externalDecls.map(currentTheory.get(_)).find(_.asInstanceOf[Constant].rl == Some ("mainDecl")).isEmpty)
            println ("no main Declaration found for " + e.path.toPath)
        }*/
      } catch {
          case er: Throwable =>
            val mes = showErrorInformation(er, " while simplifying: "+e.path.toPath)
            throw new TranslatingError(mes)
      }
  }
  }
  def makeConstant(n: LocalName, t: Term) : Constant = makeConstant(n, Some(t), None)
  def makeReferencingConstant(n: LocalName, gn: GlobalName)(implicit kind: String, notC:NotationContainer = NotationContainer.empty()) = makeConstant(n, None,
    Some(OMS(gn / kind)))
  def makeConstant(n: LocalName, tO: Option[Term], dO: Option[Term])(implicit notC:NotationContainer = NotationContainer.empty(), role: Option[String] = None) : Constant = {
    Constant(OMMOD(currentTheoryPath), n, Nil, tO, dO, None, notC)
  }
  def makeConstantInContext(n: LocalName, tO: Option[Term], dO: Option[Term], unboundArgs: Context)(implicit notC:NotationContainer) : Constant = {
    implicit val args = unboundArgs.map(vd => vd.copy(tp = vd.tp map (lambdaBindArgs(_)(unboundArgs map (_.toTerm))))) map (_.tp.get)
    makeConstant(n, tO map(piBindArgs(_)), dO map(lambdaBindArgs(_)))
  }
  def makeConstantInContext(n: LocalName, tO: Option[Term], dO: Option[Term] = None)(implicit notC:NotationContainer = NotationContainer.empty(), defContext: DefinitionContext): Constant =
    makeConstantInContext(n, tO, dO, defContext.args)

  def inferType(tm:Term)(implicit defContext: DefinitionContext): Term = {
    try {
      checking.Solver.infer(controller, defContext.args++defContext.getLocalBindingVars, tm, None).getOrElse(any)
    } catch {
      case e: LookupError =>
        println ("Lookup error while trying to infer a type: \nVariable not declared in context: "+defContext.args.toStr(true)+defContext.getLocalBindingVars+"\n"+e.shortMsg)
        throw e
      case e: GeneralError =>
        val mes = showErrorInformation(e, " while trying to infer a type in the context of "+(defContext.args++defContext.getLocalBindingVars).toStr(true))
        println (mes)
        throw new TranslatingError(mes)
    }
  }
}