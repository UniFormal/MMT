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
import mmtwrapper.{Mizar, MizarPatternInstance}

import scala.collection._

object TranslationController {


  var controller = {
    val c = new frontend.Controller
    //       c.setFileReport(File("mizar.log"))
    //       c.setCheckNone //c.setFoundChecker(new libraries.DefaultFoundation(controller.report))
    c
  }

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
  def incrementAnonymousTheoremCount() = {anonymousTheoremCount += 1}
  def getAnonymousTheoremCount() = anonymousTheoremCount

  private var identifyCount = 0
  def incrementIdentifyCount() = {identifyCount += 1}
  def getIdentifyCount() = identifyCount

  def currentBaseThy : Option[MPath] = Some(mmtwrapper.Mizar.MizarPatternsTh)
  def currentBaseThyFile = File("/home/user/Erlangen/MMT/content/MathHub/MMT/LATIN2/source/foundations/mizar/"+mmtwrapper.Mizar.MizarPatternsTh.name.toString+".mmt")
  def localPath : LocalName = LocalName(currentAid.toLowerCase())
  def currentThyBase : DPath = currentOutputBase / localPath
    //DPath(utils.URI(TranslationController.currentOutputBase.toString + localPath.toString))
  def currentTheoryPath : MPath = {
    val res = currentThyBase ? localPath
    assert (res == currentThy.path)
    res
  }
  def getTheoryPath(aid: String) = (TranslationController.currentOutputBase / aid.toLowerCase()) ? aid.toLowerCase()
  def currentSource : String = mmtwrapper.Mizar.mathHubBase + "/source/" + currentAid.toLowerCase() + ".miz"

  def makeDocument() = {
    val doc = new Document(currentThyBase)
    currentDoc = doc
    controller.add(currentDoc)
    doc
  }
  def makeTheory() = {
    val thy = new Theory(currentThyBase, localPath, currentBaseThy, Theory.noParams, Theory.noBase)
    controller.add(thy)
    currentThy = thy
    identifyCount = 0
    anonymousTheoremCount = 0
    thy
  }
  def getDependencies() : List[MPath] = {
    var dependencies: List[MPath] = Nil
    def isDependency(p: String) = p contains currentOutputBase.toString
    def getDependencies(tm: Term) = tm.subobjects.map(_._2).filter({case OMS(p) => true case _ => false}).map({case OMS(p) => p})
      .filter(p=>isDependency(p.toString))
    currentThy.getDeclarations foreach {
      case decl: Declaration =>
        //val s = controller.presenter.asString(decl)
        val deps: List[GlobalName] = if (isDependency(decl.toString())) {
          decl match {
            case MizarPatternInstance(_, _, args) => args flatMap getDependencies
            case c: Constant =>
              List(c.tp, c.df).flatMap(_ map(List(_)) getOrElse(Nil)) flatMap getDependencies
            case _ => Nil
          }
        } else Nil
        val depThs = deps.map(_.module)
        dependencies = (dependencies ++ depThs).distinct
    }
    dependencies.filterNot(List(currentTheoryPath, TranslatorUtils.hiddenArt).contains _)
  }
  def includeDependencies() = {
    val includes = getDependencies() map(PlainInclude(_, currentTheoryPath))
    includes.reverse foreach {inc =>
      try {
        addFront(inc)
      } catch {
        case e: GetError =>
          addUnresolvedDependency(inc.from.toMPath)
          throw new TranslatingError("GetError while trying to include the dependent theory "+inc.from.toMPath+" of the translated theory "+inc.to.toMPath+": \n"+
          "Please make sure the theory is translated (build with mizarxml-omdoc build target) and try again. ")
      }
    }
  }
  def endMake() = {
    includeDependencies()
    controller.endAdd(currentThy)
    currentDoc.add(MRef(currentDoc.path, currentThy.path))
    controller.endAdd(currentDoc)
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
  def add(e : Declaration) : Unit = {
    val eC = complify(e)
    try {
      //println(eC.toString)
      //println(controller.presenter.asString(eC))
      controller.add(eC)
    } catch {
      case e: AddError =>
        throw new TranslatingError("error adding declaration "+eC.name+", since a declaration of that name is already present. ")
    }
  }
  private def complify(d: Declaration) = {
    val rules = RuleSet.collectRules(controller, Context(mmtwrapper.Mizar.MizarPatternsTh))
    foundations.IntroductionRule.allRules.foreach {rules.declares(_)}
    val complifier = controller.complifier(rules).toTranslator()
    try {
      d.translate(complifier,Context.empty)
    } catch {case e: Exception =>
      println("error while complifying instance " + d.path)
      d
    }
  }

  def makeConstant(n: LocalName, t: Term) : Constant =
    Constant(OMMOD(currentTheoryPath), n, Nil, Some(t), None, None)
  def makeConstant(n: LocalName, tO: Option[Term], dO: Option[Term])(implicit notC:NotationContainer = NotationContainer.empty()) : Constant =
    Constant(OMMOD(currentTheoryPath), n, Nil, tO, dO, None)

  def simplifyTerm(tm:objects.Term): objects.Term = {
    val su = SimplificationUnit(Context.empty,true,false)
    val rules = RuleSet.collectRules(controller, su.context)
    controller.simplifier.objectLevel(tm,su, rules)
  }

  def inferType(tm:objects.Term, ctx: Context = Context.empty): objects.Term = {
    checking.Solver.infer(controller, ctx, tm, None).getOrElse(Mizar.any)
  }
}