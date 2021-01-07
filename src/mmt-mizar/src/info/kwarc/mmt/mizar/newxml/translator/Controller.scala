package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt.mizar.objects._
import info.kwarc.mmt.mizar.mmtwrappers._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.parser._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.presentation._
import info.kwarc.mmt.api.uom.SimplificationUnit

import scala.collection.mutable.ArrayStack
import info.kwarc.mmt.lf._
import info.kwarc.mmt.mizar.newxml.mmtwrapper

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

  def currentBaseThy : Option[MPath] = Some(mmtwrapper.Mizar.MizarPatternsTh)
  def currentBaseThyFile = File("/home/user/Erlangen/MMT/content/MathHub/MMT/LATIN2/source/foundations/mizar/"+mmtwrapper.Mizar.MizarPatternsTh.name.toString+".mmt")
  def localPath : LocalName = LocalName(currentAid.toLowerCase())
  def currentThyBase : DPath = TranslationController.currentOutputBase / localPath
  def currentTheoryPath : MPath = {
    val res = currentThyBase ? localPath
    assert (res == currentThy.path)
    res
  }
  def getTheoryPath(aid: String) = (TranslationController.currentOutputBase / aid.toLowerCase()) ? aid.toLowerCase()
  def currentSource : String = mmtwrapper.Mizar.mathHubBase + "/source/" + currentAid.toLowerCase() + ".miz"

  def makeDocument() = {
    val doc = new Document(currentThyBase, documents.ModuleLevel, None)
    controller.add(doc)
    currentDoc = doc
    doc
  }
  def makeTheory() = {
    val thy = new Theory(currentThyBase, localPath, currentBaseThy, Theory.noParams, Theory.noBase)
    controller.add(thy)
    currentThy = thy
    thy
  }
  def endMake() = {
    controller.endAdd(currentThy)
    controller.endAdd(currentDoc)
  }

  var anonConstNr = 0
  var defs = 0
  var justBlocks = 0
  var theorems = 0
  var notations = 0
  var regs = 0
  var schemes = 0
  var varContext : ArrayStack[Term] = new ArrayStack
  var locusVarContext : ArrayStack[Term] = new ArrayStack
  // consider/set/reconsider everywhere and let in proofs
  var constContext : mutable.HashMap[Int,Term] = mutable.HashMap()
  //deftheorems, lemmas, assume and others
  var propContext : mutable.HashMap[Int,Term] = mutable.HashMap()

  def clear() = {
    constContext = mutable.HashMap()
    propContext = mutable.HashMap()
    //controller.clear
    anonConstNr = 0
  }

  def getLmName(nrO : Option[Int]) : String = nrO match {
    case Some(nr) =>
      "Lm" + nr
    case None =>
      anonConstNr += 1
      "AnonLm" + anonConstNr
  }

  def add(e: NarrativeElement) {
    controller.add(e)
  }
  def add(m: Module) {
    controller.add(m)
  }
  def add(e : Declaration) {
    println(e.toString)
    //val eC = complify(e)
    controller.add(e)
  }
  private def complify(d: Declaration) = {
    val rules = RuleSet.collectRules(controller, Context(mmtwrapper.Mizar.MizarPatternsTh))
    org.omdoc.latin.foundations.mizar.IntroductionRule.allRules.foreach {rules.declares(_)}
    val complifier = controller.complifier(rules).toTranslator
    try {
      d.translate(complifier,Context.empty)
    } catch {case e: Exception =>
      println("error while complifying instance " + d.path)
      d
    }
  }

  def resolveVar(nr : Int) : Term = {
    varContext(varContext.length - nr)
  }

  def resolveLocusVar(nr : Int) : Term = {
    assert (locusVarContext.length >= nr,"TranslationController.resolveLocusVar " +
      "for number " + nr +" which is over the size of context " + locusVarContext)
    locusVarContext(locusVarContext.length - nr)
  }

  def addLocalProp(nrO : Option[Int]) : LocalName = nrO match {
    case Some(nr) =>
      val name = LocalName("p" + nr)
      propContext(nr) = OMV(name)
      name
    case _ => OMV.anonymous
  }

  def addGlobalProp(nrO : Option[Int], sName : String) = nrO match {
    case Some(nr) =>
      val name = LocalName(sName)
      propContext(nr) = OMID(mmtwrapper.MMTUtils.getPath(TranslationController.currentAid, name))
    case _ => None
  }

  def resolveProp(nr : Int) : Term = try {
    propContext(nr)
  } catch {
    case _ : Throwable =>
      println(propContext)
      throw new java.lang.Error("propContext lookup failed for " + nr)
  }

  def addGlobalConst(nr : Int, kind : String) : LocalName = {
    val name = LocalName(kind + nr)
    constContext(nr) = OMID(mmtwrapper.MMTUtils.getPath(TranslationController.currentAid, name))
    name
  }

  def addLocalConst(nr : Int) : LocalName = {
    val name = LocalName("c" + nr)
    constContext(nr) = OMV(name)
    name
  }

  def resolveConst(nr : Int) : Term = {
    if (query) {
      mmtwrapper.Mizar.apply(OMID(mmtwrapper.MMTUtils.getPath("qvar","const")), OMV("c" + nr.toString))
    } else {
      constContext(nr)
    }
  }

  def addQVarBinder() = {
    val name = "x" + varContext.length
    varContext.push(mmtwrapper.Mizar.apply(OMID(DPath(mmtwrapper.Mizar.mmlBase) ? "qvar" ? "qvar"), OMV(name)))
  }

  def addVarBinder(n : Option[String]) : String = n match {
    case Some(x) =>
      varContext.push(OMV(x))
      x
    case None =>
      val n = varContext.length
      val base = (n % 10 + 'a').toChar.toString
      val counter = (n / 10) match {
        case 0 => ""
        case n =>  n.toString
      }
      val name = base + counter
      varContext.push(OMV(name))
      name
  }

  def clearConstContext() = {
    constContext = mutable.HashMap()
  }

  def clearVarBinder() = {
    varContext.pop()
  }

  def clearVarContext() = {
    varContext = new ArrayStack()
  }

  def addLocusVarBinder(tm : Term) {
    locusVarContext.push(tm)
  }

  def addRetTerm(path: GlobalName) = {
    locusVarContext.length match {
      case 0 => locusVarContext.push(OMID(path))
      case _ => locusVarContext.push(mmtwrapper.Mizar.apply(OMID(path), locusVarContext .toSeq: _*))
    }
  }

  def clearLocusVarBinder() {
    locusVarContext.pop()
  }

  def clearLocusVarContext() {
    locusVarContext = new ArrayStack()
  }

  def getFreeVar() : String = {
    var i : Int = 0
    val totalContext = varContext.toList ::: locusVarContext.toList
    while (totalContext.contains(OMV("x" + i))) {
      i = i + 1
    }
    "x" + i
  }

  def makeConstant(n: LocalName, t: Term) : Constant =
    Constant(OMMOD(currentTheoryPath), n, Nil, Some(t), None, None)
  def makeConstant(n: LocalName, tO: Option[Term], dO: Option[Term]) : Constant =
    Constant(OMMOD(currentTheoryPath), n, Nil, tO, dO, None)
  def makeConstant(gn:info.kwarc.mmt.api.GlobalName, notC:NotationContainer, df: Option[objects.Term], tp:Option[objects.Term] = None) : Constant =
    Constant(OMMOD(gn.module), gn.name, Nil, tp, df, None, notC)

  def getNotation(kind : String, absnr : Int) : NotationContainer = {
    val lname = LocalName(kind + absnr.toString)
    val name = currentTheoryPath ? lname
    ParsingController.dictionary.getFormatByAbsnr(currentAid, kind, absnr) match {
      case Some(format) if format.symbol != null => //found a format so will add notation
        val argnr = format.argnr
        val leftargnr = format.leftargnr
        val sname = Delim(format.symbol.name)
        val args = 1.to(argnr).flatMap {x =>
          val d = if (x == leftargnr + 1) sname else Delim(",")
          d :: SimpArg(x) :: Nil
        }.toList match {
          case Nil => Nil
          case l => l.tail //to remove first extra delimiter (need args separated by delim)
        }
        val markers : List[Marker] = format.rightsymbol match {
          case None => //single delimiter
            if (leftargnr == 0) sname :: args
            else if (leftargnr == argnr) args ::: List(sname)
            else args
          case Some(rightsymbol) => // double delimiter
            val rsname = Delim(rightsymbol.name)
            sname :: (args ::: List(rsname)) //TODO, what if not 0 - argnr pair ?
        }
        val not = new TextNotation(Mixfix(markers), Precedence.integer(0), None)
        NotationContainer.apply(not)
      case x =>
        NotationContainer()
    }
  }

  def simplifyTerm(tm:objects.Term): objects.Term = {
    val su = SimplificationUnit(Context.empty,true,false)
    val rules = RuleSet.collectRules(controller, su.context)
    controller.simplifier.objectLevel(tm,su, rules)
  }

  def inferType(tm:objects.Term, ctx: Context = Context.empty): objects.Term = {
    checking.Solver.infer(controller, ctx, tm, None).getOrElse(Mizar.any)
  }
  def conforms(tpA:objects.Term, tpB: objects.Term) : Boolean = {
    tpA == tpB
  }
}