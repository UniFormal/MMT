package info.kwarc.mmt.tptp

import scala.collection.immutable._
import scala.collection.JavaConversions._

import tptp.SimpleTptpParserOutput
import tptp.SimpleTptpParserOutput.Formula._
import tptp.SimpleTptpParserOutput.{TopLevelItem,Formula,AnnotatedFormula,IncludeDirective}
import tptp.TptpParserOutput
import tptp.TptpParserOutput.TptpInput
import tptp.TptpParserOutput.FormulaRole
import tptp.TptpParserOutput.FormulaRole._

import info.kwarc.mmt.tptp.TptpUtils._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.backend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.presentation._
import info.kwarc.mmt.api.lf._

class TptpTranslator {
  
  // imports in this file
  var imports = new HashSet[MPath]()
  // function and predicate constants (name -> object)
  var constants = new scala.collection.mutable.LinkedHashMap[String, Constant]()
  // translated formulae
  var translated: List[StructuralElement] = Nil
  
  // errors encountered during translation
  var errors: List[CompilerError] = Nil
  
  var file: File = null
  var theoryDir: String = ""
  var theory: String = ""
  var theoryPath: MPath = null
  var theoryFile: String = ""
  
  val LOG = false
  
  def translate(theoryDir: String, theory: String, file: File) {
    // handle document
    val d = new Document(new DPath(baseURI / theoryDir))
    
    // handle theory
    val t = new DeclaredTheory(d.path, LocalPath(theory::Nil), Some(fofTh))
    this.theoryPath = t.path
    try {
      TptpTranslator.controller.get(t.path)
      log("..." + t.path.toString + " already translated")
      return // the document has already been translated
    } catch {
      case e: info.kwarc.mmt.api.backend.NotFound =>
        TptpTranslator.add(d)
        TptpTranslator.add(t)
    }
    
    this.theoryDir = theoryDir
    this.theory = theory
    this.file = file
    this.theoryFile = file.toJava.getPath
    
    if (theory.endsWith(".ax") && !TptpUtils.fileVersion(theory).equals("0")) {
      val newTheory = theory.replaceFirst("\\+\\d+", "+0")
      val translator = new TptpTranslator()
      val targetBase = theoryFile.substring(0, theoryFile.indexOf(theory))
      val targetPath = targetBase + newTheory
      log("...importing/translating " + targetPath)
      translator.translate(theoryDir, newTheory, File(targetPath))
      imports += MPath(d.path, LocalPath(newTheory::Nil))
    }
    
    // parses the input file
    val parser = new TptpParser(file.toJava)
    
    // parameterRenaming.newTheory();
    var input: TptpParserOutput.TptpInput = parser.parseNext
    while (input != null) {
      input match {
        case tli: SimpleTptpParserOutput.TopLevelItem =>
          errors = errors:::translate(tli)
        case _ => println("Error, unknown input")
      }
      input = parser.parseNext
    }
    
    // add imports
    imports.foreach { x => TptpTranslator.add(PlainInclude(x, theoryPath)) }
    // add predicate/function constants
    constants.foreach { case (k,v) => TptpTranslator.add(v) }
    // add translated formulae
    translated.reverse.foreach { x =>
      try {
        TptpTranslator.add(x)
      } catch {
        case _ => log("Error adding to controller " + x.toString)
      }
    }
  }
  
  /**
   * Translates a TPTP formula given as a string, and returns the reference
   * to the resulting theory
   */
  def translate(tptpFormula: String): Option[Term] = {
    // handle document
    this.theoryDir = UNKNOWN
    this.theory = UNKNOWN
    val d = new Document(new DPath(baseURI / theoryDir))
    val t = new DeclaredTheory(d.path, LocalPath(theory::Nil), Some(fofTh))
    this.theoryPath = t.path
    
    val parser = new TptpParser(tptpFormula)
    val parsed = parser.parseString
    if (parsed == null) {
      println("Failed parsing " + tptpFormula)
      return None
    }
    val res = parsed match {
      case x: Quantified => term(x)
      case x: Negation => term(x)
      case x: Binary => term(x)
      case x: tptp.SimpleTptpParserOutput.Term => term(x)
      case x: tptp.SimpleTptpParserOutput.Symbol => term(x)
      case x: Atomic => term(x)
      case x: Formula => term(x)
      case x: AnnotatedFormula => term(x.getFormula)
      case x: TopLevelItem => term(x.asInstanceOf[AnnotatedFormula].getFormula)
      case _ =>
        println("Failed translating to OMDoc: " + parsed.toString)
        return None
    }
    res match {
      case Some(x: Term) => Some(x)
      case _ => 
        println("Failed translating to OMDoc: " + parsed.toString)
        return None
    }
  }

  def translate(item: TopLevelItem): List[CompilerError] = {
    log("Translating TopLevelItem " + item.toString)
    item.getKind match {
      case TptpInput.Kind.Formula =>
        translate(item.asInstanceOf[AnnotatedFormula]) match {
          case Some(x) => log(x.toString)
            translated = x::translated
          case None => None // an error has already been reported
        }
      case TptpInput.Kind.Include => translate(item.asInstanceOf[IncludeDirective])
      case _ => error("Unknown input type, can not translate", item)
    }
    errors
  }

  def translate(item: IncludeDirective) {
    log("Translating IncludeDirective " + item.toString)
    val included = item.getFileName.substring(1, item.getFileName.length - 1)
    val theoryDir = included.substring(0, included.lastIndexOf("/"));
    val theoryName = included.substring(included.lastIndexOf("/") + 1);
    val targetTheory = MPath(DPath(baseURI / theoryDir), LocalPath(theoryName::Nil))
    imports += targetTheory

    // translate the target theory (if it hasn't been already)'
    val translator = new TptpTranslator()
    val targetBase = theoryFile.substring(0, theoryFile.indexOf(this.theoryDir))
    val targetPath = targetBase + theoryDir + "/" + theoryName
    log("...importing/translating " + targetPath)
    translator.translate(theoryDir, theoryName, File(targetPath))
  }

  def translate(item: AnnotatedFormula): Option[StructuralElement] = {
    log("Translating AnnotatedFormula " + item.toString)
    item.getFormula match {
      case f: Formula =>
        term(f) match {
          case Some(x) => 
            // Axiom, Hypothesis, Definition, Lemma, Theorem, Conjecture,
            // LemmaConjecture, NegatedConjecture, Plain, FiDomain, FiFunctors,
            // FiPredicates, Unknown;
            val tp = item.getRole match {
              case Axiom | Conjecture => OMA(constant(TRUE), x::Nil)
              case _ => x
            }
            Some(new Constant(OMMOD(theoryPath),
                              LocalName(item.getName), Some(tp),
                              None, Individual(None)))
          case None => None
        }
      case _ => error("Unknown formula type, can not translate", item.getFormula)
    }
  }

  def term(item: Formula): Option[Term] = {
    log("Translating Formula " + item.toString)
    item.getKind match {
      case Kind.Atomic => term(item.asInstanceOf[Atomic])
      case Kind.Negation => term(item.asInstanceOf[Negation])
      case Kind.Binary => term(item.asInstanceOf[Binary])
      case Kind.Quantified => term(item.asInstanceOf[Quantified])
      case _ => None
    }
  }
  
  def term(item: Atomic): Option[Term] = {
    log("Translating Atomic " + item.toString)
    var id: OMID = null
    var eq = false
    if (item.getPredicate.equals("=")) {
      id = OMID(fofTh ? "=")
      eq = true
    } else
      id = omid(item.getPredicate)
    
    if (item.getNumberOfArguments == 0) {
      Some(id)
    } else {
      var args: List[Term] = Nil
      var argTypes: List[Term] = Nil
      for (arg <- item.getArguments) {
        term(arg) match {
          case Some(x) => 
            args = x::args
            argTypes = funType::argTypes
          case None => error("Error translating argument", arg)
        }
      }
      if (!eq) {
        addConstant(item.getPredicate, TptpUtils.form::argTypes)
      }
      Some(OMA(id, args.reverse))
    }
  }
  
  var funType: Term = null

  def term(item: tptp.SimpleTptpParserOutput.Term): Option[Term] = {
    log("Translating Term " + item.toString)
    funType = null
    val sym = term(item.getTopSymbol)
    if (item.getNumberOfArguments == 0) {
      funType = TptpUtils.term
      sym
    } else {
      sym match {
        case Some(x) => 
          var args: List[Term] = Nil
          var argTypes: List[Term] = Nil
          for (arg <- item.getArguments) {
            term(arg) match {
              case Some(x) =>
                args = x::args
                argTypes = funType::argTypes
              case None => error("Error translating argument", arg)
            }
          }
          addConstant(item.getTopSymbol.getText, TptpUtils.term::argTypes)
          Some(OMA(x, args.reverse))
        case None => None
      }
    }
  }
  
  def term(item: tptp.SimpleTptpParserOutput.Symbol): Option[Term] = {
    log("Translating Symbol " + item.toString)
    if (item == null)
      None
    else if (item.isVariable)
      Some(OMV(item.getText))
    else {
      val name = item.getText
      if (!constants.contains(name)) {
        val con = new Constant(OMMOD(theoryPath),
                               LocalName(name), Some(TptpUtils.term),
                               None, Individual(None))
        constants.put(name, con)
      }
      Some(omid(item.getText))
    }
  }
  
  def addConstant(name: String, argTypes: List[Term]) {
    funType = OMA(LF.arrow, argTypes.reverse)
    if (!constants.contains(name)) {
      val con = new Constant(OMMOD(theoryPath),
                             LocalName(name), Some(funType),
                             None, Individual(None))
      constants.put(name, con)
    }
  }

  def term(item: Negation): Option[Term] = {
    log("Translating Negation")
    term(item.getArgument) match {
      case Some(x) => Some(OMA(constant("not"), x::Nil))
      case None => None
    }
  }

  def term(item: Binary): Option[Term] = {
    log("Translating Binary " + item.toString)
    (term(item.getLhs), term(item.getRhs)) match {
      case (Some(l: Term), Some(r: Term)) => Some(OMA(constant(item.getConnective), l::r::Nil))
      case (_, _) => None
    }
  }

  def term(item: Quantified): Option[Term] = {
    log("Translating Quantified " + item.toString)
    term(item.getMatrix) match {
      case Some(x) =>
        var ctx: Context = Context()
        for (v <- item.getVariables) {
          ctx = ctx ++ TermVarDecl(v, None, None)
        }
        Some(OMBIND(constant(item.getQuantifier), ctx, x))
      case None => None
    }
  }

  def error(msg: String, item: Any): Option[StructuralElement] = {
    errors = CompilerError(Region(file,-1,-1,-1,-1), msg + " " + item.toString::Nil, true)::errors
    None
  }
  
  def omid(name: String): OMID = {
//    log("Creating OMID for " + name)
    OMID(theoryPath ? name)
  }
  
  def log(s: String) {
    if (LOG)
      println(s)
  }
}

object TptpTranslator {
	
  val controller = {
    val report = new FileReport(new java.io.File("tptp.log"))
    val checker = NullChecker //new StructuralChecker(report)
    new Controller(checker,report)
  }
  
  def add(e: StructuralElement) {
    controller.add(e)
  }
  
  def main(args:Array[String]) = {
    val translator = new TptpTranslator()
//    val res = translator.translate("fof(majority,axiom,(! [X,Y] : f(X,X,Y) = X )).")
//    println(res.toString)
    translator.translate("Axioms", "GEO006+1.ax", File("/home/dimitar/projects/oaff/tptp/source/Axioms/GEO006+1.ax"))
    println("Done.")
  }
}