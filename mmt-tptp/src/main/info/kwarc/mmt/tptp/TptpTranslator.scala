package info.kwarc.mmt.tptp

import scala.xml.Node
import scala.collection.immutable._
import scala.collection.mutable.LinkedHashMap
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
import documents._
import utils._
import frontend._
import backend._
import symbols._
import libraries._
import modules._
import objects._
import parser._
import presentation._
import info.kwarc.mmt.lf._

/**
 * Translates a Java structure representing parsed TPTP formula (parts) to OMDoc.
 */
class TptpTranslator {
  
  /** imports in this file */
  private var imports = new HashSet[MPath]()
  /** function and predicate constants (name -> object) */
  private var constants = new LinkedHashMap[String, Constant]()
  /** translated formulae */
  private var translated: List[StructuralElement] = Nil
  
  /** errors encountered during translation */
  private var errors: List[SourceError] = Nil
  
  private var file: File = null
  private var theoryDir: String = ""
  private var theory: String = ""
  private var theoryPath: MPath = null
  private var theoryFile: String = ""
  
  /**
   * Translate TPTP file theory in the TPTP distribution dir theoryDir to OMDoc.
   */
  def translate(theoryDir: String, theory: String, file: File) {
    log("Translating file: " + theory)
    
    // handle document
    val d = new Document(new DPath(baseURI / theoryDir))
    
    // handle theory
    val t = new DeclaredTheory(d.path, LocalPath(theory::Nil), Some(fofTh))
    this.theoryPath = t.path
    try {
      TptpTranslator.controller.get(t.path)
      log("..." + t.path.toString + " already translated")
      return
    } catch {
      case _ => TptpTranslator.add(d); TptpTranslator.add(t)
    }
    
    this.theoryDir = theoryDir
    this.theory = theory
    this.file = file
    this.theoryFile = file.toJava.getPath
    
    // axiom files of version > 0 implicitly import the axiom file of version 0
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
        case e: AddError =>
          if (e.toString.endsWith("already exists")) {
            x match {
              case c: Constant => TptpTranslator.add(Constant(c.home,
                LocalName(c.name.last + "_rule"), None, c.tp, c.df, c.rl, c.not))
              case _ =>
                println("Error adding " + x.toString)
                println(e.toString)
            }
          } else {
            println("Error adding " + x.toString)
            println(e.toString)
          }
      }
    }
  }
  
  /**
   * Translate a TPTP formula given as a string to OMDoc.
   */
  def translateFormula(tptpFormula: String): Option[Term] = {
    log("Translating formula: " + tptpFormula)
    
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
    
    // call the right translating method, depending on the formula part
    parsed match {
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
        println("Can't translate to OMDoc: " + parsed.toString)
        None
    }
  }

  def translate(item: TopLevelItem): List[SourceError] = {
    log("Translating TopLevelItem " + item.toString)
    item.getKind match {
      case TptpInput.Kind.Formula =>
        translate(item.asInstanceOf[AnnotatedFormula]) match {
          case Some(x) => translated = x::translated
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
            val tp = item.getRole match {
              case Axiom | Conjecture => OMA(TptpUtils.t, x::Nil)
              case _ => x
            }
            Some(Constant(OMMOD(theoryPath),
                              LocalName(item.getName), None, Some(tp),
                              None, None, None))
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
    val pred = item.getPredicate
    if (pred.equals("=")) {
      id = TptpUtils.equals
      eq = true
    } else if (pred.equals("$true")) {
      id = TptpUtils.t
    } else if (pred.equals("$false")) {
      id = TptpUtils.f
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
            argTypes = TptpUtils.term::argTypes
          case None => error("Error translating argument", arg)
        }
      }
      if (!eq) {
        addConstant(item.getPredicate, TptpUtils.form::argTypes)
      }
      Some(OMA(id, args.reverse))
    }
  }
  
  def term(item: tptp.SimpleTptpParserOutput.Term): Option[Term] = {
    log("Translating Term " + item.toString)
    val sym = term(item.getTopSymbol)
    if (item.getNumberOfArguments == 0) {
      item.getTopSymbol match {
        case x: tptp.SimpleTptpParserOutput.Symbol =>
          if (!x.isVariable)
            addConstant(x.getText, Nil)
        case _ =>
      }
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
                argTypes = TptpUtils.term::argTypes
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
      Some(omid(item.getText))
    }
  }
  
  /**
   * Adds a constant name to the global constants map
   */
  def addConstant(name: String, argTypes: List[Term]) {
    var conType: Term = null
    if (argTypes != Nil)
      conType = OMA(LF.arrow, argTypes.reverse)
    else
      conType = TptpUtils.term
    if (!constants.contains(name)) {
      val con = Constant(OMMOD(theoryPath),
                             LocalName(name), None, Some(conType),
                             None, None, None)
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
          ctx = ctx ++ VarDecl(LocalName(v), None, None)
        }
        Some(OMBIND(constant(item.getQuantifier), ctx, x))
      case None => None
    }
  }

  def error(msg: String, item: Any): Option[StructuralElement] = {
    errors = CompilerError(SourceRef(FileURI(file),SourcePosition(-1,-1,-1).toRegion), msg + " " + item.toString::Nil, true)::errors
    None
  }
  
  def omid(name: String) = OMID(theoryPath ? name)
}

object TptpTranslator {
	
  val controller = {
    val con = new Controller
//    con.setFileReport(File("tptp.log"))
//    con.setCheckNone
    con.handleLine("catalog /home/dimitar/projects/mmt/src/mmt-tptp/trunk/locutor.xml")
    con.handleLine("archive add /home/dimitar/projects/cds/")
    con.handleLine("archive add /home/dimitar/projects/oaff/tptp")
    con
  }
  
  def add(e: StructuralElement) {
    controller.add(e)
  }
  
  def main(args:Array[String]) = {
    val translator = new TptpTranslator()
//    val res = translator.translateFormula("f(X,Y,$$z)")
//    println(res.toString)
    translator.translate("Axioms", "LCL007+0.ax", File("/home/dimitar/projects/oaff/tptp/source/Axioms/LCL007+0.ax"))
    println("Done.")
  }
}