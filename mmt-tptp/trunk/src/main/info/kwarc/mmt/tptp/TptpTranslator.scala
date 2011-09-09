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
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.presentation._

class TptpTranslator {
  
  // imports in this file
  var imports = new HashSet[MPath]()
  // constants to be added 
  var addConstants = new HashSet[String]()
  // errors encountered during translation
  var errors: List[CompilerError] = Nil
  
  var file: File = null
  var theoryDir: String = ""
  var theory: String = ""
  var theoryPath: MPath = null
  var theoryFile: String = ""
  
  def translate(theoryDir: String, theory: String, file: File) {
    // handle document
    val d = new Document(new DPath(baseURI / theoryDir))
    
    // handle theory
    val t = new DeclaredTheory(d.path, LocalPath(theory::Nil), None)
    this.theoryPath = t.path
    try {
      TptpTranslator.controller.get(t.path)
      println("..." + t.path.toString + " already translated")
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
      println("...importing/translating " + targetPath)
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
    
    // add constants
    addConstants.foreach { x =>
      val con = new Constant(OMMOD(theoryPath), LocalName(x), None, None, Individual(None))
      try {
        TptpTranslator.add(con)
      } catch {
        case e => None
      }
      TptpTranslator.addToSignature(x, theoryPath)
    }
    
    // imports
    imports.foreach { imported =>
      TptpTranslator.add(PlainInclude(imported, theoryPath))
    }
  }

  def translate(item: TopLevelItem): List[CompilerError] = {
//    println("Translating TopLevelItem")
    item.getKind match {
      case TptpInput.Kind.Formula =>
        translate(item.asInstanceOf[AnnotatedFormula]) match {
          case Some(x) => TptpTranslator.add(x)
          case None => None // an error has already been reported
        }
      case TptpInput.Kind.Include => translate(item.asInstanceOf[IncludeDirective])
      case _ => error("Unknown input type, can not translate", item)
    }
    errors
  }

  def translate(item: IncludeDirective) {
//    println("Translating IncludeDirective")
    val included = item.getFileName.substring(1, item.getFileName.length - 1)
    val theoryDir = included.substring(0, included.lastIndexOf("/"));
    val theoryName = included.substring(included.lastIndexOf("/") + 1);
    val targetTheory = MPath(DPath(baseURI / theoryDir), LocalPath(theoryName::Nil))
    imports += targetTheory

    // translate the target theory (if it hasn't been already)'
    val translator = new TptpTranslator()
    val targetBase = theoryFile.substring(0, theoryFile.indexOf(this.theoryDir))
    val targetPath = targetBase + theoryDir + "/" + theoryName
    println("...importing/translating " + targetPath)
    translator.translate(theoryDir, theoryName, File(targetPath))
  }

  def translate(item: AnnotatedFormula): Option[StructuralElement] = {
//    println("Translating AnnotatedFormula: " + item.getName)
    item.getFormula match {
      case f: Formula =>
        term(f) match {
          case Some(x) => 
            // Axiom, Hypothesis, Definition, Lemma, Theorem, Conjecture,
            // LemmaConjecture, NegatedConjecture, Plain, FiDomain, FiFunctors,
            // FiPredicates, Unknown;
            val tmp = item.getRole match {
              case Axiom | Conjecture => OMA(constant(TRUE), x::Nil)
              case _ => x
            }
            TptpTranslator.addToSignature(item.getName, theoryPath)
            Some(new Constant(OMMOD(theoryPath),
                              LocalName(item.getName), Some(tmp),
                              None, Individual(None)))
          case None => None
        }
      case _ => error("Unknown formula type, can not translate", item.getFormula)
    }
  }

  def term(item: Formula): Option[Term] = {
//    println("Translating Formula")
    item.getKind match {
      case Kind.Atomic => term(item.asInstanceOf[Atomic])
      case Kind.Negation => term(item.asInstanceOf[Negation])
      case Kind.Binary => term(item.asInstanceOf[Binary])
      case Kind.Quantified => term(item.asInstanceOf[Quantified])
      case _ => None
    }
  }

  def term(item: Atomic): Option[Term] = {
//    println("Translating Atomic")
    val id = omid(item.getPredicate)
    if (item.getNumberOfArguments == 0) {
      Some(id)
    } else {
      var args: List[Term] = Nil
      for (arg <- item.getArguments) {
        term(arg) match {
          case Some(x) => args = x::args
          case None => error("Error translating argument", arg)
        }
      }
      Some(OMA(id, args))
    }
  }

  def term(item: tptp.SimpleTptpParserOutput.Term): Option[Term] = {
//    println("Translating Term")
    val sym = term(item.getTopSymbol)
    if (item.getNumberOfArguments == 0) {
      sym
    } else {
      var args: List[Term] = Nil
      for (arg <- item.getArguments) {
        term(arg) match {
          case Some(x) => args = x::args
          case None => error("Error translating argument", arg)
        }
      }
      sym match {
        case Some(x) => Some(OMA(x, args))
        case None => None
      }
    }
  }
  
  def term(item: tptp.SimpleTptpParserOutput.Symbol): Option[Term] = {
//    println("Translating Symbol")
    if (item == null)
      None
    else if (item.isVariable)
      Some(OMV(item.getText))
    else
      Some(omid(item.getText))
  }

  def term(item: Negation): Option[Term] = {
//    println("Translating Negation")
    term(item.getArgument) match {
      case Some(x) => Some(OMA(constant("not"), x::Nil))
      case None => None
    }
  }

  def term(item: Binary): Option[Term] = {
//    println("Translating Binary")
    (term(item.getLhs), term(item.getRhs)) match {
      case (Some(l: Term), Some(r: Term)) => Some(OMA(constant(item.getConnective), l::r::Nil))
      case (_, _) => None
    }
  }

  def term(item: Quantified): Option[Term] = {
//    println("Translating Quantified")
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
//    println("Creating OMID for " + name)
    TptpTranslator.signature.get(name) match {
      case Some(x) =>
        val s = x.foreach {
          el => if (imports.contains(el))
            return OMID(el ? name)
        }
      case None => None
    }
    addConstants += name
    OMID(theoryPath ? name)
  }
}

object TptpTranslator {
  
  // all formulae
  val signature = new scala.collection.mutable.HashMap[String, HashSet[MPath]]()
	
  val controller = {
    val report = new FileReport(new java.io.File("tptp.log"))
    val checker = libraries.NullChecker // TODO use structural checker
    new Controller(checker,report)
  }
  
  def add(e: StructuralElement) {
    controller.add(e)
  }
  
  def addToSignature(name: String, theory: MPath) {
    var values: HashSet[MPath] = null
    signature.get(name) match {
      case Some(s) => values = s
      case None =>
        values = new HashSet() + theory
    }
    signature.put(name, values)
  }
}