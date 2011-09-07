package info.kwarc.mmt.tptp

import scala.collection.mutable._
import scala.collection.JavaConversions._
import tptp.SimpleTptpParserOutput.Formula._
import tptp.SimpleTptpParserOutput.{TopLevelItem,Formula,AnnotatedFormula,IncludeDirective}
import java.io.File
import tptp.TptpParserOutput.TptpInput
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

class TptpTranslator {

  val imports = new HashSet[String]()
  
  var currDoc: DPath = null
  var currTh: MPath = null

  def this(theory: String) = {
    this ()
    if (theory.endsWith(".ax") && !TptpUtils.fileVersion(theory).equals("0"))
      imports.add(theory.replaceFirst("\\+\\d+", "+0"))
    val d = new Document(new DPath(TptpUtils.baseURI))
    TptpTranslator.add(d)
    currDoc = d.path
    val t = new DeclaredTheory(d.path, LocalPath(theory::Nil), None)
    currTh = t.path
    TptpTranslator.add(t)
  }

  def translate(item: TopLevelItem): Option[Term] = {
    if (item.getKind().equals(TptpInput.Kind.Formula))
      translate(item.asInstanceOf[AnnotatedFormula])
    else if (item.getKind().equals(TptpInput.Kind.Include))
      translate(item.asInstanceOf[IncludeDirective])
    else
      error(item)
  }

  def translate(item: IncludeDirective): Option[Term] = {
    val from = item.getFileName().substring(item.getFileName().indexOf("/") + 1);
//    val mfrom = MPath(DPath(xml.URI(item.getFileName)), LocalPath(TptpUtils.removeExtension(from)));
//    imports.add(mfrom, );
//    PlainImport()
    None
  }

  def translate(item: AnnotatedFormula): Option[Term] = {
    item.getFormula match {
      case f: Formula => translate(f)
      case _ => error(item.getFormula)
    }
  }

  def translate(item: Formula): Option[Term] = {
    println("Translating Formula")
    item.getKind match {
      case Kind.Atomic => translate(item.asInstanceOf[Atomic])
      case Kind.Negation => translate(item.asInstanceOf[Negation])
      case Kind.Binary => translate(item.asInstanceOf[Binary])
      case Kind.Quantified => translate(item.asInstanceOf[Quantified])
      case _ => None
    }
  }

  def translate(item: Atomic): Option[Term] = {
    println("Translating Atomic")
    val omid = OMID(currTh ? item.getPredicate)
    if (item.getNumberOfArguments == 0) {
      Some(omid)
    } else {
      var args: List[Term] = Nil
      for (arg <- item.getArguments)
        translate(arg) match {
          case Some(x) => args = x::args
          case None => error(arg)
        }
      Some(OMA(omid, args))
    }
  }

  def translate(item: tptp.SimpleTptpParserOutput.Term): Option[Term] = {
    println("Translating Term")
    if (item.getNumberOfArguments == 0) {
//      return translate(term.getTopSymbol());
    } else {
//      OMA ret = new OMA();
//      ret.setOperator(translate(term.getTopSymbol()));
//      for (Term arg : term.getArguments()) {
//        ret.addArgument(translate(arg));
//      }
//      return ret;
    }
    None
  }

  def translate(item: Negation): Option[Term] = {
    println("Translating Negation")
    None
  }

  def translate(item: Binary): Option[Term] = {
    println("Translating Binary")
    None
  }

  def translate(item: Quantified): Option[Term] = {
//    OMBIND ret = new OMBIND();
//    ret.setOperator(baseSymbol(OPERATORS.get(formula.getQuantifier())));
//    OMBVAR ombvar = new OMBVAR();
//    for (String v : formula.getVariables()) {
//      ombvar.addVariable(new OMV(v));
//    }
//    ret.setVariables(ombvar);
//    ret.setBody(translate(formula.getMatrix()));
//    return ret;
    OMBIND(TptpUtils.constant(TptpUtils.OPERATORS.get(formula.getQuantifier)),
           Context(TermVarDecl(v, Some(Mizar.any), None)),
           prop)
  }

  def error(item: Any): Option[Term] = {
    println("error", "Error translating " + item.toString)
    None
  }
}

object TptpTranslator {
	
  val controller  =  {
    val report = new FileReport(new java.io.File("tptp.log"))
    val checker = libraries.NullChecker // TODO use structural checker
    new frontend.Controller(checker,report)
  }
  
	def add(e : StructuralElement) {
		controller.add(e)
	}
}