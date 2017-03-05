package info.kwarc.mmt.LFX

import info.kwarc.mmt.api._
import objects.{Context, OMS, Term, VarDecl}
import modules.{DeclaredModule, DeclaredTheory, Module}
import symbols._
import checking._
import parser.{KeywordBasedParser, ParserExtension, ParserState, SourceRef}
import utils.URI
import notations._
import Subtyping._
import info.kwarc.mmt.lf.{Arrow, Typed}

object LFX {
  val ns = DPath(URI.http colon "gl.mathhub.info") / "MMT" / "LFX"
}

class SubTypeParserExt extends ParserExtension {

  def isApplicable(se: StructuralElement, keyword: String): Boolean = se match {
    case c:FinalConstant => c.tp.isEmpty && keyword == "<:"
    case _ => false
  }

  def apply(sp: KeywordBasedParser, s: ParserState, se: StructuralElement, keyword: String,con:Context) = se match {
    case c: Constant if keyword == "<:" =>
      val (obj, reg, pr) = sp.readParsedObject(con)(s)
      c.tpC.read = obj
      c.tpC.parsed = pr.map(t => subtypeOf(t)).toTerm
    case _ => s.errorCont(SourceError("SubTypeParserExt", SourceRef(s.ps.source, s.startPosition.toRegion),
      "not applicable to StructuralElement "+se.getClass.toString))
  }
}

class Plugin extends frontend.Plugin {
  val theory = Subtyping.SubTyped.path
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]) {
    val em = controller.extman
    // content enhancers
    //em.addExtension(new SubTypeParserExt)
    em.addExtension(new SubtypeFeature)
  }
}

object SubtypeDecl {
  val feature = "Subtype"
  def apply(top : DeclaredTheory, givenname : String, suptp : Term, notC: NotationContainer = NotationContainer()) =
    new DerivedDeclaration(top.toTerm,LocalName(givenname),feature, TermContainer(suptp), notC)
}

class SubtypeFeature extends StructuralFeature(SubtypeDecl.feature) {
  
  def getHeaderNotation = List(LabelArg(1,LabelInfo.none), Delim("<:"), SimpArg(2))

  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) : Elaboration = {
    val suptp = dd.getComponent(TypeComponent) getOrElse {
      throw GetError("")
    } match {
      case tc: TermContainer => tc.get.getOrElse {
        throw GetError("")
      }
      case _ =>
        throw GetError("")
    }
    val name = LocalName(dd.name.toString.drop(1))

    new Elaboration {
      val pred = Constant(parent.toTerm,LocalName(name.toString + "_pred"),Nil,Some(Arrow(suptp,OMS(Typed.ktype))),None,None)
      val tp = Constant(parent.toTerm,name,Nil,Some(OMS(Typed.ktype)),Some(predsubtp(suptp,pred.toTerm)),None)
      val decls = List(pred,tp)
      def domain: List[LocalName] = decls.map(_.name)
      def getO(name: LocalName): Option[Declaration] = decls.find(_.name == name)
    }

  }
  def check(d: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = {
    if (!d.name.toString.startsWith("_")) throw GetError("Name of Subtype Declaration must start with '_'!")
  }
}