package info.kwarc.mmt.LFX

import info.kwarc.mmt.api.objects.{Context, OMS, Term}
import Subtyping._
import info.kwarc.mmt.api.modules.{DeclaredModule, DeclaredTheory, Module}
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.checking.CheckingEnvironment
import info.kwarc.mmt.api.parser.{KeywordBasedParser, ParserExtension, ParserState, SourceRef}
import info.kwarc.mmt.api.utils.URI
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
    case c:FinalConstant if keyword == "<:" =>
      val (obj, reg, tm) = sp.readParsedObject(con)(s)
      c.tpC.read = obj
      c.tpC.parsed = subtypeOf(tm)
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

object PredSubFeature extends StructuralFeatureRule("Subtype",List(TypeComponent),hasname = true)

case class SubtypeDecl(top : DeclaredTheory, givenname : String, suptp : Term) extends
  DerivedDeclaration(top.toTerm,LocalName(givenname),"Subtype",
  List(DeclarationComponent(TypeComponent,TermContainer(suptp))))

class SubtypeFeature extends StructuralFeature("Subtype") {

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
  def modules(d: DerivedDeclaration): List[Module] = Nil
  def check(d: DerivedDeclaration)(implicit env: CheckingEnvironment): Unit = {
    if (!d.name.toString.startsWith("_")) throw GetError("Name of Subtype Declaration must start with '_'!")
  }
}