package info.kwarc.mmt.LFX

import info.kwarc.mmt.api._
import objects._
import modules.{DeclaredModule, DeclaredTheory, Module}
import symbols._
import checking._
import parser.{KeywordBasedParser, ParserExtension, ParserState, SourceRef}
import utils.URI
import notations._
import Subtyping._
import info.kwarc.mmt.LFX.Records.Rectype
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
    em addExtension new RecordFromTheory
    // content enhancers
    //em.addExtension(new SubTypeParserExt)
    // em.addExtension(new SubtypeFeature)
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

class WInductive extends NamedInductiveTypes {
  val tpsym = Typed.ktype
  val arrow = info.kwarc.mmt.lf.Arrow.path

  override def doType(tpname: LocalName)(implicit dd: InductiveType): FinalConstant = {
    val consts = dd.constructornames.filter(_._3.name == tpname)
    val tps = consts map {
      case (_,Nil,_) => FiniteTypes.Unit.term
      case (_,tpls,_) => Coproducts.Coprod(tpls:_*)
    }
    val tp = WTypes.WType
    ???
  }

  override def doConstructor(tpname: LocalName)(implicit dd: InductiveType): FinalConstant = ???
}

class RecordFromTheory extends StructuralFeature("FromTheory") with IncludeLike {

  private def substitute(tm : Term,cont : List[OML]) = {
    val trav = new StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
        case OMS(mp) =>
          val opt = cont.find(_.name == mp.name)
          if (opt.isDefined) opt.get else Traverser(this,t)
        case _ => Traverser(this,t)
      }
    }
    trav(tm,Context.empty)
  }

  private def fromTheory(th : DeclaredTheory,start : List[OML] = Nil) : List[OML] = {
    var cont : List[OML] = start
    th.getDeclarations foreach {
      case PlainInclude(from,path) if path == th.path =>
        cont = fromTheory(termtoTheory(OMMOD(from),from),cont).reverse ::: cont
      case c : Constant if c.df.isEmpty =>
        cont ::= OML(c.name,c.tp.map(substitute(_,cont)),None,c.not,None)
      case _ => ???
    }
    cont.reverse
  }

  private def termtoTheory(tm : Term, parent : MPath) : DeclaredTheory = tm match {
    case OMMOD(mp) => controller.get(mp) match {
      case t : DeclaredTheory => t
      case _ => ???
    }
    case t => controller.simplifier.materialize(Context(parent),t,false,None) match {
      case t : DeclaredTheory => t
      case _ => ???
    }
  }

  def elaborate(parent: DeclaredModule, dd:  DerivedDeclaration) : Elaboration = {
    val dom = termtoTheory(getDomain(dd),dd.parent)
    val tp = Rectype(fromTheory(dom):_*)
    new Elaboration {
      def domain: List[LocalName] = List(dom.name)
      def getO(name: LocalName): Option[Declaration] = {

        if (name == dom.name) Some(Constant(dd.home,name,Nil,Some(OMID(Typed.ktype)),Some(tp),None))
        else None
      }
    }
  }

  def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = {

  }
}

object FromTheoryRule extends StructuralFeatureRule("FromTheory")