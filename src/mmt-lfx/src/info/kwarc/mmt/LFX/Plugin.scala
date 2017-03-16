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
import info.kwarc.mmt.LFX.Coproducts.Addfunc
import info.kwarc.mmt.LFX.Records.Rectype
import info.kwarc.mmt.lf.{Apply, Arrow, Typed}

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
/*
class WInductive extends NamedInductiveTypes {
  val tpsym = Typed.ktype
  val arrow = info.kwarc.mmt.lf.Arrow.path

  override def doConstants(implicit dd: InductiveType) = dd.typenames foreach (tpname => {
    val consts = dd.constructornames.filter(_._3.name == tpname.name)
    val tps = consts map {
      case (_,Nil,_) => (FiniteTypes.Unit.term,FiniteTypes.EmptyType.term)
      case (_,tpls,_) => //Coproducts.Coprod(tpls:_*)
        var tpinit : List[Term] = Nil
        var arity = 0
        tpls foreach {
          case OMS(pth) if pth.name == tpname.name =>
            arity += 1
          case t => tpinit ::=t
        }
        tpinit = tpinit.reverse
        (Coproducts.Coprod(tpinit:_*),FiniteTypes.Finite(arity))
    }
    val wdom = Coproducts.Coprod(tps.view.map(_._1):_*)
    val fun = Addfunc(tps.map{
      case (fr,to) => Arrow(fr,to)
    }:_*)
    val df = WTypes.WType(LocalName("x"),wdom,Apply(fun,OMV("x")))
    dd.types ::= Constant(OMMOD(dd.dd.parent),tpname.name,Nil,Some(OMS(Typed.ktype)),Some(df),None)
  })

}
*/
class RecordFromTheory extends StructuralFeature("FromTheory") {

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
      case o =>
        println(o)
        ???
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

  def getDomain(dd: DerivedDeclaration): Term = dd.tpC.get.get.asInstanceOf[OML].tp.get

  def elaborate(parent: DeclaredModule, dd:  DerivedDeclaration) : Elaboration = {
    val dom = termtoTheory(getDomain(dd),dd.parent)
    val tp = Rectype(fromTheory(dom).distinct:_*)
    val tpname = dd.tpC.get match {
      case Some(OML(name,_,_,_,_)) => name
      case _ => ???
    }
    new Elaboration {
      def domain: List[LocalName] = List(tpname)
      def getO(name: LocalName): Option[Declaration] = {

        if (name == tpname) Some(Constant(dd.home,name,Nil,Some(OMID(Typed.ktype)),Some(tp),None))
        else None
      }
    }
  }

  def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = {

  }

  def getHeaderNotation = List(LabelArg(1,LabelInfo.none),Delim(":"),SimpArg(2))//List(LabelArg(2, LabelInfo.none), Delim("("), Var(1, true, Some(Delim(","))), Delim(")"))

  override def getInnerContext(dd: DerivedDeclaration) = Context.empty// Type.getParameters(dd)

  override def processHeader(header: Term) = header match {
    case OMA(OMMOD(`mpath`), List(OML(name,_,_,_,_),t @ OMMOD(path))) => (LocalName(path),OML(name,Some(t),None,None,None))// (name, Type(cont))
  }
  override def makeHeader(dd: DerivedDeclaration) = dd.tpC.get match {
    case Some(OML(name,Some(OMMOD(path)),_,_,_)) => OMA(OMMOD(mpath), List(OML(name,None,None),OMMOD(path)))
  }
}

object FromTheoryRule extends StructuralFeatureRule("FromTheory")