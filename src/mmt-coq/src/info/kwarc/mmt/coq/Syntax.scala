package info.kwarc.mmt.coq

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.{DerivedDeclaration, PlainInclude}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.coq.coqxml.TranslationState
import info.kwarc.mmt.lf._

import scala.util.Try

object Coq {
  val namespace = DPath(URI.http colon "coq.inria.fr")
  val foundation = (namespace / "foundation") ? "CoqSyntax"

  val tp = foundation ? "tp"
  val sort = foundation ? "sort"
  val prop = foundation ? "Prop"
  val set = foundation ? "Set"
  val utype = foundation ? "Type"
  val fix = foundation ? "fix"
  val cofix = foundation ? "cofix"
  val ccase = foundation ? "match"
  val proj = foundation ? "proj"

  val decltype = foundation ? "DeclarationType"

  def makeSort(value : String) = value match {
    case "Type" => Apply(OMS(Coq.utype),OMS(foundation ? "BaseType"))
    case "Set" => OMS(Coq.set)
    case "Prop" => OMS(Coq.prop)
    case _ =>
      // println(value) // TODO
      Apply(OMS(Coq.utype),OML(LocalName(value),Some(OMS(foundation ? "univ")),None))
  }

  val fail = foundation ? "FAIL"

  def makeImplicit : OMV = ???

  private def coqtoomdoc(uri : URI)(implicit controller : Controller): ContentPath = {
    var paths = uri.path
    def error : Nothing = ???
    var currentD : DPath = Path.parseD("cic:/",NamespaceMap.empty)
    var currentM : MPath = null
    while (paths.nonEmpty) {
      controller.getO(currentD ? paths.head) match {
        case Some(_) =>
          currentM = currentD ? paths.head
          paths = paths.tail
          while (paths.nonEmpty) {
            controller.getO(currentM.parent ? (currentM.name / paths.head)) match {
              case Some(_) =>
                currentM = currentM.parent ? (currentM.name / paths.head)
                paths = paths.tail
              case None =>
                return currentM ? paths.mkString("/").split('.').head
            }
          }
          return currentM
        case None =>
          currentD = currentD / paths.head
          paths = paths.tail
      }
    }
    error
  }
  def toMPath(uri : URI)(implicit state : TranslationState) : MPath = Try(coqtoomdoc(uri)(state.controller)).toOption match {
    case Some(mp:MPath) =>
      ensure(mp)
      mp
    case _ =>
      Coq.foundation // TODO
  }
  private def ensure(p : ContentPath)(implicit state : TranslationState): Unit = {
    val mp = p.module
    var current = state.controller.get(state.current)
    while (!current.isInstanceOf[Theory]) {
      current = state.controller.get(current.parent)
    } // .parent ? state.current.name.head
    val parent = current.asInstanceOf[Theory].path
    state.controller.library.getImplicit(mp,parent) match {
      case Some(_) =>
      case None => try {
        state.controller.add(PlainInclude(mp,parent),AtBegin)
      }
        catch {
          case e:ExtensionError =>
          case e =>
            println(e.getClass)
            ???
        }
    }
  }
  def toGlobalName(uri : URI)(implicit state : TranslationState) : GlobalName = Try(coqtoomdoc(uri)(state.controller)).toOption match {
    case Some(gn:GlobalName) =>
      ensure(gn)
      gn
    case _ =>
      Coq.fail // TODO
  }
}

object Let {
  val baseURI = DPath(URI.http colon "gl.mathhub.info") / "MMT" / "LFX" / "Datatypes"
  val th = baseURI ? "LetSymbol"

  val path = th ? "Let"

  def unapply(arg: Term): Option[(LocalName,Term,Term)] = arg match {
    case OMBINDC(OMS(`path`), Context(vd), df :: body :: Nil) =>
      Some((vd.name,df,body))
    case _ => None
  }

  def apply(x:LocalName,df:Term,bd:Term) = OMBINDC(OMS(path),Context(VarDecl(x)),List(df,bd))
}

object LFXSub {
  val baseURI = Let.baseURI
  val th = baseURI ? "SubSymbol"

  val path = th ? "substitute"

  def unapply(tm : Term) : Option[(Term,GlobalName,Term)] = tm match {
    case OMA(OMS(`path`),body :: OMS(q) :: t :: Nil) =>
      Some((body,q,t))
    case _ => None
  }
  def apply(body : Term, vartm : Term, t : Term) =
    OMA(OMS(path),List(body,vartm,t))
}

abstract class Coqsymbol(name : String) {
  val path = Coq.foundation ? name
  val tm = OMS(path)
}

object Coqtp extends Coqsymbol("tp") {
  def apply(t : Term) = Apply(this.tm,t)
}

object expr extends Coqsymbol("expr")

object CoqPROD extends Coqsymbol("PROD") {
  def apply(sort : String, name : String, tp : Term, body : Term)(implicit implicits:TranslationState) : Term = {
    val s = Lambda(LocalName(name), expr.tm, body)
    ApplySpine(tm,Coqtp(tp),s)
  }
  def apply(_type : String,vars : List[(String,Term)],body : Term)(implicit implicits:TranslationState) : Term =
    vars.foldLeft(body)((tm,p) => apply(_type,p._1,p._2,tm))
}

object CoqApply extends Coqsymbol("APPLY") {
  def apply(sort : String,f : Term,arg : Term)(implicit implicits:TranslationState): Term = {
    // val u1 = implicits.newImplicit(Some(OMS(Coq.sort)))
    //val u2 = Coq.makeSort(sort)
    // val t = implicits.newImplicit(Some(Coqtp(u1)))
    // val s = implicits.newImplicit(Some(Arrow(expr(u1,t),Coqtp(u2))))
    // val x = implicits.pickFresh
    // val ftp = implicits.newImplicit(Some(Pi(x,expr(u1,t),expr(u2,Apply(s,OMV(x))))))
    ApplySpine(tm,f,arg)
  }
  def apply(sort : String,f : Term, args : List[Term])(implicit implicits:TranslationState) : Term = {
    args.foldLeft(f)((fi,arg) => apply(sort,fi,arg))
  }
}

object CoqLambda extends Coqsymbol("LAMBDA") {
  def apply(sort : String,name:LocalName,tp : Term,body : Term)(implicit implicits:TranslationState): Term = {
    // val u1 = implicits.newImplicit(Some(OMS(Coq.sort)))
    // val u2 = Coq.makeSort(sort)
    // val t = tp
    // val s = implicits.newImplicit(Some(Arrow(expr(u1,t),Coqtp(u2))))
    val ftp = Lambda(name,Coqtp(tp),body)
    ApplySpine(tm,tp,ftp)
  }
  def apply(sort : String,args : List[(LocalName,Term)],body : Term)(implicit implicits:TranslationState) : Term = {
    args.foldLeft(body)((fi,arg) => apply(sort,arg._1,arg._2,fi))
  }
}