package info.kwarc.mmt.coq

import info.kwarc.mmt.api.{DPath, GlobalName, LocalName}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.coq.coqxml.TranslationState
import info.kwarc.mmt.lf._

object Coq {
  val namespace = DPath(URI.http colon "coq.inria.fr")
  val foundation = (namespace / "foundation") ? "Coq"

  val tp = foundation ? "Type"
  val sort = foundation ? "sort"
  val prop = foundation ? "Prop"
  val set = foundation ? "Set"

  val decltype = foundation ? "DeclarationType"

  def makeSort(value : String) = value match {
    case "Type" => OMS(Coq.tp)
    case "Set" => OMS(Coq.set)
    case "Prop" => OMS(Coq.prop)
    case _ =>
      println(value)
      ???
  }

  val fail = foundation ? "FAIL"

  def makeImplicit : OMV = ???
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
  def apply(s : String) : Term = apply(Coq.makeSort(s))
  def apply(sort : Term) : Term = Apply(tm,sort)
}

object expr extends Coqsymbol("expr") {
  def apply(s : String,tp : Term): Term = apply(Coq.makeSort(s),tp)
  def apply(sort : Term,tp : Term) : Term = ApplySpine(tm,sort,tp)
}

object CoqPROD extends Coqsymbol("PROD") {
  def apply(sort : String, name : String, tp : Term, body : Term)(implicit implicits:TranslationState) : Term = {
    val u1 = implicits.newImplicit(Some(OMS(Coq.sort)))
    val u2 = Coq.makeSort(sort)
    val t = tp
    val s = Lambda(LocalName(name), tp, body)
    ApplySpine(tm,
      u1,
      u2,
      t,
      s)
  }
  def apply(_type : String,vars : List[(String,Term)],body : Term)(implicit implicits:TranslationState) : Term =
    vars.foldLeft(body)((tm,p) => apply(_type,p._1,p._2,tm))
}

object CoqApply extends Coqsymbol("APPLY") {
  def apply(sort : String,f : Term,arg : Term)(implicit implicits:TranslationState): Term = {
    val u1 = implicits.newImplicit(Some(OMS(Coq.sort)))
    val u2 = Coq.makeSort(sort)
    val t = implicits.newImplicit(Some(Coqtp(u1)))
    val s = implicits.newImplicit(Some(Arrow(expr(u1,t),Coqtp(u2))))
    // val x = implicits.pickFresh
    // val ftp = implicits.newImplicit(Some(Pi(x,expr(u1,t),expr(u2,Apply(s,OMV(x))))))
    ApplySpine(tm,
      u1,
      u2,
      t,
      s,
      f,
      arg
    )
  }
  def apply(sort : String,f : Term, args : List[Term])(implicit implicits:TranslationState) : Term = {
    args.foldLeft(f)((fi,arg) => apply(sort,fi,arg))
  }
}

object CoqLambda extends Coqsymbol("LAMBDA") {
  def apply(sort : String,name:LocalName,tp : Term,body : Term)(implicit implicits:TranslationState): Term = {
    val u1 = implicits.newImplicit(Some(OMS(Coq.sort)))
    val u2 = Coq.makeSort(sort)
    val t = tp
    val s = implicits.newImplicit(Some(Arrow(expr(u1,t),Coqtp(u2))))
    val ftp = Lambda(name,expr(u1,t),body)
    ApplySpine(tm,
      u1,
      u2,
      t,
      s,
      ftp
    )
  }
  def apply(sort : String,args : List[(LocalName,Term)],body : Term)(implicit implicits:TranslationState) : Term = {
    args.foldLeft(body)((fi,arg) => apply(sort,arg._1,arg._2,fi))
  }
}