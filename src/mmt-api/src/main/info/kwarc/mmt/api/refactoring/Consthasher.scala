package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api.frontend.{Controller, Logger, Report}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api._

import scala.collection.mutable

case class Consthash(iname:GlobalName, hash: List[Int], pars: List[Hasher.Targetable],
                     isProp: Boolean, optDef : Option[(Int,List[Hasher.Targetable])]) {
  val name = if (iname.name.steps.last == SimpleStep("defexp")) iname.module ? iname.name.steps.init else iname

  //def matches(l:List[(GlobalName,GlobalName)])(that:Consthash):Boolean = newPairs(l)(that).isEmpty
  override def toString = iname.toString + "[" + pars.map(_.toString).mkString(", ") + "]" +
    optDef.map(p => " = [" + p._2.map(_.toString).mkString(", ") + "]").getOrElse("")

  def <>(that : Consthash) = this.hash == that.hash && this.pars.length == that.pars.length
  def !<>(that :Consthash) = !(this <> that)

  private[refactoring] def toJson = JSONObject(
    ("name",JSONString(name.toString)),
    ("hash",JSONArray(hash.map(JSONInt(_)):_*)),
    ("pars",JSONArray(pars.map(p => JSONString(p.toString)):_*)),
    ("isProp",JSONBoolean(isProp)) // TODO Definition
  )
}

class Theoryhash(val path:MPath) {
  private var consts : List[Consthash] = Nil
  private var includes : List[Theoryhash] = Nil
  private var allincludes : List[Theoryhash] = Nil
  private var allconsts : List[Consthash] = Nil

  private[refactoring] def toJson = JSONObject(
    ("path",JSONString(path.toString)),
    ("includes",JSONArray(includes.map(i => JSONString(i.path.toString)):_*)),
    ("consts",JSONArray(consts.map(_.toJson):_*))
  )

  def getLocal = consts
  def getAll = allconsts
  def getincludes = includes
  def getAllIncludes = allincludes

  def addConstant(c : Consthash) = consts::=c
  def addInclude(th : Theoryhash) = includes::=th
  def init: Unit = {
    allincludes = (includes:::includes.flatMap(_.getAllIncludes)).distinct
    allconsts = consts ::: allincludes.flatMap(_.getLocal)
  }

  def <(that : Theoryhash) = that.getAllIncludes contains this

  def toStringIndent(ind:String) : String =
    ind + path.name.toString +
      includes.map(x => "\n" + x.toStringIndent(ind + "  ")).mkString("\n") +
      consts.map(p => "\n  " + ind + p.toString).mkString("")

  override def toString = "TheoryHash " + path.name.toString + "\n" + consts.mkString("\n ") + ")"

}

object Hasher {
  val FROM = 0
  val TO = 1
  val COMMON = 2

  trait Targetable // todo symbols, free variables, ...
  case class Symbol(gn : GlobalName) extends Targetable {
    override def toString: String = gn.module.name.toString + "?" + gn.name.toString
  }
  val complexpath = utils.mmt.mmtcd ? "unknown"
  class Complex(val tm : Term) extends Targetable {
    override def equals(obj: scala.Any): Boolean = obj match {
      case that : Complex => this.tm == that.tm
      case _ => false
    }
    def asTerm : Term = OMA(OMS(complexpath),List(tm))

    override def toString: String = tm.toStr(true)
  }

  object Bind {
    val path = utils.mmt.mmtcd ? "free"
    def apply(ln : LocalName) = OMBIND(OMS(path),VarDecl(ln),OMV(ln))
  }

  object Complex {
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`complexpath`),List(itm)) => Some(new Complex(itm))
      case _ => None
    }
    def apply(tm : Term) = OMA(OMS(complexpath),List(tm))
  }
  /*
  case class FreeVar(ln : LocalName) extends Targetable {
    val tm = OMBINDC(OMS(utils.mmt.mmtcd ? "TargetableVariable"),VarDecl)

    override def toString: String = "Variable("
  }
  */
}

trait Hasher {

  val controller : Controller

  val cfg : FinderConfig

  def from : List[Theoryhash]
  def to : List[Theoryhash]
  def common : List[MPath]

  def get(mp : MPath) : Option[Theoryhash]

  def add(th : Theory, as : Int) : Unit
}

class HashesNormal(val cfg : FinderConfig,val controller: Controller) extends Hasher {
  private var theories : List[(Theoryhash,Int)] = Nil
  private var commons : List[MPath] = Nil
  private var numbers : List[GlobalName] = cfg.fixing.map { a =>
    a.alignment.to.mmturi match {
      case gn : GlobalName => gn
      case _ => throw ImplementationError("Expected a GlobalName")
    }
  }

  private[refactoring] def toJson = JSONObject(
    ("theories",JSONArray(theories.map(p => JSONArray(JSONInt(p._2),p._1.toJson)):_*)),
    ("commons",JSONArray(commons.map(p => JSONString(p.toString)):_*)),
    ("numbers",JSONArray(numbers.map(gn => JSONString(gn.toString)):_*))
  )

  def from = theories.collect {
    case (th,Hasher.FROM) => th
  }.reverse
  def to = theories.collect {
    case (th,Hasher.TO) => th
  }.reverse
  def common = commons

  def get(p : MPath) : Option[Theoryhash] = theories.find(_._1.path == p).map(_._1)

  def add (th : Theory, as : Int) = as match {
    case Hasher.COMMON =>
      commons ::= th.path
      th.getConstants.foreach(c => numbers ::= c.path)
    case _ => theories.find(p => p._1.path == th.path && p._2 == as).getOrElse {
      val ret = get(th)
      theories ::= ((ret, as))
    }
  }

  private def get(th : Theory) : Theoryhash = {
    val h = new Theoryhash(th.path)
    if (!(commons contains th.path)) {
      th.getConstants.collect({
        case c : FinalConstant
          if !cfg.fixing.exists(a => a.alignment.from.mmturi == c.path || a.alignment.to.mmturi == c.path) => c
      }) foreach (c => h.addConstant(doConstant(c)))
      th.getIncludes.filterNot(commons.contains).foreach(t =>
        get(t) match {
          case Some(r) => h.addInclude(r)
          case _ =>
        }
      )
    }
    h.init
    h
  }

  private def doConstant(c:FinalConstant) : Consthash = {
    var isAxiom = false
    var pars : List[Hasher.Targetable] = Nil

    def traverse(t: Term)(implicit vars : List[LocalName]) : List[Int] = {
      // assumption: at most one alignment applicable
      val al = cfg.fixing.find(_.applicable(t)(AcrossLibraryTranslator.trivial(controller)))
      val tm = al.map(_.apply(t)(AcrossLibraryTranslator.trivial(controller))).getOrElse(t)
      tm match {
        case Hasher.Complex(itm) => List(1,1,{
          pars ::= itm
          pars.length-1
        })
        case OMV(name) =>
          List(0, 2 * vars.indexOf(name))
        case OMS(path) =>
          if (cfg.judg1.contains(path) || cfg.judg2.contains(path)) isAxiom = true
          val nopt = numbers.indexOf(path)
          val (i,j) = nopt match {
            case -1 => (1,{
              pars ::= Hasher.Symbol(path)
              pars.length-1
            })
            case n => (0,n)
          }
          List(1,i,j)
        case OMA(f, args) =>
          2 :: args.length :: traverse(f) ::: args.flatMap(traverse)
        case OMBINDC(f, con, bds) =>
          val (cont, newvars) = con.foldLeft((List(3, con.length), vars))((p, v) =>
            (p._1 ::: v.tp.map(traverse(_)(p._2)).getOrElse(List(-1)), v.name :: p._2))
          cont ::: List(bds.length) ::: bds.flatMap(traverse(_)(newvars))
        case OMLIT(value, rt) =>
          4 :: value.hashCode :: traverse(rt.synType)
        case UnknownOMLIT(s, tp) =>
          5 :: s.hashCode :: traverse(tp)
        case OML(name, tp, df, _,_) =>
          6 :: tp.map(traverse).getOrElse(List(-1)) ::: df.map(traverse).getOrElse(List(-1))
        case OMSemiFormal(_) | OMMOD(_) => List(1,1,{
          pars ::= new Hasher.Complex(t)
          pars.length-1
        })
        case _ =>
          println("Missing: " + tm.getClass)
          println(c.path)
          println(c.tp)
          ???
      }
    }
    val hash = c.tp.map(traverse(_)(Nil)).getOrElse(Nil)//.asInstanceOf[List[Any]]
    val tppars = pars
    val optDef = if(cfg.doDefs) {
      pars = Nil
      c.df.map(traverse(_)(Nil))
    } else None
    Consthash(c.path,hash,tppars,isAxiom,optDef.map(d => (d.hashCode(),pars)))
  }

}