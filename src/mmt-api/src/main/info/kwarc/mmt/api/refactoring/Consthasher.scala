package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api.frontend.{Logger, Report}
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.{GlobalName, LocalName, MPath}

import scala.collection.mutable

case class Consthash(name:GlobalName, hash: List[Int], pars: List[GlobalName],
                     isProp: Boolean, optDef : Option[(Int,List[GlobalName])]) {

  //def matches(l:List[(GlobalName,GlobalName)])(that:Consthash):Boolean = newPairs(l)(that).isEmpty
  override def toString = name.toString + "[" + pars.map(_.name.toString).mkString(", ") + "]" +
    optDef.map(p => " = [" + p._2.map(_.name.toString).mkString(", ") + "]").getOrElse("")

  def <>(that : Consthash) = this.hash == that.hash && this.pars.length == that.pars.length
  def !<>(that :Consthash) = !(this <> that)

  private[refactoring] def toJson = JSONObject(
    ("name",JSONString(name.toString)),
    ("hash",JSONArray(hash.map(JSONInt):_*)),
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

  private def toStringIndent(ind:String) : String =
    ind + path.name.toString +
      includes.map(x => "\n" + x.toStringIndent(ind + "  ")).mkString("\n") +
      consts.map(p => "\n  " + ind + p.toString).mkString("")

  override def toString = toStringIndent("")

}

object Hasher {
  val FROM = 0
  val TO = 1
  val COMMON = 2
}

trait Hasher {

  val cfg : FinderConfig

  def from : List[Theoryhash]
  def to : List[Theoryhash]
  def common : List[MPath]

  def get(mp : MPath) : Option[Theoryhash]

  def add(th : DeclaredTheory, as : Int) : Unit
}

class HashesNormal(val cfg : FinderConfig) extends Hasher {
  private var theories : List[(Theoryhash,Int)] = Nil
  private var commons : List[MPath] = Nil
  private var numbers : List[GlobalName] = cfg.fixing.map { a =>
    a.alignment.to.mmturi match {
      case gn : GlobalName => gn
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

  def add (th : DeclaredTheory, as : Int) = as match {
    case Hasher.COMMON =>
      commons ::= th.path
      th.getConstants.foreach(c => numbers ::= c.path)
    case _ => theories.find(_._1.path == th.path).getOrElse{
      val ret = get(th)
      theories ::= ((ret,as))
    }
  }

  private def get(th : DeclaredTheory) : Theoryhash = {
    val h = new Theoryhash(th.path)
    if (!(commons contains th.path)) {
      th.getConstants.collect({
        case c : FinalConstant
          if !cfg.fixing.exists(a => a.alignment.from.mmturi == c.path || a.alignment.to.mmturi == c.path) => c
      }) foreach (c => h.addConstant(doConstant(c)))
      th.getIncludes.filterNot(commons.contains).foreach(t => h.addInclude(get(t).getOrElse( ??? )))
    }
    h.init
    h
  }

  private def doConstant(c:FinalConstant) : Consthash = {
    var isAxiom = false
    var pars : List[GlobalName] = Nil

    def traverse(t: Term)(implicit vars : List[LocalName]) : List[Int] = {
      // assumption: at most one alignment applicable
      val al = cfg.fixing.find(_.applicable(t))
      val tm = al.map(_.apply(t)).getOrElse(t)
      tm match {
        case OMV(name) =>
          List(0, 2 * vars.indexOf(name))
        case OMS(path) =>
          if (cfg.judg1.contains(path) || cfg.judg2.contains(path)) isAxiom = true
          val nopt = numbers.indexOf(path)
          val (i,j) = nopt match {
            case -1 => (1,if (pars.contains(path)) pars.length - (pars.indexOf(path)+1) else {
              pars ::= path
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
        case _ =>
          println("Missing: " + tm.getClass)
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