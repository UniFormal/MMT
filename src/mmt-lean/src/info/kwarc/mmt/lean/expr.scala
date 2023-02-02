package info.kwarc.mmt.lean

import java.util.function.Predicate

import Level._

import scala.annotation.tailrec
import scala.collection.mutable

sealed abstract class BinderInfo extends Product {
  def dump = s"BinderInfo.$productPrefix"
}
object BinderInfo {
  case object Default extends BinderInfo
  case object Implicit extends BinderInfo
  case object StrictImplicit extends BinderInfo
  case object InstImplicit extends BinderInfo
}

case class Binding(prettyName: Name, ty: Expr, info: BinderInfo) {
  def toShort(implicit lcs: mutable.Map[LocalConst.Name, String]) =
    prettyName.toString + ": " + ty.toShort
  def dump(implicit lcs: mutable.Map[LocalConst.Name, String]) =
    s"Binding(${prettyName.dump}, ${ty.dump}, ${info.dump})"

  override val hashCode: Int = prettyName.hashCode + 37 * (ty.hashCode + 37 * info.hashCode)

  def equalsCore(that: Binding)(implicit cache: ExprEqCache): Boolean =
    this.info == that.info &&
      this.ty.equalsCore(that.ty) &&
      this.prettyName == that.prettyName
}

private class ExprCache extends java.util.IdentityHashMap[Expr, Expr] {
  @inline final def getOrElseUpdate(k: Expr)(v: Expr => Expr): Expr = {
    val cached = get(k)
    if (cached != null) {
      cached
    } else {
      val computed = v(k)
      put(k, computed)
      computed
    }
  }
}
private class ExprOffCache extends mutable.ArrayBuffer[ExprCache] {
  @inline final def getOrElseUpdate(k: Expr, off: Int)(v: Expr => Expr): Expr = {
    while (off >= size) this += new ExprCache
    this(off).getOrElseUpdate(k)(v)
  }
}

private class ExprEqCache extends java.util.IdentityHashMap[Expr, UFNode] {
  def find(e: Expr): UFNode = {
    var n = get(e)
    if (n == null) {
      n = new UFNode
      put(e, n)
    } else {
      n = n.find()
    }
    n
  }

  @inline final def checkAndThenUnion(a: Expr, b: Expr)(v: (Expr, Expr) => Boolean): Boolean = {
    val a_ = find(a)
    val b_ = find(b)
    if (a_ eq b_) return true
    if (v(a, b)) {
      a_.union(b_)
      true
    } else {
      false
    }
  }
}

private object Breadcrumb

sealed abstract class Expr(val varBound: Int, val hasLocals: Boolean, override val hashCode: Int) extends Product {
  final def hasVar(i: Int): Boolean =
    this match {
      case _ if varBound <= i => false
      case Var(idx) => idx == i
      case App(a, b) => a.hasVar(i) || b.hasVar(i)
      case Lam(dom, body) => dom.ty.hasVar(i) || body.hasVar(i + 1)
      case Pi(dom, body) => dom.ty.hasVar(i) || body.hasVar(i + 1)
      case Let(dom, value, body) => dom.ty.hasVar(i) || value.hasVar(i) || body.hasVar(i + 1)
    }

  def hasVars: Boolean = varBound > 0

  override def equals(that: Any): Boolean =
    that match {
      case that: Expr => equals(that)
      case _ => false
    }
  def equals(that: Expr): Boolean = equalsCore(that)(new ExprEqCache)
  def equalsCore(that: Expr)(implicit cache: ExprEqCache): Boolean =
    (this eq that) || this.hashCode == that.hashCode &&
      cache.checkAndThenUnion(this, that) {
        case (Var(i1), Var(i2)) => i1 == i2
        case (Sort(l1), Sort(l2)) => l1 == l2
        case (Const(n1, l1), Const(n2, l2)) => n1 == n2 && l1 == l2
        case (LocalConst(_, n1), LocalConst(_, n2)) => n1 == n2
        case (App(a1, b1), App(a2, b2)) => a1.equalsCore(a2) && b1.equalsCore(b2)
        case (Lam(d1, b1), Lam(d2, b2)) => d1.equalsCore(d2) && b1.equalsCore(b2)
        case (Pi(d1, b1), Pi(d2, b2)) => d1.equalsCore(d2) && b1.equalsCore(b2)
        case (Let(d1, v1, b1), Let(d2, v2, b2)) => d1.equalsCore(d2) && v1.equalsCore(v2) && b1.equalsCore(b2)
        case _ => false
      }

  def abstr(lc: LocalConst): Expr = abstr(0, Vector(lc))
  def abstr(off: Int, lcs: Vector[LocalConst]): Expr =
    abstrCore(off, lcs)(new ExprOffCache)
  private def abstrCore(off: Int, lcs: Vector[LocalConst])(implicit cache: ExprOffCache): Expr =
    cache.getOrElseUpdate(this, off) {
      case _ if !hasLocals => this
      case LocalConst(_, name) =>
        lcs.indexWhere(_.name == name) match {
          case -1 => this
          case i => Var(i + off)
        }
      case App(a, b) =>
        App(a.abstrCore(off, lcs), b.abstrCore(off, lcs))
      case Lam(domain, body) =>
        Lam(domain.copy(ty = domain.ty.abstrCore(off, lcs)), body.abstrCore(off + 1, lcs))
      case Pi(domain, body) =>
        Pi(domain.copy(ty = domain.ty.abstrCore(off, lcs)), body.abstrCore(off + 1, lcs))
      case Let(domain, value, body) =>
        Let(domain.copy(ty = domain.ty.abstrCore(off, lcs)), value.abstrCore(off, lcs), body.abstrCore(off + 1, lcs))
    }

  def instantiate(e: Expr): Expr = instantiate(0, Vector(e))
  def instantiate(off: Int, es: Vector[Expr]): Expr =
    if (varBound <= off) this else
      instantiateCore(off, es)(new ExprOffCache)
  private def instantiateCore(off: Int, es: Vector[Expr])(implicit cache: ExprOffCache): Expr =
    cache.getOrElseUpdate(this, off) {
      case _ if varBound <= off => this
      case Var(idx) => if (off <= idx && idx < off + es.size) es(idx - off) else this
      case App(a, b) => App(a.instantiateCore(off, es), b.instantiateCore(off, es))
      case Lam(domain, body) => Lam(domain.copy(ty = domain.ty.instantiateCore(off, es)), body.instantiateCore(off + 1, es))
      case Pi(domain, body) => Pi(domain.copy(ty = domain.ty.instantiateCore(off, es)), body.instantiateCore(off + 1, es))
      case Let(domain, value, body) => Let(
        domain.copy(ty = domain.ty.instantiateCore(off, es)),
        value.instantiateCore(off, es), body.instantiateCore(off + 1, es))
    }

  def instantiate(subst: Map[Param, Level]): Expr =
    if (subst.forall(x => x._1 == x._2)) this else instantiateCore(subst)(new ExprCache)
  private def instantiateCore(subst: Map[Param, Level])(implicit cache: ExprCache): Expr =
    cache.getOrElseUpdate(this) {
      case v: Var => v
      case Sort(level) => Sort(level.instantiate(subst))
      case Const(name, levels) => Const(name, levels.map(_.instantiate(subst)))
      case LocalConst(of, name) => LocalConst(of.copy(ty = of.ty.instantiateCore(subst)), name)
      case App(a, b) => App(a.instantiateCore(subst), b.instantiateCore(subst))
      case Lam(domain, body) => Lam(domain.copy(ty = domain.ty.instantiateCore(subst)), body.instantiateCore(subst))
      case Pi(domain, body) => Pi(domain.copy(ty = domain.ty.instantiateCore(subst)), body.instantiateCore(subst))
      case Let(domain, value, body) => Let(
        domain.copy(ty = domain.ty.instantiateCore(subst)),
        value.instantiateCore(subst), body.instantiateCore(subst))
    }

  final def foreach_(f: Predicate[Expr]): Unit =
    if (f.test(this)) this match {
      case App(a, b) =>
        a.foreach_(f)
        b.foreach_(f)
      case Lam(domain, body) =>
        domain.ty.foreach_(f)
        body.foreach_(f)
      case Pi(domain, body) =>
        domain.ty.foreach_(f)
        body.foreach_(f)
      case Let(domain, value, body) =>
        domain.ty.foreach_(f)
        value.foreach_(f)
        body.foreach_(f)
      case _: Var | _: Const | _: Sort | _: LocalConst =>
    }

  @inline final def foreachNoDups(f: Expr => Unit): Unit = {
    val seen = new java.util.IdentityHashMap[Expr, Breadcrumb.type]()
    foreach_ { x =>
      if (seen.put(x, Breadcrumb) == null) {
        f(x)
        true
      } else {
        false
      }
    }
  }

  @inline private def buildSet[T](f: mutable.Set[T] => Unit): Set[T] = {
    val set = mutable.Set[T]()
    f(set)
    set.toSet
  }

  def univParams: Set[Param] =
    buildSet { ps =>
      foreachNoDups {
        case Sort(level) => ps ++= level.univParams
        case Const(_, levels) => ps ++= levels.view.flatMap(_.univParams)
        case _ =>
      }
    }

  def constants: Set[Name] =
    buildSet { cs =>
      foreachNoDups {
        case Const(name, _) => cs += name
        case _ =>
      }
    }

  def -->:(that: Expr): Expr =
    Pi(Binding(Name.Anon, that, BinderInfo.Default), this)

  override def toString: String = pretty(this)

  def toShort(implicit lcs: mutable.Map[LocalConst.Name, String] = null): String =
    this match {
      case _ if lcs eq null =>
        val lcs_ = mutable.Map[LocalConst.Name, String]()
        toShort(lcs_)
      case Var(i) => i.toString
      case Sort(level) => level.toShort
      case Const(name, levels) => name.toString + (if (levels.isEmpty) "" else levels.map(_.toShort).mkString("(", " ", ")"))
      case App(a, b) => "(" + a.toShort + " " + b.toShort + ")"
      case Lam(dom, body) => "[" + dom.toShort + "] " + body.toShort
      case Pi(dom, body) => "{" + dom.toShort + "} " + body.toShort
      case LocalConst(of, name) =>
        val of1 = of.prettyName.toString.replace('.', '_').filter { _.isLetterOrDigit }
        val of2 = if (of1.isEmpty || !of1.head.isLetter) s"n$of1" else of1
        val n = lcs.getOrElseUpdate(name, LazyList.from(0).map(i => s"$of2$i").diff(lcs.values.toSeq).head)
        n
      case Let(dom, value, body) => s"let ${dom.toShort} = ${value.toShort} in ${body.toShort}"
    }

  def dump(implicit lcs: mutable.Map[LocalConst.Name, String] = null): String =
    this match {
      case _ if lcs eq null =>
        val lcs_ = mutable.Map[LocalConst.Name, String]()
        val d = dump(lcs_)
        if (lcs_.isEmpty) d else {
          val decls = lcs.values.map { n => s"val $n = new LocalConst.Name()\n" }.mkString
          s"{$decls$d}"
        }
      case Var(i) => s"Var($i)"
      case Sort(level) => s"Sort(${level.dump})"
      case Const(name, levels) => s"Const(${name.dump}, Vector(${levels.map(_.dump).mkString(", ")}))"
      case App(a, b) => s"App(${a.dump}, ${b.dump})"
      case Lam(dom, body) => s"Lam(${dom.dump}, ${body.dump})"
      case Pi(dom, body) => s"Pi(${dom.dump}, ${body.dump})"
      case LocalConst(of, name) =>
        val of1 = of.prettyName.toString.replace('.', '_').filter { _.isLetterOrDigit }
        val of2 = if (of1.isEmpty || !of1.head.isLetter) s"n$of1" else of1
        val n = lcs.getOrElseUpdate(name, LazyList.from(0).map(i => s"$of2$i").diff(lcs.values.toSeq).head)
        s"LocalConst(${of.dump}, $n)"
      case Let(dom, value, body) => s"Let(${dom.dump}, ${value.dump}, ${body.dump})"
    }
}
case class Var(idx: Int) extends Expr(varBound = idx + 1, hasLocals = false, hashCode = idx)
case class Sort(level: Level) extends Expr(varBound = 0, hasLocals = false, hashCode = level.hashCode)

case class Const(name: Name, levels: Vector[Level])
  extends Expr(varBound = 0, hasLocals = false, hashCode = 37 * name.hashCode)
case class LocalConst(of: Binding, name: LocalConst.Name = new LocalConst.Name)
  extends Expr(varBound = 0, hasLocals = true, hashCode = 4 + name.hashCode)
case class App(a: Expr, b: Expr)
  extends Expr(
    varBound = math.max(a.varBound, b.varBound),
    hasLocals = a.hasLocals || b.hasLocals,
    hashCode = a.hashCode + 37 * b.hashCode)
case class Lam(domain: Binding, body: Expr)
  extends Expr(
    varBound = math.max(domain.ty.varBound, body.varBound - 1),
    hasLocals = domain.ty.hasLocals || body.hasLocals,
    hashCode = 1 + 37 * domain.hashCode + body.hashCode)
case class Pi(domain: Binding, body: Expr)
  extends Expr(
    varBound = math.max(domain.ty.varBound, body.varBound - 1),
    hasLocals = domain.ty.hasLocals || body.hasLocals,
    hashCode = 2 + 37 * domain.hashCode + body.hashCode)
case class Let(domain: Binding, value: Expr, body: Expr)
  extends Expr(
    varBound = math.max(math.max(domain.ty.varBound, value.varBound), body.varBound - 1),
    hasLocals = domain.ty.hasLocals || value.hasLocals || body.hasLocals,
    hashCode = 3 + 37 * (domain.hashCode + 37 * value.hashCode) + body.hashCode)

object Sort {
  val Prop = Sort(Level.Zero)
}

object LocalConst {
  final class Name {
    override def toString: String = Integer.toHexString(hashCode()).take(4)
  }
}

trait Binder[T] {
  def apply(domain: Binding, body: Expr): T
  def apply(domain: LocalConst, body: Expr): T =
    apply(domain.of, body.abstr(domain))

  trait GenericUnapply {
    def unapply(e: Expr): Option[(Binding, Expr)]
  }
  val generic: GenericUnapply
}

trait Binders[T <: Expr] {
  protected val Single: Binder[T]

  def apply(domains: Iterable[LocalConst])(body: Expr): Expr =
    domains.foldRight(body)(Single.apply)

  def apply(domains: LocalConst*)(body: Expr): Expr =
    apply(domains)(body)

  def unapply(e: Expr): Some[(List[LocalConst], Expr)] =
    e match {
      case Single.generic(dom, expr) =>
        val lc = LocalConst(dom)
        unapply(expr.instantiate(lc)) match {
          case Some((lcs, head)) =>
            Some((lc :: lcs, head))
        }
      case _ => Some((Nil, e))
    }
}

object Let {
  def apply(x: LocalConst, v: Expr, b: Expr): Let =
    Let(x.of, v, b.abstr(x))
}

object Lam extends Binder[Lam] {
  val generic: GenericUnapply = {
    case e: Lam => Lam.unapply(e)
    case _ => None
  }
}
object Lams extends Binders[Lam] {
  protected val Single = Lam
}

object Pi extends Binder[Pi] {
  val generic: GenericUnapply = {
    case e: Pi => Pi.unapply(e)
    case _ => None
  }
}
object Pis extends Binders[Pi] {
  protected val Single = Pi
}

object Apps {
  @tailrec
  private def decompose(e: Expr, as: List[Expr] = Nil): (Expr, List[Expr]) =
    e match {
      case App(f, a) => decompose(f, a :: as)
      case _ => (e, as)
    }

  def unapply(e: Expr): Some[(Expr, List[Expr])] =
    Some(decompose(e))

  def apply(fn: Expr, as: Iterable[Expr]): Expr =
    as.foldLeft(fn)(App)

  def apply(fn: Expr, as: Expr*): Expr =
    apply(fn, as)
}
