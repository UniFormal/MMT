package info.kwarc.mmt.lean

import scala.concurrent.{ ExecutionContext, Future }
import scala.language.implicitConversions
import scala.util.Try

final case class Declaration(name: Name, univParams: Vector[Level.Param], ty: Expr,
    height: Int = 0, builtin: Boolean = false) {
  def check(env: PreEnvironment): Unit = check(env, new TypeChecker(env))
  def check(env: PreEnvironment, tc: TypeChecker): Unit = {
    require(!env.declarations.contains(name))
    require(ty.univParams.subsetOf(univParams.toSet))
    require(!ty.hasVars)
    require(!ty.hasLocals)
    tc.inferUniverseOfType(ty)
  }
}

trait CompiledModification {
  def check(): Unit
  def decls: Seq[Declaration]
  def rules: Seq[ReductionRule]
}

trait Modification {
  def name: Name
  def compile(env: PreEnvironment): CompiledModification
}
object Modification {
  implicit def ofAxiom(axiom: Declaration): Modification = AxiomMod(axiom.name, axiom.univParams, axiom.ty)
}
final case class AxiomMod(name: Name, univParams: Vector[Level.Param], ty: Expr) extends Modification {
  def compile(env: PreEnvironment): CompiledModification = new CompiledModification {
    val decl = Declaration(name, univParams, ty)
    def check(): Unit = decl.check(env)
    def decls: Seq[Declaration] = Seq(decl)
    def rules: Seq[ReductionRule] = Seq()
  }
}
final case class DefMod(name: Name, univParams: Vector[Level.Param], ty: Expr, value: Expr) extends Modification {
  def compile(env: PreEnvironment): CompiledModification = new CompiledModification {
    val height: Int =
      value.constants.view.
        flatMap(env.get).
        map(_.height).
        fold(0)(math.max) + 1

    val decl = Declaration(name, univParams, ty, height = height)
    val rule = ReductionRule(Vector[Binding](), Const(name, univParams), value, List())

    def check(): Unit = {
      val tc = new TypeChecker(env)
      decl.check(env, tc)
      require(!value.hasVars)
      require(!value.hasLocals)
      tc.checkType(value, ty)
    }
    def decls: Seq[Declaration] = Seq(decl)
    def rules: Seq[ReductionRule] = Seq(rule)
  }
}

case class EnvironmentUpdateError(mod: Modification, msg: String) {
  override def toString = s"${mod.name}: $msg"
}

sealed class PreEnvironment protected (
    val declarations: Map[Name, Declaration],
    val reductions: ReductionMap,
    val proofObligations: List[Future[Option[EnvironmentUpdateError]]]) {

  def get(name: Name): Option[Declaration] =
    declarations.get(name)
  def apply(name: Name): Declaration =
    declarations(name)

  def value(name: Name): Option[Expr] =
    reductions.get(name).find(_.lhs.isInstanceOf[Const]).map(_.rhs)

  def isAxiom(name: Name): Boolean =
    !this(name).builtin && value(name).isEmpty

  private def addDeclsFor(mod: CompiledModification): Map[Name, Declaration] =
    declarations ++ mod.decls.view.map(d => d.name -> d)

  def addWithFuture(mod: Modification)(implicit executionContext: ExecutionContext): (Future[Option[EnvironmentUpdateError]], PreEnvironment) = {
    val compiled = mod.compile(this)
    val checkingTask = Future {
      Try(compiled.check()).failed.toOption.
        map(t => EnvironmentUpdateError(mod, t.getMessage))
    }
    checkingTask -> new PreEnvironment(addDeclsFor(compiled), reductions ++ compiled.rules, checkingTask :: proofObligations)
  }

  def addNow(mod: Modification): PreEnvironment = {
    val compiled = mod.compile(this)
    compiled.check()
    new PreEnvironment(addDeclsFor(compiled), reductions ++ compiled.rules, proofObligations)
  }

  def add(mod: Modification)(implicit executionContext: ExecutionContext): PreEnvironment =
    addWithFuture(mod)._2

  def force(implicit executionContext: ExecutionContext): Future[Either[Seq[EnvironmentUpdateError], Environment]] =
    Environment.force(this)
}

final class Environment private (declarations: Map[Name, Declaration], reductionMap: ReductionMap)
  extends PreEnvironment(declarations, reductionMap, Nil)
object Environment {
  def force(preEnvironment: PreEnvironment)(implicit executionContext: ExecutionContext): Future[Either[Seq[EnvironmentUpdateError], Environment]] =
    Future.sequence(preEnvironment.proofObligations).map(_.flatten).map {
      case Nil => Right(new Environment(preEnvironment.declarations, preEnvironment.reductions))
      case exs => Left(exs)
    }

  def default = new Environment(Map(), ReductionMap())
}