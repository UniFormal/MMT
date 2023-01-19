package info.kwarc.mmt.lean

import scala.collection.mutable
import info.kwarc.mmt.api._

class LibraryPrinter(env: PreEnvironment, notations: Map[Name, Notation],
    out: String => Unit,
    prettyOptions: PrettyOptions,
    lineWidth: Int = 80,
    printReductions: Boolean = false,
    printDependencies: Boolean = true) {
  private val declsPrinted = mutable.Map[Name, Unit]()
  def printDecl(name: Name): Unit = declsPrinted.getOrElseUpdate(name, {
    val tc = new TypeChecker(env, unsafeUnchecked = true)
    val pp = new PrettyPrinter(typeChecker = Some(tc), notations = notations, options = prettyOptions)

    val decl = env(name)
    if (printDependencies) {
      decl.ty.constants.foreach(printDecl)
      if (!prettyOptions.hideProofs || !tc.isProposition(decl.ty))
        env.value(name).foreach(_.constants.foreach(printDecl))
    }

    var doc = pp.pp(decl, env)

    val reds = env.reductions.get(name)
    if (printReductions && reds.nonEmpty) {
      doc = doc <> "/-" </> Doc.stack(reds.map {
        case ReductionRule(ctx, lhs, rhs, eqs) =>
          def mkEq(a: Expr, b: Expr): Expr = Apps(Const("eq", Vector(1)), Sort.Prop, a, b)
          val term1 = eqs.map((mkEq _).tupled).foldRight(mkEq(lhs, rhs))(_ -->: _)
          val term2 = ctx.foldRight(term1)(Lam(_, _))
          pp.parseBinders(term2) { (ctx_, t) =>
            Doc.text("reduction") <+> pp.nest(Doc.wordwrap(pp.telescope(ctx_) :+ Doc.text(":"))) </>
              pp.pp(t).parens(0).group <> Doc.line
          }
      }) <> "-/" <> Doc.line
    }

    out((doc <> Doc.line).render(lineWidth))
  })

  private val axiomsChecked = mutable.Map[Name, Unit]()
  def checkAxioms(name: Name): Unit = axiomsChecked.getOrElseUpdate(name, env(name) match {
    case Declaration(_, _, ty, _, _) =>
      ty.constants.foreach(checkAxioms)
      env.value(name).view.flatMap(_.constants).foreach(checkAxioms)
      if (env.isAxiom(name)) printDecl(name)
    // TODO: inductive, quotient
  })

  def handleArg(name: Name): Unit = {
    checkAxioms(name)
    printDecl(name)
  }

  val preludeHeader: String =
    """prelude
      |set_option eqn_compiler.lemmas false
      |set_option inductive.no_confusion false
      |set_option inductive.rec_on false
      |set_option inductive.brec_on false
      |set_option inductive.cases_on false
      |noncomputable theory
      |
      |""".stripMargin
}

case class MainOpts(
    inputFile: String = "",

    parallel: Boolean = true,

    printAllDecls: Boolean = false,
    printDecls: Seq[Name] = Seq(),
    printDependencies: Boolean = false,
    printReductions: Boolean = false,
    validLean: Boolean = false,

    showImplicits: Boolean = false,
    useNotation: Boolean = true,
    hideProofs: Boolean = true,
    hideProofTerms: Boolean = false) {
  def prettyOpts = PrettyOptions(
    showImplicits = showImplicits,
    hideProofs = hideProofs, hideProofTerms = hideProofTerms,
    showNotation = useNotation)
}
/*    opt[Unit]('s', "sequential").action((_, c) => c.copy(parallel = false))
      .text("type-check declarations on one thread only")

    opt[Unit]('a', "print-all-decls").action((_, c) => c.copy(printAllDecls = true))
      .text("print all checked declarations")
    opt[String]('p', "print-decl").unbounded().valueName("decl.name")
      .action((x, c) => c.copy(printDecls = c.printDecls :+ Name.ofString(x)))
      .text("print specified declarations")
    opt[Unit]('d', "print-dependencies")
      .action((_, c) => c.copy(printDependencies = true))
      .text("print dependencies of specified declarations as well")
    opt[Unit]('r', "print-reductions")
      .action((_, c) => c.copy(printReductions = true))
      .text("print reduction rules for specified declarations as well")
    opt[Unit]("valid-lean")
      .action((_, c) => c.copy(validLean = true, printDependencies = true, useNotation = false))
      .text("try to produce output that can be parsed again")

    opt[Boolean]("show-implicits").action((x, c) => c.copy(showImplicits = x))
      .text("show implicit arguments").valueName("yes/no")
    opt[Boolean]("use-notation").action((x, c) => c.copy(useNotation = x))
      .text("use notation for infix/prefix/postfix operators").valueName("yes/no")
    opt[Boolean]("hide-proofs").action((x, c) => c.copy(hideProofs = x))
      .text("hide proofs of lemmas").valueName("yes/no")
    opt[Boolean]("hide-proof-terms").action((x, c) => c.copy(hideProofTerms = x))
      .text("hide all proof terms").valueName("yes/no")
*/

object main {
  def main(args: Array[String]): Unit = {

  }
}