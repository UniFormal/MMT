package info.kwarc.mmt.api.modules.diagops

import info.kwarc.mmt.api.{ComplexStep, GlobalName, InvalidElement, LocalName}
import info.kwarc.mmt.api.modules.{DiagramInterpreter, Module}
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.{Context, OMID, OMMOD, OMS, Term}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, FinalConstant, OMSReplacer, TermContainer, Visibility}

// DSL
trait OperatorDSL extends DefaultLinearStateOperator with SystematicRenamingUtils {
  object NotApplicable {
    def apply[T](c: Declaration, msg: String = "")(implicit state: LinearState, interp: DiagramInterpreter): List[T] = {
      state.registerSkippedDeclaration(c)
      interp.errorCont(InvalidElement(
        c,
        s"`$getClass` not applicable" + (if (msg.nonEmpty) ": " + msg else "")
      ))

      Nil
    }
  }

  // note: do not make df an optional Scala argument, as then users could confuse const and assgn
  // without getting compilation errors
  /**
    * Creates a constant; effectiely a wrapper around [[Constant.apply()]].
    *
    * Only use in [[LinearModuleTransformer]]!!
    */
  def const(p: GlobalName, tp: Term, df: Option[Term])(implicit state: LinearState): Constant = {
    require(p.module == state.outContainer.path)

    new FinalConstant(
      home = OMMOD(p.module),
      name = p.name, alias = Nil,
      tpC = TermContainer.asAnalyzed(tp), dfC = TermContainer.asAnalyzed(df),
      rl = None, notC = NotationContainer.empty(), vs = Visibility.public
    )
  }

  /**
    * Creates an assignment for a view represented by a [[FinalConstant]].
    *
    * Only use in [[LinearConnectorTransformer]]s!!
    */
  def assgn(p: GlobalName, assignment: Term)(implicit state: LinearState): Constant = {
    new FinalConstant(
      home = state.outContainer.toTerm,
      name = ComplexStep(p.module) / p.name, alias = Nil,
      tpC = TermContainer.empty(), dfC = TermContainer.asAnalyzed(assignment),
      rl = None, notC = NotationContainer.empty(), vs = Visibility.public
    )
  }
}

// todo: rename this class as mmt API already features a "Renamer" class?
trait Renamer[T] {
  def apply(name: LocalName): LocalName
  def apply(path: GlobalName)(implicit state: T): GlobalName
  def apply(term: Term)(implicit state: T): Term
  def apply(c: Constant)(implicit state: T): OMID

  /**
    * @todo would like to have signature
    * {{{
    * def coercedTo[S <: SystematicRenamingUtils](implicit state: S#LinearState): Renamer[S] = {
    * }}}
    * but can't because LinearState is a protected type in LinerOperatorState. Change that to public?
    **/
  def coercedTo[S](implicit state: S): Renamer[S] = {
    implicit val TState: T = state.asInstanceOf[T]
    new Renamer[S] {
      override def apply(name: LocalName): LocalName = Renamer.this(name)
      override def apply(path: GlobalName)(implicit state: S): GlobalName = Renamer.this(path)
      override def apply(term: Term)(implicit state: S): Term = Renamer.this(term)
      override def apply(c: Constant)(implicit state: S): OMID = Renamer.this(c)
    }
  }
}

/**
  * Utilities and DSL to systematically rename constants in [[LinearTransformer]]s.
  *
  * These utilities are meant to be invoked within [[LinearTransformer.applyDeclaration()]]
  * or methods called therein; in particular [[LinearTransformer.applyConstant()]],
  * [[SimpleLinearModuleTransformer.applyConstantSimple()]], and
  * [[SimpleLinearConnectorTransformer.applyConstantSimple()]].
  *
  * Only renames constants seen so far while processing (incl. the declaration being processed
  * right now).
  * Concretely, the methods herein depend on declarations being added to
  * `state.processedDeclarations` *before* they are passed to [[LinearTransformer.applyDeclaration()]].
  * See also the pre-condition of [[LinearTransformer.applyDeclaration()]].
  *
  *
  * @todo add example
  */
trait SystematicRenamingUtils extends LinearTransformer {
  protected def coerceRenamer[T](renamer: Renamer[T])(implicit state: LinearState): Renamer[LinearState] = {
    implicit val coercedState: T = state.asInstanceOf[T]
    new Renamer[LinearState] {
      override def apply(name: LocalName): LocalName = renamer(name)
      override def apply(path: GlobalName)(implicit state: LinearState): GlobalName = renamer(path)
      override def apply(term: Term)(implicit state: LinearState): Term = renamer(term)
      override def apply(c: Constant)(implicit state: LinearState): OMID = renamer(c)
    }
  }

  protected def getRenamerFor(tag: String): Renamer[LinearState] = new Renamer[LinearState] {
    override def apply(name: LocalName): LocalName = name.suffixLastSimple(tag)

    /**
      * Bends a path pointing to something in `state.inContainer` to a path
      * pointing to the systematically renamed variant in `state.outContainer`.
      *
      * @example Suppose `path = doc ? thy ? c` where c is a [[SimpleStep]],
      *          then `apply(path) = applyModulePath(doc ? thy) ? apply(c)`
      * @example Suppose we are in a view and encounter an assignment with
      *          `path = doc ? view ? ([doc ? thy] / c)` where `[doc ? thy]`.
      *          Here, `[doc ? thy]` is a [[ComplexStep]] encoding the domain
      *          the constant to be assigned.
      *          Then,
      *          `apply(path) = applyModulePath(doc ? view) ? ([applyModulePath(doc ? thy)] / c)`.
      * @todo Possibly, this method is wrong for nested theories and views. Not tested so far.
      */
    override def apply(path: GlobalName)(implicit state: LinearState): GlobalName = {
      if (state.processedDeclarations.exists(_.path == path)) {
        val newModule = applyModulePath(path.module)
        val newName = path.name match {
          case LocalName(ComplexStep(domain) :: name) =>
            LocalName(applyModulePath(domain)) / apply(name)
          case name => apply(name)
        }

        newModule ? newName
      } else {
        path
      }
    }

    override def apply(term: Term)(implicit state: LinearState): Term = {
      val self = this // to disambiguate this in anonymous subclassing expression below
      new OMSReplacer {
        override def replace(p: GlobalName): Option[Term] = Some(OMS(self(p)))
      }.apply(term, state.outContext) // todo: is this the right context?
    }

    override def apply(c: Constant)(implicit state: LinearState): OMID = OMS(apply(c.path))
  }
}