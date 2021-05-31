package info.kwarc.mmt.api.modules.diagrams

import info.kwarc.mmt.api.objects.{Context, OMID, OMS, Term}
import info.kwarc.mmt.api.symbols.{Constant, OMSReplacer}
import info.kwarc.mmt.api.{ComplexStep, GlobalName, LocalName}

// todo: rename this class as mmt API already features a "Renamer" class?
trait SystematicRenamer {
  def apply(name: LocalName): LocalName

  /**
    * Only renames symbols already processed in `state`.
    */
  def apply(path: GlobalName): GlobalName
/*
  /**
    * Always renames the symbol referred to by `path`.
    *
    * Use case: your operator has once processed `path` in a previous diagram operator invocation, but
    * in the meantime "forgot" about it. And now in a second operator invocation, you need to rename this symbol
    * still.
    */
  def applyAlways(path: GlobalName): GlobalName*/
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
trait SystematicRenamingUtils {
  this: LinearModuleTransformer =>
  protected lazy val emptyRenamer: SystematicRenamer = getRenamerFor("")

  protected def getRenamerFor(tag: String): SystematicRenamer = new SystematicRenamer {
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
    override def apply(path: GlobalName): GlobalName = {
      if (seenDeclarations(path.module).contains(path)) {
        applyAlways(path)
      } else {
        path
      }
    }

    def applyAlways(path: GlobalName): GlobalName = {
      val newModule = applyModulePath(path.module)
      val newName = path.name match {
        case LocalName(ComplexStep(domain) :: name) =>
          LocalName(applyModulePath(domain)) / apply(name)
        case name => apply(name)
      }

      newModule ? newName
    }

    def apply(term: Term): Term = {
      val self: SystematicRenamer = this // to disambiguate this in anonymous subclassing expression below
      new OMSReplacer {
        override def replace(p: GlobalName): Option[Term] = Some(OMS(self(p)))
      }.apply(term, Context.empty)
    }

    def apply(c: Constant): OMID = OMS(apply(c.path))
  }
}