package info.kwarc.mmt.api.modules
// TODO: document why anonymous diagrams were inferior
// TODO: decoupling connectors from functors has advantages (flexibility, e.g., where not all connectors are needed, e.g., between two functors, these are only desired if both functors are used anyway) and disadvantages (functors and connectors that are mutually recursive)
/**
  * [[UnaryOperator]]s perform operations on entire [[Diagram diagrams]] of MMT [[Theory theories]] and [[View views]].
  * An important special case is given by [[LinearModuleOperator]]s: these map diagrams to diagrams, module-by-module,
  * and declaration-by-declaration.
  * The most important [[LinearModuleOperator]]s are the following two:
  *
  *  - [[info.kwarc.mmt.api.modules.diagrams.LinearFunctor LinearFunctor]]s `F` map theories `T` to theories `F(T)`
  *    and views `v: S -> T` to views `F(v): F(S) -> F(T)`.
  *  - [[info.kwarc.mmt.api.modules.diagrams.LinearConnector LinearConnector]]s `C` between two [[LinearFunctor]]s
  *    `F` and `G` map theories `T` to views `C(T): F(T) -> G(T)`.
  *
  * Implementations of both functors and connectors effectively only need to give a single method
  * [[LinearModuleOperator.applyConstant applyConstant]] that describes their action on constants (in a given context).
  * Everything else, i.e., the induced translation on [[info.kwarc.mmt.api.symbols.IncludeData includes]],
  * [[info.kwarc.mmt.api.symbols.Structure structure]]s, [[Theory theories]], [[View views]],
  * [[info.kwarc.mmt.api.symbols.NestedModule nested theories and views]], and [[Diagram diagram]]s is given automatically.
  *
  * Functors and connectors are exposed to MMT surface syntax by additionally implementing the interfaces
  * [[NamedLinearFunctor]] and [[NamedLinearConnector]], which are
  * [[info.kwarc.mmt.api.SyntaxDrivenRule SyntaxDrivenRule]]s that are loaded via the `rule` keyword in surface
  * syntax and then bind to an (untyped) constant making the diagram operator accessible in surface syntax.
  *
  * Named diagram operators can then be used to compose [[DiagramInterpreter diagram expressions]] that are interpreted
  * and installed to the ambient theory graph using the [[InstallDiagram]] structural feature.
  *
  * Intuitively, we should think about functors and connectors as pure functions (no side effects and constants in the
  * same context are always translated the same way).
  * However, in practice [[LinearFunctor]] and [[LinearConnector]] are designed specifically for operators that are
  * pure modulo maintain a "linear state" for efficiency reasons. A linear state means that when translating constants,
  * say, in a theory `T`, these operators have access to a state that only depends on all theories transitively included
  * into `T`. For example, operators may choose to maintain the linear state of "have I seen a constant of this and that
  * form yet?". In principle, such state can entirely be avoided (yielding a clean specification of the operator on
  * paper), but in practice such state is necessary for efficiency reasons.
  * Since linear states are for efficiency reasons only, it is fine to throw them away, e.g., between two MMT sessions.
  *
  * Design Decisions
  *
  *  - separate interfaces for anonymous diagram operators and named ones that are bound to an MMT symbol
  *    This is useful to support named parametric operators that create anonymous (parametrized operators at runtime.
  *    Moreover, we expect operators to also be repeatedly called programmatically in the future (in contrast to solely
  *    being invoked from surface syntax).<br><br>
  *    Note that parametric operators differ from [[info.kwarc.mmt.api.ParametricRule ParametricRule]]s in that
  *    for the former we want to make the parametric operator accessible once in a theory (using the `rule` keyword)
  *    to be able to invoke it subsequently many times, possibly with many different parametrizations.
  *    In contrast, parametric rules have so far been used such that the set of parametrizations occurring in practice
  *    is very small. This makes it feasible to implement parametrization of parametric rules by the `rule` keyword.
  *    For parametric operators this way is infeasible as users would need to use the `rule` keyword too many times.
  *
  *  - operators are objects, invocations of operators are method calls on that objects, and state is maintained in
  *    those objects (in particular, this means across invocations of one and the same operator, possibly on different
  *    diagrams)<br><br>
  *    This requires operators to be particularly cautious about what they save into and load from their state.
  *    In theory, a cleaner solution would be to create a dedicated "state object" `st` before invoking a diagram
  *    operator on a given diagram, say, via `op.apply(diag, st)`. And the operator's `apply` method would forward
  *    that state object to all internally called methods, incl. the method that translates constants one-by-one.
  *    We were unable to implement this "cleaner" solution in a way that keeps the code base simple and type safe.
  *    Concretely, different operators may need to maintain different state, thus every operator should be able to
  *    dictate its own state interface. Now to keep things type safe, the `apply` method sketched above must take a
  *    generic type parameter for the state interface. And this carries through the entire code base, making things
  *    unbearable.
  */
package object diagrams {
}
