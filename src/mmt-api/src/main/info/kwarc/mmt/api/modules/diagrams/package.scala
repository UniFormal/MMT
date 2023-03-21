package info.kwarc.mmt.api.modules

/**
  * <h1>A Framework of Diagram Operators</h1>
  *
  * [[DiagramOperator]]s are transformations on diagrams of formalizations (consisting of [[Theory theories]] and
  * [[View views]]), mapping diagrams to diagrams.
  * These operators shine when used to automate certain syntactical transformations on formalizations applied to large
  * libraries, e.g., to take a whole diagram of formalizations of Church-flavored languages and to transform it to a
  * corresponding diagram of Curry-flavored languages. As such, these operators can be used to structure libraries and
  * to reduce the amount of formalizations that needs to be humanly maintained.
  *
  * <h2>An Overview</h2>
  *
  * The most important diagram operators are:
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
  * Functors and connectors are <strong>exposed MMT surface syntax</strong> by additionally implementing the interfaces
  * [[NamedLinearFunctor]] and [[NamedLinearConnector]], which are
  * [[info.kwarc.mmt.api.SyntaxDrivenRule SyntaxDrivenRule]]s that are loaded via the `rule` keyword in surface
  * syntax and then bind to an (untyped) constant making the diagram operator accessible in surface syntax.
  *
  * Named diagram operators can then be used to compose [[DiagramInterpreter diagram expressions]] that are interpreted
  * and installed to the ambient theory graph using the [[InstallDiagram]] structural feature.
  *
  * Intuitively, we should think about functors and connectors as pure functions (no side effects and constants in the
  * same context are always translated the same way).
  * However, in practice [[LinearFunctor]] and [[LinearConnector]] are designed to add output modules to the theory
  * graph known to the [[info.kwarc.mmt.api.frontend.Controller Controller]] and specifically for operators that are
  * pure modulo maintain a "linear state" for efficiency reasons. A linear state means that when translating constants,
  * say, in a theory `T`, these operators have access to a state that only depends on all theories transitively included
  * into `T`. For example, operators may choose to maintain the linear state of "have I seen a constant of this and that
  * form yet?". In principle, such state can entirely be avoided (yielding a clean specification of the operator on
  * paper), but in practice such state is necessary for efficiency reasons.
  * Since linear states are for efficiency reasons only, it is fine to throw them away, e.g., between two MMT sessions.
  *
  * <h2>Design Decisions of the Framework</h2>
  *
  *  - separate interfaces for <strong>anonymous operators and operators bound to an MMT symbol</strong>
  *    This is useful to support named parametric operators that create anonymous (parametrized) operators at runtime.
  *    Moreover, we expect operators to also be repeatedly called programmatically in the future (in contrast to solely
  *    being invoked from surface syntax).
  *    <p>Note that parametric operators differ from [[info.kwarc.mmt.api.ParametricRule ParametricRule]]s in that
  *    for the former we want to make the parametric operator accessible once in a theory (using the `rule` keyword)
  *    to be able to invoke it subsequently many times, possibly with many different parametrizations.
  *    In contrast, parametric rules have so far been used such that the set of parametrizations occurring in practice
  *    is very small. This makes it feasible to implement parametrization of parametric rules by the `rule` keyword.
  *    For parametric operators this way is infeasible as users would need to use the `rule` keyword too many times.</p>
  *
  *  - <strong>operators are objects</strong>, invocations of operators are method calls on that objects, and <strong>
  *    state is maintained in those objects</strong> (in particular, this means across invocations of one and the same
  *    operator, possibly on different diagrams)
  *    <p>This requires operators to be particularly cautious about what they save into and load from their state.
  *    In theory, a cleaner solution would be to create a dedicated "state object" `st` before invoking a diagram
  *    operator on a given diagram, say, via `op.apply(diag, st)`. And the operator's `apply` method would forward
  *    that state object to all internally called methods, incl. the method that translates constants one-by-one.
  *    We were unable to implement this "cleaner" solution in a way that keeps the code base simple and type safe.
  *    Concretely, different operators may need to maintain different state, thus every operator should be able to
  *    dictate its own state interface. Now to keep things type safe, the `apply` method sketched above must take a
  *    generic type parameter for the state interface. And this carries through the entire code base, making things
  *    unbearable.</p>
  *
  *  - <strong>operators are generative, not applicative</strong>; operator inputs and outputs are diagrams of
  *    <emph>named</emph> theories and views; operator application adds to the ambient theory graph
  *    <p>The alternative would be that operators were applicative, that they input and output diagrams of
  *    <emph>anonymous</emph> theories and views (i.e., anonymous lists of declarations), and that operator application
  *    is a side-effect free computation (modulo caching some state, maybe).
  *    Applicative operators were once tried for the first iteration of diagram operators in MMT by Florian Rabe and Yasmine
  *    Sharoda. However, when the framework was scaled up (by Navid Roux in their M.Sc. thesis), it turned out that
  *    this alternative approach is inferior to the one that we have taken nowadays.
  *
  *     - Much of MMT's core API has been designed from the ground up as a module system for <emph>named</emph> modules.
  *       Named theories and views have matured over years. In contrast, the anonymous theories and views that were
  *       introduced for applicative operators back then could only be designed as an afterthought. For example, they
  *       were lists of [[info.kwarc.mmt.api.objects.OML OMLs]], abusing OMLs to represent constant declarations because
  *       [[info.kwarc.mmt.api.symbols.Constant Constant]]s are always fully-qualified named. Moreover, includes between
  *       anonymous theories could not be represented; and even if, they would not have scaled. In general, includes
  *       (imports, references, etc.) between anonymous modules does not scale in practice because anonymity precludes
  *       shared references, thus the memory necessitated for the representation of anonymous modules blows up.
  *     - Users want to inspect and interact with results of operator applications; even with intermediate results,
  *       e.g., in the application `O(O'(D))` consisting of operators O, O' and the diagram D, users want to have
  *       `O'(D)` to be accessible for inspection and interaction (e.g., for debugging).
  *       Since much of MMT's ecosystem is designed around inspecting and interacting with named modules (incl. the
  *       MMT IntelliJ plugin), named modules are a necessity for good UX.
  *
  *  - <strong>[[LinearFunctor]]s and [[LinearConnector]]s are decoupled</strong> interfaces, even for operators that
  *    come equipped with both.
  *    <p>It is common for operators to come equipped with at least one functor and one connector, and those two being
  *    mutually recursive. One prime example for this is the pushout operator.
  *    Should the framework force users to (A) implement two interfaces for these operators (and not help at all in
  *    guaranteeing the well-foundedness of mutual recursion) or (B) should it provide users with a single interface
  *    specifically designed for mutually recursive operators?</p>
  *    <p>An advantage of (B) is that it might keep the source code for a handful of closely related operators in a
  *    single place.
  *    Still, we should only couple operators this way if they must be run together. If not, we might couple operators
  *    that can later no longer be applied individually, meaning that users would clutter their theory graph with
  *    modules they do not even want (and slowing down thing with unnecessary computation, of course).
  *    As of now, we have not found a way to elegantly design an API for (B). How would an interface for coupled
  *    operators look like? How would implementors specify how many operators are coupled and of which kind?
  *    Would they give specification lists à la "one functor, one connector"? How would the API of the interface
  *    guarantee type-wise that such implementors indeed implement everything twice (once for the functor, once for the
  *    connector)?</p>
  *
  * @author Navid Roux
  * @since 2021
  */
package object diagrams {
}
