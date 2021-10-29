package info.kwarc.mmt.api.modules

/**
  * [[DiagramOperator]]s perform operations on entire [[Diagram diagrams]] of MMT [[Theory theories]] and [[View views]].
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
  * [[LinearModuleOperator.applyConstant applyConstant]] that describes their action on constants. Everything else,
  * i.e., the induced translation on [[info.kwarc.mmt.api.symbols.IncludeData includes]],
  * [[info.kwarc.mmt.api.symbols.Structure structure]]s, [[Theory theories]], [[View views]],
  * [[info.kwarc.mmt.api.symbols.NestedModule nested theories and views]], and [[Diagram diagram]]s is given automatically.
  *
  * Functors and connectors can be exposed to MMT surface syntax by using the extended interfaces
  * [[NamedLinearFunctor]] and [[NamedLinearConnector]], which are
  * [[info.kwarc.mmt.api.SyntaxDrivenRule SyntaxDrivenRule]]s that are loaded via the `rule` keyword in surface
  * syntax and then bind to an (untyped) constant making the diagram operator accessible in surface syntax.
  * 
  * TODO rewrite/organize/remove this
  *
  * * global invariant: diagrams are closed under includes (included thy is either in diagram itself or in meta diagram), otherwise maybe unexpected/partial behavior only
  *
  *
  * See [[DiagramInterpreter.apply]] for the syntax of diagram expressions.
  *
  * Design decisions
  * ====================
  *
  * - separation into named and anonymous operators in Operators.scala and LinearTransformer.scala,
  *   respectively.
  *
  *   Named operators are associated to an MMT symbol and inherit SyntaxDrivenRule.
  *
  * - use cases of anonymous operators so far:
  *
  *   - named parametric operators that create anonymous (parametrized!) operators on-the-fly at runtime
  *
  * - Parametric operators *cannot* be implemented by ParametricRules:
  *
  *   For parametric operators, you'd like to load them once via ''rule scala://...ParametricOperator''
  *   and to be able to use them afterwards with *arbitrary* parameters.
  *   If they were ParametricRules, you'd have to have a ''rule'' declaration every time you'd like
  *   to use the operator with a different set of parameters.
  *
  * - DiagramState is complicated because you'd like every operator to be able to carry its own state
  *   (which might be more than the default state) while ensuring type safety.
  *
  * - Invariant of operator states:
  *
  *   - operator states are a mathematical function of the context (i.e. pure)
  *   - we should throw the states away after processing ''diagram'' declarations
  *     => if later another ''diagram'' declaration appears that necessitates some of the thrown away
  *     states, we just recompute
  */
package object diagrams {
}
