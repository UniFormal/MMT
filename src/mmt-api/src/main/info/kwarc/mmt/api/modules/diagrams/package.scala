package info.kwarc.mmt.api.modules

/*
   TODO rewrite/organize/remove this

  * Diagram operators implementation:
  *
  * in mmt-api, it consists of files {Diagram, DiagramState, DiagramTransformer, FunctorialOperator, StandardOperators}.scala.
  *
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

/**
  * See [[DiagramInterpreter.apply()]] for the syntax of diagram expressions.
  */
package object diagrams {
}
