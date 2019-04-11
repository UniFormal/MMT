package info.kwarc.mmt.api

import objects._

/**
  * MMT objects are
  *  - [[Term]]s
  *  - [[Context]]s, which are lists of [[VarDecl]]
  *  - [[Substitution]]s, which are lists of [[Sub]]
  *
  * [[AnonymousDiagram]], [[AnonymousTheory]], and [[AnonymousMorphism]] represent anonymous counterparts to [[libraries.Library]] [[modules.Module}]].
  * 
  * This package also contains various auxiliary classes:
  * - [[Position]] defines paths within objects
  * - [[SubstitutionApplier]] is the main interface for substitution strategies.
  * - [[Matcher]] is a simple matcher.
  */
package object objects {
}
