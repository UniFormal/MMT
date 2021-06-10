package info.kwarc.mmt.frameit.business

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.{Link, Theory}
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.{AnonymousTheory, OMMOD, Term, Traverser}
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api._
import info.kwarc.mmt.moduleexpressions.operators.PushoutUtils

import scala.collection.mutable

object Pushout {
  /**
    * Computes the canonical pushout along a direct inclusion and injects it into a pre-existing
    * theory.
    *
    * Pictorially, we compute the pushout
    *
    * {{{
    *    v
    * A ---> B
    * (      )
    * v      v
    * C ---> D
    *    w
    * }}}
    *
    * where the objects A-D are [[Theory theories]] and the morphisms v and w are [[Link links]].
    *
    * @param C A theory that includes A, even directly so, i.e., not transitively.
    * @param D The output theory, into which the declarations pushed out from C will be put into.
    *          It may already contain declarations as appropriate for your use case
    * @param v The link from A to B over which to push out.
    * @param w The link from C to D into which the assignments (relating declarations from C with the pushed
    *          out declarations put into D) into.
    *          It should be empty, but this is neither checked nor enforced.
    * @param addPosition The position at which pushed out declarations from C will be added to D via
    *                    [[Controller.add()]].
    *                    Usually, [[AtEnd]] (the default) suffices here. But if you do extravagant things
    *                    like having `D` prepopulated with `w` as a nested module, the pushed out declarations
    *                    need to go before `w` (because the assignments in `w` that will be generated will
    *                    reference them).
    *                    In that particular case, specify `Before(w.name.dropPrefix(D.name).get)`.
    *
    * @throws Possibly an [[AddError]] is thrown upon name clashes, e.g. C had a constant `c` with [[LocalName]]
    *                  `n` and D already has a constant with the same local name, thus `c` cannot be pushed out and
    *                  added to D.
    */
  def injectPushoutAlongDirectInclusion(A: Theory, B: Theory, C: Theory, D: Theory, v: Link, w: Link,
                                        addPosition: AddPosition = AtEnd)
                                       (implicit ctrl: Controller): Unit = {
    require(
      C.getAllIncludes.contains(IncludeData(OMMOD(C.path), A.path, Nil, None, total = false)),
      "C does not directly (i.e., not transitively) include A as required by this function"
    )
    require(
      v.from == OMMOD(A.path) && v.to == OMMOD(B.path),
      "Link v does not go from A to B as required by this function"
    )

    val translate: Term => Term = t => ctrl.library.ApplyMorphs(t, w.toTerm)

    for (decl <- C.getDeclarations) {
      decl match {
        case c: FinalConstant =>
          val pushedOutConstant = {
            val newC = new FinalConstant(
              home = D.toTerm,
              name = c.name,
              alias = c.alias,
              tpC = TermContainer.asParsed(c.tp.map(translate)),
              dfC = TermContainer.asParsed(c.df.map(translate)),
              rl = c.rl,
              notC = c.notC,
              vs = c.vs
            )
            val newMetadata = c.metadata.getAll.map {
              case MetaDatum(key, value: Term) => MetaDatum(key, translate(value))
              case x => x
            }
            newC.metadata.add(newMetadata: _*)
            newC
          }
          val pushoutAssignment = new FinalConstant(
            home = w.toTerm,
            name = LocalName(ComplexStep(c.parent) :: c.name),
            alias = Nil,
            tpC = pushedOutConstant.tpC.copy,
            dfC = TermContainer.asParsed(Some(pushedOutConstant.toTerm)),
            rl = None,
            notC = NotationContainer.empty(),
            vs = Visibility.public
          )

          ctrl.add(pushedOutConstant, addPosition)
          ctrl.add(pushoutAssignment)

        case SimpleStructure(_, fromPath) if fromPath == A.path =>
          val include = Include(D.toTerm, B.path, Nil, None)
          ctrl.add(include)
          ctrl.endAdd(include)

          val viewInclude = Include.assignment(w.toTerm, A.path, Some(v.toTerm))
          ctrl.add(viewInclude)
          ctrl.endAdd(viewInclude)

        case SimpleStructure(_, fromPath) if ctrl.globalLookup.hasImplicit(OMMOD(fromPath), A.toTerm) =>
          // in this case, we face an inclusion in C of a theory that was already included in A
          // we can safely skip this as we assume that C already includes A directly, and hence
          // the case above applies

        case _ => ???
      }
    }
  }
}
