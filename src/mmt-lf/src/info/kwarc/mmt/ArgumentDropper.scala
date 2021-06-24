package info.kwarc.mmt

import info.kwarc.mmt.api.{ErrorHandler, GlobalName, InvalidObject, LocalName}
import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.notations.{Mixfix, NotationContainer}
import info.kwarc.mmt.api.objects.{Context, OMS, OMV, StatelessTraverser, Term, Traverser, VarDecl}
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.lf.{ApplySpine, Arrow, FunType, Lambda, Pi}

import scala.collection.mutable


/**
  * Utilities to drop arguments in constant declarations and terms.
  *
  * Two main methods are exposed:
  *
  *  - [[ArgumentDropper.clean()]]: remove argument positions from constants referenced in terms
  *  - [[ArgumentDropper.drop()]]: drop binders (lambda, pi) from type and definiens components of terms
  *
  * While it makes sense to call [[ArgumentDropper.clean()]] arbitrarily deep on subterms,
  * for [[ArgumentDropper.drop()]] it only makes sense to be called on type and definiens components
  * (the "most outer terms").
  *
  *
  * NOTE: removedArgs one-based everywhere (to be analogous to argument marker positions in notations).
  */
object ArgumentDropper {
  type ArgPath = Int
  type DropInfo = mutable.Map[GlobalName, Set[ArgPath]]

  /**
    * One-based since argument positions of notations are also one-based.
    */
  val INITIAL_PATH: ArgPath = 1

  /**
    * Cleans notation in a copy of input notation container
    * @todo from old code base, review it
    */
  def cleanArgumentsInNotation(p: GlobalName, notC: NotationContainer, dropped: DropInfo): NotationContainer = {
    notC.copy().collectInPlace(not => {
      val newFixity = not.fixity match {
        case fixity: Mixfix =>
          fixity.removeArguments(dropped.getOrElse(p, Nil).toSet)
        case _ => None // not implemented yet
      }
      newFixity.map(fix => not.copy(fixity = fix))
    })
  }

  def clean(t: Term, dropped: DropInfo)(implicit lookup: Lookup): Term = {
    new StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
        case ApplySpine.orSymbol(OMS(p), args) if dropped.get(p).exists(_.nonEmpty) =>
          val argsToRemove = dropped(p)

          val newArgs = args.zipWithIndex.filter {
            case (_, index) => !argsToRemove.contains(index + 1) // index is 0-based, argsToRemove 1-based
          }.map(_._1)

          if (args.size >= argsToRemove.max) { // (recall: argsToRemove has one-based argument positions)
            // All argument positions that we want to remove are given.
            ApplySpine.orSymbol(OMS(p), newArgs : _*)
          } else {
            // At least one argument position that we wanted to remove is not given,
            // hence reduce to eta-expanded case.
            //
            // Example: for c: GlobalName we shall remove arguments 1 and 3 and we're given the term t = `c x y`.
            //          Then etaExpand(c.path) = `\u. \v. \w.  c u v w`,
            //          and traverse(etaExpand(c.path)) = `\u. \v. \w.  c v`.
            //          And we return the term `(\u. \v. \w.  c v) x y`, which beta-reduces to `\w.  c y` as desired.
            etaExpand(p)
              .map(etaTerm => ApplySpine.orSymbol(traverse(etaTerm), args : _*))
              .getOrElse(t) // default to original term in case eta-expansion fails
          }

        case t => Traverser(this, t)
      }
    }.apply(t, Context.empty)
  }

  def drop(t: Term, args: Set[ArgPath], error: ErrorHandler): Option[Term] = {
    def f(t: Term, path: ArgPath): Option[Term] = t match {
      case t@(Pi(_, _, _) | Lambda(_, _, _)) =>
        val (v, tp, body) = (t: @unchecked) match {
          case Pi(v, tp, body) => (v, tp, body)
          case Lambda(v, tp, body) => (v, tp, body)
        }

        f(body, path + 1).flatMap(newBody => {
          if (args.contains(path)) {
            if (newBody.freeVars.contains(v)) {
              error(InvalidObject(t, s"Cannot remove lambda or pi binder at head of this term since the " +
                s"subterm (that had its binders dropped recursively already) `$newBody` still references the " +
                s"bound variable with name `$v`."))
              None
            } else {
              Some(newBody)
            }
          } else {
            // keep bound variable
            (t: @unchecked) match {
              case Pi(_, _, _) => Some(if (v == OMV.anonymous) Arrow(tp, newBody) else Pi(v, tp, newBody))
              case Lambda(_, _, _) => Some(Lambda(v, tp, newBody))
            }
          }
        })

      case t => Some(t)
    }

    f(t, INITIAL_PATH)
  }

  def getArguments(c: Constant): Option[Context] = {
    c.tp.collect {
      case FunType(argTypes, _) => argTypes.zipWithIndex.map {
        case ((maybeName, argType), idx) =>
          val name = maybeName.getOrElse(LocalName(s"arg$idx"))
          VarDecl(name, argType)
      }
    }
  }

  // can only eta-expand constants with function type component
  private def etaExpand(p: GlobalName)(implicit lookup: Lookup): Option[Term] = {
    getArguments(lookup.getConstant(p)).map(ctx => {
      Lambda(ctx, ApplySpine(OMS(p), ctx.map(_.toTerm) : _*))
    })
  }
}
