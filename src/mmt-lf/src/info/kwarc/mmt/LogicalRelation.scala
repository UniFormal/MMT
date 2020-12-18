package info.kwarc.mmt

import info.kwarc.mmt.api.modules.{DiagramInterpreter, Renamer, SimpleLinearOperator, SystematicRenamingUtils}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.utils.UnicodeStrings
import info.kwarc.mmt.api.{GlobalName, LocalName, MPath, Path}
import info.kwarc.mmt.lf._

// todo: ask Florian where to put this file

object LogicalRelation {
  private def star(name: LocalName): LocalName = name.suffixLastSimple("ᕁ")

  def singleIdentityLogrel(rel: GlobalName => Term): Traverser[Option[Term]] = {
    new Traverser[Option[Term]] {
      override def traverse(t: Term)(implicit con: Context, target: Option[Term]): Term = t match {
        case OMS(Typed.ktype) =>
          FunType(List((None, target.get)), OMS(Typed.ktype))

        case OMS(s) => target match {
          case Some(t) => ApplySpine(rel(s), t)
          case None => rel(s)
        }

        case OMV(name) => OMV(star(name))

        case ApplySpine(f, args) =>
          ApplySpine(
            traverse(f)(con, None),
            args.flatMap(arg => List(
              arg,
              traverse(arg)(con, Some(arg))
            )) : _*
          )

        case Lambda(x, tp, body) =>
          val tpRecursed = traverse(tp)(con ++ VarDecl(x, tp), Some(OMV(x)))
          val bodyRecursed = traverse(body)(con ++ VarDecl(x, tp) ++ VarDecl(star(x), tpRecursed), None)

          Lambda(x, tp, Lambda(star(x), tpRecursed, bodyRecursed))

        /**
          * Unifying case for function types with named or unnamed arguments:
          *
          * (a) A_1 -> ... -> A_n -> B
          * (b) \Pi (a: A_1). ... \Pi (a_n: A_n). B
          *
          * or mixed forms thereof.
          */
        case FunType(argTypes, retType) if argTypes.nonEmpty =>
          val argNames = argTypes.zipWithIndex.map {
            case ((Some(argName), _), _) => argName
            case ((None, _), index) => LocalName("x" + UnicodeStrings.subscriptInteger(index))
          }

          // Output is a new function type, which we will build with FunType

          // the context for the new return type
          var newContext = con

          // build the new (Pi-bound) arguments, to be later passed to FunType()
          val newArgs: List[(Option[LocalName], Term)] = argNames.zip(argTypes.map(_._2)).flatMap {
            case (argName, argTp) =>
              // build `\Pi argName: argTp. \Pi argName*: recurse(argTp, argName)
              val argTpRecursed = traverse(argTp)(con ++ VarDecl(argName, argTp), Some(OMV(argName)))

              newContext ++= VarDecl(argName, argTp)
              newContext ++= VarDecl(star(argName), argTpRecursed)

              List(
                (Some(argName), argTp),
                (Some(star(argName)), argTpRecursed)
              )
          }

          val newRetType = traverse(retType)(
            newContext,
            Some(ApplySpine.applyOrSymbol(target.get, argNames.map(argName => OMV(argName)) : _*))
          )

          FunType(newArgs, newRetType)

        case _ => Traverser(this, t)
      }
    }
  }
}

object UnaryIdentityLogicalRelationOperator extends SimpleLinearOperator with SystematicRenamingUtils {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?unary_id_logrel_operator")

  override val operatorDomain: MPath = LF.theoryPath
  override val operatorCodomain: MPath = LF.theoryPath

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("id_log_rel")

  val par: Renamer[LinearState] = getRenamerFor("ᵖ")
  val logrel: Renamer[LinearState] = getRenamerFor("ʳ")

  override protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter, state: LinearState): List[(LocalName, Term, Option[Term])] = {
    val traverser = LogicalRelation.singleIdentityLogrel(p => {
      if (p == c.path || state.processedDeclarations.exists(_.path == p)) {
        OMS(logrel(p))
      } else {
        return NotApplicable(c, "refers to constant not previously processed. Implementation error?")
      }
    })

    val parCopy = (par(name), par(tp), df.map(par(_)))
    val logrelConstant = {
      // definienses not mapped so far
      (logrel(name), traverser(tp, Some(par(c))), None)
    }

    List(parCopy, logrelConstant)
  }
}