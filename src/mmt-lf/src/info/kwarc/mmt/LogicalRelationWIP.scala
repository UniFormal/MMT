package info.kwarc.mmt

import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.utils.UnicodeStrings
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.lf._

// todo: ask Florian where to put this file

object LogicalRelationWIP {
  private def star(name: LocalName): LocalName = name.suffixLastSimple("ᕁ")

  def traverser(links: List[Link], rel: GlobalName => Term)(implicit lookup: Lookup): Traverser[Option[Term]] = {
    ???
    /*
    new Traverser[Option[Term]] {
      override def traverse(t: Term)(implicit con: Context, targets: List[Term]): Term = t match {
        case OMS(Typed.ktype) =>
          FunType(
            links.zip(targets).map {
              case (link, target) => (None, lookup.ApplyMorphs(target, link.toTerm))
            },
            OMS(Typed.ktype)
          )

        case OMS(s) => ApplySpine.applyOrSymbol(rel(s), targets : _*)

        case OMV(name) => OMV(star(name))

        case ApplySpine(f, args) =>
          ApplySpine(
            traverse(f)(con, targets),
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
          val arguments = argNames.zip(argTypes.map(_._2))

          // Output is a new function type, which we will build with FunType

          // the context for the new return type
          var newContext = con

          def linkAnnotate(name: LocalName, linkIndex: Integer): LocalName = {
            name.suffixLastSimple(UnicodeStrings.superscriptInteger(linkIndex))
          }

          // build the new (Pi-bound) arguments, to be later passed to FunType()
          val newArgs: List[(LocalName, Term)] = arguments.flatMap {
            case (argName, argTp) =>
              val argTranslations = links.zipWithIndex.map {
                case (link, linkIndex) =>
                  val argNameInThisLink = linkAnnotate(argName, linkIndex)
                  val argTpInThisLink = lookup.ApplyMorphs(argTp, link.toTerm)

                  (argNameInThisLink, argTpInThisLink)
              }
              val argTranslationsVariables = argTranslations.map(argTranslation => OMV(argTranslation._1))

              val argTranslationsRelated = {
                (star(argName), ApplySpine(traverse(argTp), argTranslationsVariables: _*))
              }

              argTranslations :+ argTranslationsRelated
          }

          val appliedTargetArguments = links.zipWithIndex.map {
            case (link, linkIndex) =>
              ApplySpine(
                lookup.ApplyMorphs(target.get, link.toTerm),
                argNames.map(argName => OMV(linkAnnotate(argName, linkIndex))) : _*
              )
          }


              /*val blah = links.map(link => {
                lookup.ApplyMorphs(target.get, link)
              })
          }*/

          val newRetType = traverse(retType)(
            newContext,
            Some(ApplySpine.applyOrSymbol(target.get, argNames.map(argName => OMV(argName)) : _*))
          )

          FunType(newArgs, newRetType)

        case _ => Traverser(this, t)
      }
    }*/
  }
}

final class LogicalRelationTransformer(links: List[Link], commonLinkDomain: MPath, commonLinkCodomain: MPath) extends SimpleLinearModuleTransformer with SystematicRenamingUtils {

  override val operatorDomain: MPath = commonLinkDomain
  override val operatorCodomain: MPath = commonLinkCodomain

  // todo: encode links in name?
  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_logrel")

  val par: Renamer[LinearState] = getRenamerFor("ᵖ")
  val logrel: Renamer[LinearState] = getRenamerFor("ʳ")

  override protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter, state: LinearState): List[(LocalName, Term, Option[Term])] = {
    val traverser = LogicalRelationWIP.traverser(links, p => {
      if (p == c.path || state.processedDeclarations.exists(_.path == p)) {
        OMS(logrel(p))
      } else {
        return NotApplicable(c, "refers to constant not previously processed. Implementation error?")
      }
    })(interp.ctrl.globalLookup)

    val parCopy = (par(name), par(tp), df.map(par(_)))
    val logrelConstant = {
      // definienses not mapped so far
      (logrel(name), traverser(tp, Some(par(c))), None)
    }

    List(parCopy, logrelConstant)
  }
}

object LogicalRelationOperator extends ParametricLinearOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?logrel_operator")

  override def instantiate(parameters: List[Term])(implicit interp: DiagramInterpreter): Option[SimpleLinearModuleTransformer] = {
    val links = parameters.map {
      case OMMOD(linkPath) => interp.ctrl.getAs(classOf[Link], linkPath)
      case t =>
        interp.errorCont(InvalidObject(t, "cannot parse as path to link"))
        return None
    }

    val (domain, codomain) = {
      val domainsCodomains = links.map(link => (link.from, link.to)).unzip
      (domainsCodomains._1.distinct, domainsCodomains._2.distinct) match {
        case (List(dom), List(cod)) => (dom, cod)
        case _ =>
          interp.errorCont(GeneralError("passed links to logical relation operators must all have same domain/codomain"))
          return None
      }
    }

    Some(new LogicalRelationTransformer(links, domain.toMPath, codomain.toMPath))
  }
}


/*SimpleLinearOperator with SystematicRenamingUtils {
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
}*/