package info.kwarc.mmt.api.modules

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api._

import scala.collection.mutable

trait DiagramOperator extends SyntaxDrivenRule {
  // need access to Controller for generative operators (i.e. most operators)
  def apply(diagram: Term)(implicit ctrl: Controller): Option[Term]
}

trait NameInferrableDiagramOperator extends DiagramOperator {
  def transformModuleName(name: LocalName): LocalName

  final def apply(mpath: MPath): MPath = mpath.doc ? transformModuleName(mpath.name)
}

trait LinearOperator extends NameInferrableDiagramOperator {
  protected val operatorSymbol: GlobalName
  protected val operatorDomain: MPath
  protected val operatorCodomain: MPath

  final override def head: GlobalName = operatorSymbol

  private def transformModule(modulePath: MPath, elementPaths: List[MPath], elements: Map[MPath, Module], processedContexts: mutable.Map[MPath, (Context, List[Declaration])])(implicit ctrl: Controller): Unit = {
    processedContexts.get(modulePath) match {
      // already processed
      case Some(x) => return
      case _ => // nop: keep on processing
    }

    assert(elementPaths.contains(modulePath))
    assert(elements.contains(modulePath))
    val module = ctrl.getAs(classOf[Module], modulePath)

    val newDeclarations = mutable.ListBuffer[Declaration]()

    // For theories, the initial context is just the meta theory -- if it exists
    // For views, it is the context of the codomain
    val initialCtx = Context(operatorDomain.toMPath) ++ (module match {
      case thy: Theory if thy.meta.isDefined => Context(thy.meta.get)
      case link: Link => link.to match {
        case OMMOD(codomainOfLink) =>
          transformModule(codomainOfLink, elementPaths, elements, processedContexts)
          processedContexts.getOrElse(codomainOfLink, ???)._1

        case _ =>
          ???
      }
      case _ => Context.empty
    })

    var ctx = initialCtx

    module.getDeclarations.foreach {
      // IncludeData(home: Term, from: MPath, args: List[Term], df: Option[Term], total: Boolean)
      case Include(IncludeData(_, from, Nil, df, total)) =>
        val newFrom: MPath = from match {
          case `operatorDomain` =>
            operatorCodomain.toMPath

          case from if elementPaths.contains(from) =>
            transformModule(from, elementPaths, elements, processedContexts)
            ctx ++= processedContexts.getOrElse(from, ???)._1

            apply(from)

          case from if ctrl.globalLookup.hasImplicit(OMMOD(from), OMMOD(operatorDomain)) =>
            // e.g. for a view v: ?S -> ?T and S, T both having meta theory ?meta,
            //      the view will feature an "include ?meta = OMIDENT(OMMOD(?meta))"
            //      but in general it might be something else
            //
            // todo: what to do here? add to context? just retain and hope there's an implicit morphism from from to operatorCodomain, too?
            from

          case _ =>
            // theory included wasn't contained in diagram actually
            ???
        }

        val newDf: Option[Term] = df.map {
          case OMIDENT(`operatorDomain`) =>
            OMIDENT(OMMOD(operatorCodomain))

          case OMIDENT(thy) if ctrl.globalLookup.hasImplicit(thy, OMMOD(operatorDomain)) =>
            // e.g. for a view v: ?S -> ?T and S, T both having meta theory ?meta,
            //      the view will feature an "include ?meta = OMIDENT(OMMOD(?meta))"
            //      but in general it might be something else
            //
            // todo: what to do here? add to context? just retain and hope there's an implicit morphism from from to operatorCodomain, too?
            OMIDENT(thy)

          case OMMOD(dfPath) =>
            if (elementPaths.contains(dfPath)) {
              transformModule(dfPath, elementPaths, elements, processedContexts)
              ctx ++= processedContexts.getOrElse(dfPath, ???)._1
              OMMOD(apply(dfPath))
            } else {
              // error: morphism provided as definiens to include wasn't contained in diagram
              ???
            }

          case _ =>
            ???
        }

        newDeclarations += IncludeData(
          home = OMMOD(apply(module.path)),
          from = newFrom,
          args = Nil,
          df = newDf,
          total = total
        ).toStructure

      case c: Constant =>
        newDeclarations ++= transformConstant(c, ctx)

        // embed full path information into name of VarDecl (hack because Contexts cannot contain
        // constants otherwise)
        ctx ++= OML(LocalName(ComplexStep(c.path.module) :: c.name), c.tp, c.df, c.not).vd

      case _ =>
        // problem: all declaration types other than FinalConstant cannot be put into ctx
        //          so not sure how to handle them
        ???
    }

    assert(!processedContexts.contains(module.path))
    processedContexts.put(module.path, (ctx, newDeclarations.toList))
  }

  override def apply(diagram: Term)(implicit ctrl: Controller): Option[Term] = diagram match {
    case OMA(OMS(`operatorSymbol`), List(SimpleDiagram(dom, _))) if dom != operatorDomain =>
      // todo check for implicit morphism from `domain` to actual domain
      ???

    // circumvent some MMT parsing bug
    case OMA(OMS(`operatorSymbol`), List(OMBINDC(_, _, t))) =>
      apply(OMA(OMS(operatorSymbol), t))
    case OMA(OMS(`operatorSymbol`), List(SimpleDiagram(`operatorDomain`, elementPaths))) =>
      val processedContexts = mutable.Map[MPath, (Context, List[Declaration])]()

      val elements = elementPaths.map(path => (path, ctrl.getAs(classOf[Module], path))).toMap

      // transform all elements and store results in processedContexts
      elementPaths.foreach(transformModule(_, elementPaths, elements, processedContexts))

      // construct diagram
      val newPaths: List[MPath] = processedContexts.map {
        case (oldPath, (_, newDeclarations)) =>
          val oldModule = elements.getOrElse(oldPath, ???)
          val newPath = apply(oldModule.path)

          val newModule: Module = oldModule match {
            case thy: Theory =>
              Theory.empty(newPath.doc, newPath.name, thy.meta)

            case view: View =>
              View(
                newPath.doc,
                newPath.name,
                OMMOD(apply(view.from.toMPath)),
                OMMOD(apply(view.to.toMPath)),
                isImplicit = view.isImplicit
              )

            case _ => ???
          }

          ctrl.add(newModule)
          newDeclarations.foreach(ctrl.add(_))

          newPath
      }.toList

      Some(SimpleDiagram(operatorCodomain, newPaths))

    case _ =>
      // complex case
      ???
  }

  // todo: how should this method signal *unapplicability*/partiality (i.e. error)?
  // def transformDeclaration(decl: Declaration, context: Context): List[Declaration]
  def transformConstant(c: Constant, context: Context): List[Declaration]
}

abstract class SimpleLinearOperator extends LinearOperator {
  def transformSimpleConstant(name: LocalName, tp: Term, df: Option[Term], context: Context): List[(LocalName, Term, Option[Term])]

  final override def transformConstant(c: Constant, context: Context): List[Declaration] = {
    transformSimpleConstant(c.name, c.tp.getOrElse(return Nil), c.df, context).map {
      case (newName, tp, df) =>
        new FinalConstant(
          home = OMMOD(apply(c.path.module)),
          name = newName,
          alias = Nil,
          tpC = TermContainer.asParsed(tp),
          dfC = TermContainer.asParsed(df),
          rl = None,
          notC = NotationContainer.empty(),
          vs = c.vs
        )
    }
  }
}

trait SystematicRenaming extends NameInferrableDiagramOperator {
  protected def rename(tag: String, name: LocalName): LocalName = name.suffixLastSimple("_" +tag)
  protected final def rename(tag: String, path: GlobalName): GlobalName = apply(path.module) ? rename(tag, path.name)

  protected def rename(tag: String, ctx: Context, term: Term): Term = {
    new OMSReplacer {
      override def replace(p: GlobalName): Option[Term] = ctx.collectFirst {
        case vd if vd.name == LocalName(ComplexStep(p.module) :: p.name) =>
          OMS(rename(tag, p))
      }
    }.apply(term, ctx)
  }
}

object CopyOperator extends ParametricRule {
  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(OMS(opSymbol), OMMOD(dom), OMMOD(cod)) =>
      new SimpleLinearOperator with SystematicRenaming {

        override protected val operatorSymbol: GlobalName = opSymbol
        override protected val operatorDomain: MPath = dom
        override protected val operatorCodomain: MPath = cod

        override def transformModuleName(name: LocalName): LocalName = name.suffixLastSimple("_copy")

        override def transformSimpleConstant(name: LocalName, tp: Term, df: Option[Term], ctx: Context): List[(LocalName, Term, Option[Term])] = {

          List(
            (rename("1", name), rename("1", ctx, tp), df.map(rename("1", ctx, _))),
            (rename("2", name), rename("2", ctx, tp), df.map(rename("2", ctx, _))),
          )
        }
      }

    case _ => throw ParseError("invalid usage. correct usage: rule ...?CopyOperator <operator symbol to tie with> <domain theory OMMOD> <codomain theory OMMOD>")
  }
}

object SimpleDiagram {
  private val constant = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?simple_diagram", NamespaceMap.empty)

  def apply(baseTheory: MPath, paths: List[MPath]): Term = {
    OMA(OMS(constant), OMMOD(baseTheory) :: paths.map(OMMOD(_)))
  }
  def unapply(t: Term): Option[(MPath, List[MPath])] = t match {
    case OMA(OMS(`constant`), OMMOD(baseTheory) :: pathTerms) =>
      val paths = pathTerms.collect {
        case OMMOD(path) => path
        case _ => return None
      }
      Some((baseTheory, paths))

    case _ => None
  }
}
