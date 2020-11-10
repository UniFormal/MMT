package info.kwarc.mmt.api.modules

import info.kwarc.mmt.api.checking.CheckingCallback
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.{Constant, OMSReplacer}
import info.kwarc.mmt.api._

trait SystematicRenamingUtils extends LinearOperator {
  trait Renamer {
    def apply(name: LocalName): LocalName
    def apply(path: GlobalName)(implicit state: LinearState): GlobalName
    def apply(term: Term)(implicit state: LinearState): Term
  }

  protected def getRenamerFor(tag: String, home: Term): Renamer = new Renamer {
    override def apply(name: LocalName): LocalName = name.suffixLastSimple("_" + tag)

    override def apply(path: GlobalName)(implicit state: LinearState): GlobalName = {
      if (state.processedDeclarations.exists(_.path == path)) {
        applyModulePath(path.module) ? apply(path.name)
      } else {
        path
      }
    }

    override def apply(term: Term)(implicit state: LinearState): Term = {
      val self: Renamer = this // to disambiguate this in anonymous subclassing expression below
      new OMSReplacer {
        override def replace(p: GlobalName): Option[Term] = Some(OMS(self(p)))
      }.apply(term, Context(home.toMPath))
    }
  }
}

object CopyOperator extends ParametricRule {
  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(OMS(opSymbol), OMMOD(dom), OMMOD(cod)) =>
      new SimpleLinearOperator with SystematicRenamingUtils with DefaultStateOperator {

        override val head: GlobalName = opSymbol
        override protected val operatorDomain: MPath = dom
        override protected val operatorCodomain: MPath = cod

        override def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_Copy")

        override val connectionTypes: List[ConnectionType] = List(
          InToOutMorphismConnectionType.suffixed("_CopyProjection1"),
          InToOutMorphismConnectionType.suffixed("_CopyProjection2")
        )

        override def applyConstantSimple(module: Module, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit solver: CheckingCallback, state: LinearState): List[List[(LocalName, Term, Option[Term])]] = {

          val copy1 = getRenamerFor("1", module.toTerm)
          val copy2 = getRenamerFor("2", module.toTerm)

          List(
            List(
              (copy1(name), copy1(tp), df.map(copy1.apply(_))),
              (copy2(name), copy2(tp), df.map(copy2.apply(_)))
            ),
            List((name, tp, Some(OMID(copy1(c.path))))),
            List((name, tp, Some(OMID(copy2(c.path)))))
          )
        }
      }

    case _ => throw ParseError("invalid usage. correct usage: rule ...?CopyOperator <operator symbol to tie with> <domain theory OMMOD> <codomain theory OMMOD>")
  }
}

object PushoutOperator extends DiagramOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories/modexp-test?DiagramOperators?pushout_operator")

  override def apply(diagram: Term, interp: DiagramInterpreter)(implicit ctrl: Controller): Option[Term] = diagram match {
    case OMA(OMS(`head`), List(OMMOD(translationViewPath), diagram)) =>
      val translationView = ctrl.getAs(classOf[View], translationViewPath)

      new SimpleLinearOperator with DefaultStateOperator {
        override val head: GlobalName = PushoutOperator.head
        override protected val operatorDomain: MPath = translationView.from.toMPath
        override protected val operatorCodomain: MPath = translationView.to.toMPath

        override protected val connectionTypes = List(
          new InToOutMorphismConnectionType {
            // suffixing complex steps doesn't work: ComplexStep(viewtranslationViewPath) / "projection"
            override def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_pushout_projection")
          }
        )

        // suffixing complex step doesn't work: / ComplexStep(translationViewPath)
        override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_pushout")

        override protected def applyConstantSimple(module: Module, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit solver: CheckingCallback, state: LinearState): List[List[(LocalName, Term, Option[Term])]] = {

          // cannot use [[ApplyMorphs]] here as the View we're constructing in parallel does not even exist yet
          // todo: isn't creation of a new OMSReplacer every time inefficient? How about creating a parametrized OMSReplacers?
          val pushout: Term => Term = {
            val replacer = new OMSReplacer {
              override def replace(p: GlobalName): Option[Term] = {
                // Using [[Lookup.getO]] (instead of [[Controller.getO]]) provides us with the logic
                // of assignments being looked up in included views and the identity view on the meta theory
                // of domain/codomain as well.
                // E.g. when working in LF and if p points to LF's arrow symbol, then most views users would write
                // don't map it. Rather it is mapped by the implicitly assumed identity on the domain/codomain's
                // meta theory (LF). And this logic is only implemented in [[Lookup.getO]].
                ctrl.globalLookup.getO(translationView.toTerm, LocalName(p.module) / p.name) match {
                  case Some(assignment@(_: Constant)) => Some(assignment.df.get)
                  case Some(_) => ???
                  case None if state.processedDeclarations.exists(_.path == p) =>
                    Some(OMS(applyModulePath(module.path) ? p.name))

                  case None => throw GeneralError("encountered unbound OMS reference for pushout")
                }
              }
            }

            replacer(_, state.outerContext)
          }


          List(
            List((name, pushout(tp), df.map(pushout))),
            List((name, tp, Some(OMID(applyModulePath(module.path) ? name))))
          )
        }
      }.apply(OMA(OMS(head), List(diagram)), interp)

    case _ => None
  }
}

