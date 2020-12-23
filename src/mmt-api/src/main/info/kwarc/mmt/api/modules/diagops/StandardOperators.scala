package info.kwarc.mmt.api.modules.diagops

/**
  * Foundation-independent diagram operators.
  * Prime example is the pushout operator.
  */

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.DiagramInterpreter
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.{Constant, OMSReplacer}

// todo: rename this class as mmt API already features a "Renamer" class?
trait Renamer[T] {
  def apply(name: LocalName): LocalName
  def apply(path: GlobalName)(implicit state: T): GlobalName
  def apply(term: Term)(implicit state: T): Term
  def apply(c: Constant)(implicit state: T): OMID

  /**
    * @todo would like to have signature
    * {{{
    * def coercedTo[S <: SystematicRenamingUtils](implicit state: S#LinearState): Renamer[S] = {
    * }}}
    * but can't because LinearState is a protected type in LinerOperatorState. Change that to public?
    **/
  def coercedTo[S](implicit state: S): Renamer[S] = {
    implicit val TState: T = state.asInstanceOf[T]
    new Renamer[S] {
      override def apply(name: LocalName): LocalName = Renamer.this(name)
      override def apply(path: GlobalName)(implicit state: S): GlobalName = Renamer.this(path)
      override def apply(term: Term)(implicit state: S): Term = Renamer.this(term)
      override def apply(c: Constant)(implicit state: S): OMID = Renamer.this(c)
    }
  }
}

trait SystematicRenamingUtils extends LinearTransformer {
  protected def coerceRenamer[T](renamer: Renamer[T])(implicit state: LinearState): Renamer[LinearState] = {
    implicit val coercedState: T = state.asInstanceOf[T]
    new Renamer[LinearState] {
      override def apply(name: LocalName): LocalName = renamer(name)
      override def apply(path: GlobalName)(implicit state: LinearState): GlobalName = renamer(path)
      override def apply(term: Term)(implicit state: LinearState): Term = renamer(term)
      override def apply(c: Constant)(implicit state: LinearState): OMID = renamer(c)
    }
  }

  protected def getRenamerFor(tag: String): Renamer[LinearState] = new Renamer[LinearState] {
    override def apply(name: LocalName): LocalName = name.suffixLastSimple(tag)

    override def apply(path: GlobalName)(implicit state: LinearState): GlobalName = {
      if (state.processedDeclarations.exists(_.path == path)) {
        applyModulePath(path.module) ? apply(path.name)
      } else {
        path
      }
    }

    override def apply(term: Term)(implicit state: LinearState): Term = {
      val self = this // to disambiguate this in anonymous subclassing expression below
      new OMSReplacer {
        override def replace(p: GlobalName): Option[Term] = Some(OMS(self(p)))
      }.apply(term, state.outerContext)
    }

    override def apply(c: Constant)(implicit state: LinearState): OMID = OMS(apply(c.path))
  }
}

object CopyOperator extends ParametricRule {
  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(OMS(opSymbol), OMMOD(dom), OMMOD(cod)) => new CopyOperator(opSymbol, dom, cod)
    case _ => throw ParseError("invalid usage. correct usage: rule ...?CopyOperator <operator symbol " +
      "to tie with> <domain theory OMMOD> <codomain theory OMMOD>")
  }
}


class CopyOperator(override val head: GlobalName, dom: MPath, cod: MPath) extends SimpleLinearOperator with SystematicRenamingUtils with DefaultLinearStateOperator {

  override val operatorDomain: MPath = dom
  override val operatorCodomain: MPath = cod

  override def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_Copy")

  override def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter, state: LinearState): List[SimpleConstant] = {

    val copy1 = getRenamerFor("1")
    val copy2 = getRenamerFor("2")

    List(
      (copy1(name), copy1(tp), df.map(copy1.apply(_))),
      (copy2(name), copy2(tp), df.map(copy2.apply(_)))
    )
  }
}

/* to be reinstantiated by Navid (this comment is from 2020-12-11):

object PushoutOperator extends DiagramOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories/modexp-test?DiagramOperators?pushout_operator")

  override def apply(diagram: Term, interp: DiagramInterpreter)(implicit ctrl: Controller): Option[Term] = diagram match {
    case OMA(OMS(`head`), List(OMMOD(translationViewPath), diagram)) =>
      val translationView = ctrl.getAs(classOf[View], translationViewPath)

      new SimpleLinearOperator with DefaultLinearStateOperator {
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

        override protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit diagInterp: DiagramInterpreter, state: LinearState): List[List[(LocalName, Term, Option[Term])]] = {

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
                    Some(OMS(applyModulePath(container.modulePath) ? p.name))

                  case None => throw GeneralError("encountered unbound OMS reference for pushout")
                }
              }
            }

            replacer(_, state.outerContext)
          }


          List(
            List((name, pushout(tp), df.map(pushout))),
            List((name, tp, Some(OMID(applyModulePath(container.modulePath) ? name))))
          )
        }
      }.apply(OMA(OMS(head), List(diagram)), interp)

    case _ => None
  }
}

*/