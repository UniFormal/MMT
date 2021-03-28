package info.kwarc.mmt.mizar.newxml.mmtwrapper

import info.kwarc.mmt.api._
import objects._
import symbols._
import notations._
import checking._
import modules._
import frontend.Controller
import info.kwarc.mmt.lf._
import StructuralFeatureUtil._
import structuralfeatures._
import StructuralFeatureUtils.parseInternalDeclarations
import PatternUtils._
import info.kwarc.mmt.mizar.newxml._
import translator._
import MMTUtils._
import MizarPrimitiveConcepts._

object MizarStructure {
  def ancestorSubtypingDecls(params: Context, ancestorTps: List[Term])(implicit parentTerm: GlobalName) = ancestorTps map {
    case ancestorTp@ApplyGeneral(OMS(ancestorTpPath), _) =>
      val ancestorTpName = ancestorTpPath.module.name / ancestorTpPath.name.head.toString
      val tp = PiOrEmpty(params, forall(OMV("s"), ancestorTp, is(OMV("s"), ApplyGeneral(OMS(structureTypePath), params.variables.toList.map(_.toTerm)))))
      Constant(OMMOD(parentTerm.module), structureAncestorSubtypingPath(ancestorTpName).name, Nil,
        Some(tp), None, None)
    case ancestorTp@OMS(ancestorTpPath) =>
      val ancestorTpName = ancestorTpPath.module.name / ancestorTpPath.name.head.toString
      val tp = PiOrEmpty(params, forall(OMV("s"), ancestorTp, is(OMV("s"), ApplyGeneral(OMS(structureTypePath), params.variables.toList.map(_.toTerm)))))
      Constant(OMMOD(parentTerm.module), structureAncestorSubtypingPath(ancestorTpName).name, Nil,
        Some(tp), None, None)
    case _ => throw ImplementationError("This can never happen since all term and types are build using lf.apply and omses. ")
  }
  def elaborateAsMizarStructure(args: Context, fields: Context, ancestorTps: List[Term], controller: Controller, notCons: List[NotationContainer], slashFunction: Option[(LocalName, LocalName) => LocalName] = None)(implicit parentTerm: GlobalName) = {
    val fieldDecls: List[OutgoingTermLevel] = fields.variables.toList map {vd =>
      val path = (parentTerm.module / parentTerm.name) ? vd.name
      new OutgoingTermLevel(path, Nil, vd.tp.get)
    }
    val params = fieldDecls.head.argContext()._1
    elaborateContent(params, fieldDecls, ancestorTps, controller, notCons)
  }

  /**
   *
   * @param params A context with the arguments to the structure (instance)
   * @param origDecls A list with the internal declarations declaring the fields (selectors) of the structure
   * @param ancestorTps A list of OMS containing referencing the type declarations of the structure instances extended
   *               by this structure instance (the ancestors), applied to the arguments for said ancestors
   * @param controller
   * @param parentTerm (implicit) the path to this derived declaration
   * @return The list of constants forming the elaboration
   */
  def elaborateContent(params: Context, origDecls: List[InternalDeclaration], ancestorTps: List[Term], controller: Controller, notCons: List[NotationContainer])(implicit parentTerm: GlobalName): List[Constant] = {
    val recordElabDeclsNoNot = structuralfeatures.Records.elaborateContent(params, origDecls, controller)
    def replaceSlashesLN(n: LocalName) = n.steps.tail.foldLeft(LocalName(List(n.steps.head)))((nm, step) => PatternUtils.pseudoSlash(nm, LocalName(step)))
    val rep = OMSReplacer(gn => Some(OMS(gn.module ? replaceSlashesLN(gn.name))))
    val tr = {
      c: Constant =>
        val List(tpO: Option[Term], dfO: Option[Term]) = List(c.tp, c.df).map(_.map(rep.toTranslator()(Context.empty, _)))
        Constant(c.home, replaceSlashesLN(c.name), c.alias, tpO, dfO, c.rl, c.notC)
    }
    val strNot::aggrNot::forgNot::strictNot::selNots = notCons
    val recordElabDecls = ((recordElabDeclsNoNot zip strNot::aggrNot::selNots) zip structureTypePath::structureMakePath::recordElabDeclsNoNot.drop(2).map(d => structureSelectorPath(d.name)) map {
      case ((d: Constant, n:NotationContainer), gn) => Constant(OMMOD(gn.module), gn.name, d.alias, d.tp, d.df, d.rl, n)
    })

    val argTps = origDecls.filter(_.isTypeLevel).map(d => OMV(LocalName(d.name)) % d.internalTp)
    val l = argTps.length
    val argsTyped = MMTUtils.freeVarContext(argTps map(_.toTerm))

    val structTpx = ApplyGeneral(OMS(structureTypePath), params.variables.toList.map(_.toTerm))
    val strictDecl = Constant(OMMOD(parentTerm.module), structureStrictDeclPath.name, Nil,
      Some(PiOrEmpty(params, Pi(LocalName("s"), structTpx, prop))), Some(LambdaOrEmpty(params,
        Lam("s", structTpx, equal(OMV("s"), Apply(OMS(structureForgetfulFunctorPath), OMV("s")))))), None, strictNot)
    val forgetfulFunctorDecl = Constant(OMMOD(parentTerm.module), structureForgetfulFunctorPath.name, Nil,
      Some(Arrow(structTpx, structTpx)), None, None, forgNot)
    val furtherDecls = forgetfulFunctorDecl::strictDecl::ancestorSubtypingDecls(params, ancestorTps)
    (recordElabDecls ::: furtherDecls) map tr
  }
}

import Records._

class MizarStructure extends StructuralFeature("mizarStructure") with ParametricTheoryLike {

  /**
   * Checks the validity of the mizar structure to be constructed
   * @param dd the derived declaration from which the mizar structure is to be constructed
   */
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) : Unit = {}

  /**
   * Elaborates the declaration of a Mizar structure into the external declarations of a record type,
   * as well as the restrictions to the substructures and the corresponding definition axioms
   * @param parent The parent module of the declared inductive types
   * @param dd the derived declaration to be elaborated
   */
  def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration)(implicit env: Option[uom.ExtendedSimplificationEnvironment] = None): Elaboration = {
    val params = Type.getParameters(dd)
    implicit val parentTerm = dd.path
    val context = if (params.nonEmpty) {Some(params)} else {None}
    def preProcessIncludes(d:Declaration): (Boolean, Option[Term]) = d match {
      case PlainInclude(from, to) =>
        assert(dd.path.toMPath == from)
        (true, Some(OMMOD(to)))
      case _ => (false, None)
    }
    val substrPaths = dd.getDeclarations map (preProcessIncludes(_)) filter (_._1) map (_._2.get)
    val origDecls = parseInternalDeclarations(dd, controller, context) map {
      // Replace types by their definiens, whenever available
      case tml:OutgoingTermLevel => new OutgoingTermLevel(tml.path, tml.args, tml.df.getOrElse(tml.ret), tml.df)
      case intDecl => intDecl
    }
    val notC = dd.not map(NotationContainer(_)) getOrElse NotationContainer.empty()
    val notCs = (1 to origDecls.length+3).map(_ => NotationContainer.empty()).toList
    val elabDecls = MizarStructure.elaborateContent(params, origDecls, substrPaths, controller, notC::notCs)(parentTerm)
    externalDeclarationsToElaboration(elabDecls, Some({c => log(defaultPresenter(c)(controller))}))
  }
}

object MizarStructureRule extends StructuralFeatureRule(classOf[MizarStructure], "mizarStructure")