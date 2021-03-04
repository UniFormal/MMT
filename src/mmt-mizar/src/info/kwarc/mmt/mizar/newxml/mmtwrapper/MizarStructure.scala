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
import RecordUtil._
import StructuralFeatureUtils.{Eq, parseInternalDeclarations}
import PatternUtils._
import info.kwarc.mmt.mizar.newxml._
import translator.{DeclarationTranslationError, TranslationController}
import MMTUtils._
import MizarPrimitiveConcepts._

object MizarStructure {
  def elaborateAsMizarStructure(args: Context, fields: Context, substructs: List[Term], controller: Controller, notC: NotationContainer, slashFunction: Option[(LocalName, LocalName) => LocalName] = None)(implicit parentTerm: GlobalName) = {
    val fieldDecls: List[OutgoingTermLevel] = fields.variables.toList map {vd =>
      val path = (parentTerm.module / parentTerm.name) ? vd.name
      new OutgoingTermLevel(path, args.map(vd => (Some(vd.name), vd.toTerm)), vd.tp.get)
    }
    val params = fieldDecls.head.argContext()._1
    elaborateContent(params, fieldDecls, substructs, controller, notC, slashFunction)
  }

  /**
   *
   * @param params A context with the arguments to the structure (instance)
   * @param origDecls A list with the internal declarations declaring the fields (selectors) of the structure
   * @param substr A list of OMS containing referencing the (derived declarations) of the
   *               structure instances extended by this structure instance
   * @param controller
   * @param parentTerm (implicit) the path to this derived declaration
   * @return The list of constants forming the elaboration
   */
  def elaborateContent(params: Context, origDecls: List[InternalDeclaration], substr: List[Term], controller: Controller, notC: NotationContainer, slashFunction: Option[(LocalName, LocalName) => LocalName] = None)(implicit parentTerm: GlobalName): List[Constant] = {
    val recordElabDeclsNoNot = structuralfeatures.Records.elaborateContent(params, origDecls, controller)
    def pseudoSlash1: (LocalName, LocalName) => LocalName = slashFunction getOrElse {(a:LocalName, b:LocalName) => a / b}
    def replaceSlashesLN(n: LocalName) = n.steps.tail.foldLeft(LocalName(List(n.steps.head)))((nm, step) => pseudoSlash1(nm, LocalName(step)))
    def replaceSlashes(gn: GlobalName) = {
      OMS(gn.module ? replaceSlashesLN(gn.name))
    }
    val rep = OMSReplacer(gn => Some(replaceSlashes(gn)))
    val tr = {
      c: Constant =>
        val List(tpO: Option[Term], dfO: Option[Term]) = List(c.tp, c.df).map(_.map(rep.toTranslator()(Context.empty, _)))
        Constant(c.home, replaceSlashesLN(c.name), c.alias, tpO, dfO, c.rl, c.notC)
    }
    val recordElabDecls = (recordElabDeclsNoNot match {
      case Nil => Nil
      case hd::tl => Constant(hd.home, hd.name, hd.alias, hd.tp, hd.df, hd.rl, notC)::tl
    })

    val argTps = origDecls.filter(_.isTypeLevel).map(d => OMV(LocalName(d.name)) % d.internalTp)
    val l = argTps.length
    val argsTyped = MMTUtils.freeVarContext(argTps map(_.toTerm))
    def refDecl(nm: String) = OMS(parentTerm.module ? pseudoSlash1(parentTerm.name, LocalName(nm)))

    val structx = ApplyGeneral(refDecl(recTypeName), params.variables.toList.map(_.toTerm))
    def typedArgsCont = PiOrEmpty(params++argsTyped, _)
    val strict = VarDecl(structureStrictDeclName,typedArgsCont(
      Lambda(LocalName("s"), structx, prop)))
    val strictProp = VarDecl(structureStrictPropName,typedArgsCont(
      Lambda(LocalName("s"), structx, proof(Apply(refDecl(structureStrictDeclName.toString), OMV("s"))))))
      val substrRestr : List[VarDecl] = substr.zipWithIndex.flatMap {
        case (OMS(substrGN),i) =>
          val substrPrePath = substrGN.module ? substrGN.name.init
          val substrName = substrGN.toMPath.name.init.toString
          val subselectors = origDecls.map(_.path.name.last).filter(n => TranslationController.controller.getO(substrPrePath.module ? pseudoSlash1(substrPrePath.name, LocalName(n))).isDefined) map (n => LocalName(n))
          val restrName = structureDefRestrName(substrName)
          val restr = VarDecl(parentTerm.name / restrName,typedArgsCont(
            Pi(LocalName("s"),structx,OMS(substrPrePath / LocalName(recTypeName)))))
          val restrSelProps = subselectors map {n =>
            VarDecl(parentTerm.name/ structureDefSubstrSelPropName(restrName,n), Pi(LocalName("s"),structx, proof(MizarPrimitiveConcepts.eq(
              Apply(referenceExtDecl(substrPrePath, n.toString), Apply(refDecl(restrName.toString), OMV("s"))),
              Apply(refDecl(n.toString), OMV("s"))))))
          }
          restr::restrSelProps
        case tm => throw ImplementationError("Expected an OMS referencing the type declaration of a substructure, but instead found the term "+tm._1.toStr(true))
    }
    val furtherDecls = (substrRestr++List(strict, strictProp)) map (_.toConstant(parentTerm.module,Context.empty))
    (recordElabDecls ++ furtherDecls) map tr
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
    val elabDecls = MizarStructure.elaborateContent(params, origDecls, substrPaths, controller, notC)(parentTerm)
    externalDeclarationsToElaboration(elabDecls, Some({c => log(defaultPresenter(c)(controller))}))
  }
}

object MizarStructureRule extends StructuralFeatureRule(classOf[MizarStructure], "mizarStructure")