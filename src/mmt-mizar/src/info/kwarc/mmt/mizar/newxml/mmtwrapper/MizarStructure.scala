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
import translator.TranslationController
import MMTUtils._

object MizarStructure {
  def elaborateAsMizarStructure(args: List[(Option[LocalName], Term)], fields: Context, substructs: List[Term], controller: Controller)(implicit parentTerm: GlobalName) = {
    val fieldDecls: List[OutgoingTermLevel] = fields.variables.toList map {vd =>
      val path = (parentTerm.module / parentTerm.name) ? vd.name
      new OutgoingTermLevel(path, args, vd.tp.get)
    }
    val params = fieldDecls.head.argContext()._1
    elaborateContent(params, fieldDecls, substructs, controller)
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
  def elaborateContent(params: Context, origDecls: List[InternalDeclaration], substr: List[Term], controller: Controller)(implicit parentTerm: GlobalName): List[Constant] = {
    val recordElabDecls = structuralfeatures.Records.elaborateContent(params, origDecls, controller)

    val argTps = origDecls.filter(_.isTypeLevel).map(d => OMV(LocalName(d.name)) % d.internalTp)
    val l = argTps.length
    val argsTyped = MMTUtils.freeVarContext(argTps map(_.toTerm))
    def refDecl(nm: String) = OMS(parentTerm.toMPath ? nm)

    val structx = ApplyGeneral(refDecl(recTypeName), params.variables.toList.map(_.toTerm))
    val makex = ApplyGeneral(refDecl(makeName), (params++argsTyped).variables.toList.map(_.toTerm))
    def typedArgsCont(tm: Term) = if ((params++argsTyped).isEmpty) tm else Pi(params++argsTyped, tm)
    val strict = VarDecl(structureStrictDeclName,typedArgsCont(
      Lam("s", structx, Mizar.prop)))
    val strictProp = VarDecl(structureStrictPropName,typedArgsCont(
      Lam("s", makex, Mizar.proof(Apply(refDecl(structureStrictDeclName.toString), OMV("s"))))))
    val substrRestr : List[VarDecl] = substr.zipWithIndex.flatMap {case (OMS(substrGN),i) =>
      val (substrPrePath, substrName) = (substrGN.module ? substrGN.name.init, substrGN.name.head)
      val subselectors = origDecls.map(_.path.name.last).filter(n => TranslationController.controller.getO(substrPrePath/n).isDefined) map (n => LocalName(n))
      val restrName = structureDefRestrName(substrName.toString)
      val restr = VarDecl(restrName,typedArgsCont(
        Pi(LocalName("s"),structx,OMS(substrPrePath/recTypeName))))
      val restrSelProps = subselectors map {n =>
        VarDecl(structureDefSubstrSelPropName(restrName,n),Mizar.eq(refDecl(restrName.toString),OMS(substrPrePath/restrName)))
      }
      restr::restrSelProps
    }
    val furtherDecls = (substrRestr++List(strict, strictProp)) map (_.toConstant(parentTerm.module,Context.empty))
    recordElabDecls ++ furtherDecls
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
      case PlainInclude(from, to) => (true, Some(OMMOD(to)))
      case _ => (false, None)
    }
    val substrPaths = dd.getDeclarations map (preProcessIncludes(_)) filter (_._1) map (_._2.get)
    val origDecls = parseInternalDeclarations(dd, controller, context) map {
      // Replace types by their definiens, whenever available
      case tml:OutgoingTermLevel => new OutgoingTermLevel(tml.path, tml.args, tml.df.getOrElse(tml.ret), tml.df)
      case intDecl => intDecl
    }
    val elabDecls = MizarStructure.elaborateContent(params, origDecls, substrPaths, controller)(parentTerm)
    externalDeclarationsToElaboration(elabDecls, Some({c => log(defaultPresenter(c)(controller))}))
  }
}

object MizarStructureRule extends StructuralFeatureRule(classOf[MizarStructure], "mizarStructure")