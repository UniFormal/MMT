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
import structuralfeatures.InternalDeclaration.{isTypeLevel, structureDeclaration}
import structuralfeatures.RecordUtil._
import structuralfeatures.Records.{declaresRecords, elaborateContent}
import structuralfeatures.StructuralFeatureUtils.{Eq, parseInternalDeclarations}
import structuralfeatures.{InternalDeclaration, InternalDeclarationUtil, OutgoingTermLevel, Records, StructuralFeatureUtils, TermLevel, TypeLevel}
import MizSeq.{Ellipsis, OMI, nTerms}
import PatternUtils._
import info.kwarc.mmt.mizar.newxml.translator.TranslationController

object MizarStructure {
  def elaborateAsMizarStructure(declarationPath: GlobalName, args: List[(Option[LocalName], Term)], fields: Context, substructs: List[Term], controller: Controller)(implicit parentTerm: GlobalName) = {
    val fieldDecls: List[OutgoingTermLevel] = fields.variables.toList map {vd =>
      val path = (declarationPath.module / declarationPath.name) ? vd.name
      new OutgoingTermLevel(path,args, vd.tp.get)
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

    val argTps = params map (_.toTerm)
    val l = argTps.length
    val argsTyped = Ellipsis(OMI(l),LocalName("x"),Mizar.is(MizSeq.Index(OMV(LocalName("x")),OMV(LocalName("i"))), MizSeq.Index(MMTUtils.flatten(argTps),OMV(LocalName("i")))))

    val structx = Apply(OMV(recTypeName), MMTUtils.flatten(params.variables.toList.map(_.toTerm)))
    val makex = Apply(OMV(makeName), MMTUtils.flatten(params.variables.toList.map(_.toTerm)))
    def typedArgsCont(nm:Option[String]= None) : (Term => Term) = { tm: Term => Pi(LocalName("x"), nTerms(l), nm match {
      case Some(name) => Pi(LocalName(name),argsTyped, tm)
      case None => Arrow(argsTyped, tm) })
    }
    val strictDecl = VarDecl(structureStrictName,typedArgsCont()(
      Pi(LocalName("s"),structx,Mizar.prop)))
    val strictPropDecl = VarDecl(structureStrictName,typedArgsCont()(
      Pi(LocalName("s"),makex,Mizar.proof(Apply(OMV(structureStrictName), OMV("s"))))))
    val substrRestr : List[VarDecl] = substr.zipWithIndex.flatMap{case (OMS(substrGN),i) =>
      val substrPath = substrGN.module / substrGN.name
      val (substruct, substrName, sl, sargTps, sm, sfieldDefs) = TranslationController.controller.get(substrPath) match {
        case subStruct @ StructureInstance(substrName, sl, sargTps, _, _, sm, sfieldDefs) => (subStruct, substrName, sl, sargTps, sm, sfieldDefs)
      }
      val restrName = structureDefRestrName(substrName)
      val restr = VarDecl(restrName,typedArgsCont(Some("p"))(
        Pi(LocalName("s"),structx,OMS(StructuralFeatureUtils.externalName(substrGN,LocalName("struct"))))))
      val restrSelProps = sfieldDefs map {vd =>
        VarDecl(structureDefSubstrSelPropName(restrName,vd.name),Mizar.eq(OMV(restrName),OMS(StructuralFeatureUtils.externalName(substrGN,restrName))))
      }
      restr::restrSelProps
    }
    val substrRestrDecls = substrRestr map (_.toConstant(parentTerm.module,Context.empty))
    recordElabDecls ++ substrRestrDecls
  }
}

import Records._

class MizarStructure extends StructuralFeature("mizarStructure") with ParametricTheoryLike {

  /**
   * Checks the validity of the mizar structure to be constructed
   * @param dd the derived declaration from which the mizar structure is to be constructed
   */
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {}

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
    val origDecls = parseInternalDeclarations(dd, controller, context)
    val elabDecls = MizarStructure.elaborateContent(params, origDecls, substrPaths, controller)(parentTerm)
    externalDeclarationsToElaboration(elabDecls, Some({c => log(defaultPresenter(c)(controller))}))
  }
}

object MizarStructureRule extends StructuralFeatureRule(classOf[MizarStructure], "mizarStructure")