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
import structuralfeatures.RecordUtil.{converseEquivName, equivName, inductName, recExpPath, recTypeName, recTypePath, reprName}
import structuralfeatures.Records.{declaresRecords, elaborateContent}
import structuralfeatures.StructuralFeatureUtils.{Eq, parseInternalDeclarations}
import structuralfeatures.{InternalDeclaration, InternalDeclarationUtil, OutgoingTermLevel, Records, StructuralFeatureUtils, TermLevel, TypeLevel}
import MizSeq.{Ellipsis, OMI, nTerms}
import PatternUtils.{structureDefRestrName, structureDefSubstrSelPropName}
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

  def elaborateContent(params: Context, origDecls: List[InternalDeclaration], substr: List[Term], controller: Controller)(implicit parentTerm: GlobalName): List[Constant] = {
    val recordElabDecls = structuralfeatures.Records.elaborateContent(params, origDecls, controller)

    val argTps = params map (_.toTerm)
    val l = argTps.length
    val argsTyped = Ellipsis(OMI(l),LocalName("x"),Mizar.is(MizSeq.Index(OMV(LocalName("x")),OMV(LocalName("i"))), MizSeq.Index(MMTUtils.flatten(argTps),OMV(LocalName("i")))))

    val structx = Apply(OMV("struct"), MMTUtils.flatten(params.variables.toList.map(_.toTerm)))
    def typedArgsCont(nm:Option[String]= None) : (Term => Term) = { tm: Term => Pi(LocalName("x"), nTerms(l), nm match {
      case Some(name) => Pi(LocalName(name),argsTyped, tm)
      case None => Arrow(argsTyped, tm) })
    }
    val substrRestr : List[VarDecl] = substr.zipWithIndex.flatMap{case (OMS(substrPath),i) =>
      val (substruct, substrName, sl, sargTps, sm, sfieldDefs) = TranslationController.controller.get(substrPath) match {
        case subStruct @ StructureInstance(substrName, sl, sargTps, _, _, sm, sfieldDefs) => (subStruct, substrName, sl, sargTps, sm, sfieldDefs)
      }
      val restrName = structureDefRestrName(substrName)
      val restr = VarDecl(restrName,typedArgsCont(Some("p"))(Pi(LocalName("s"),structx,OMS(StructuralFeatureUtils.externalName(substrPath,LocalName("struct"))))))
      val restrSelProps = sfieldDefs map {vd =>
        VarDecl(structureDefSubstrSelPropName(restrName,vd.name),Mizar.eq(OMV(restrName),OMS(StructuralFeatureUtils.externalName(substrPath,restrName))))
      }
      restr::restrSelProps
    }
    val substrRestrDecls = substrRestr map (_.toConstant(parentTerm.module,Context.empty))
    recordElabDecls ++ substrRestrDecls
  }
}

import Records._

class MizarStructure extends StructuralFeature("mizarStructure") with MultiTypedParametricTheoryLike {

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
    val (params, substructs) = parseMultiTypedDerivedDeclaration(dd)
    implicit val parentTerm = dd.path
    val substrPaths = substructs map(_.path)
    val context = if (params.nonEmpty) {Some(params)} else {None}
    val origDecls = parseInternalDeclarations(dd, controller, context)
    val elabDecls = MizarStructure.elaborateContent(params, origDecls, substrPaths map(OMS(_)), controller)(parentTerm)
    externalDeclarationsToElaboration(elabDecls, Some({c => log(defaultPresenter(c)(controller))}))
  }
}

object MizarStructureRule extends StructuralFeatureRule(classOf[MizarStructure], "mizarStructure")