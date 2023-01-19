package info.kwarc.mmt.lf.structuralfeatures

import info.kwarc.mmt.api._
import objects._
import symbols._
import notations._
import checking._
import modules._
import frontend.Controller

import info.kwarc.mmt.lf._
import InternalDeclaration._
import InternalDeclarationUtil._

import RecordUtil._
import symbols.StructuralFeatureUtil._
import StructuralFeatureUtils._
import TermConstructingFeatureUtil._

/** theories as a set of types of expressions */ 
class RecordDefinitions extends StructuralFeature("record_term") with TypedParametricTheoryLike {

  /**
   * Checks the validity of the inductive type(s) to be constructed
   * @param dd the derived declaration from which the inductive type(s) are to be constructed
   */
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = {}
  
  /**
   * Elaborates an declaration of one or multiple mutual inductive types into their declaration, 
   * as well as the corresponding no confusion and no junk axioms
   * Constructs a structure whose models are exactly the (not necessarily initial) models of the declared inductive types
   * @param parent The parent module of the declared inductive types
   * @param dd the derived declaration to be elaborated
   */
  def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration)(implicit env: Option[uom.ExtendedSimplificationEnvironment] = None) = {
    implicit val parentTerm = dd.path
    val (recDefPath, context, indParams) = ParamType.getParams(dd)
    if (declaresRecords(parent)) {elaborateToRecordExp(context, indParams)} else {
      val recD = controller.library.get(recDefPath) match {
        case recD: DerivedDeclaration if (recD.feature == "record") => recD
        case d: DerivedDeclaration => throw LocalError("the referenced derived declaration is not of the feature record but of the feature "+d.feature+".")
        case _ => throw LocalError("Expected definition of corresponding record at "+recDefPath.toString()
              +" but no derived declaration found at that location.")
      }
      val indCtx = controller.extman.get(classOf[StructuralFeature], recD.feature).getOrElse(throw LocalError("Structural feature "+recD.feature+" not found.")) match {
        case f: ParametricTheoryLike => f.Type.getParameters(recD)
        case _ => Context.empty
      }
      
      var decls = parseInternalDeclarationsWithDefiniens(dd, controller, Some(context))
      val recDefs = parseInternalDeclarations(recD, controller, None)
      val types = tpls(recDefs)
          
      // check whether all declarations match their corresponding constructors
      decls foreach { d => correspondingDecl(recD, d.name) map {decl =>
        checkDecl(d, fromConstant(decl,controller, types, None))}
      }
      // and whether we have all necessary declarations
      recDefs.map(_.name).find(n => !decls.map(_.name).contains(n)) foreach {n => throw LocalError("No declaration found for the internal declaration "+n+" of "+recD.name+".")}
      
      val Ltp = () => {
        PiOrEmpty(context, ApplyGeneral(ApplyGeneral(OMS(recDefPath / LocalName(recTypeName)), indCtx map (_.toTerm)), decls.filter(_.isTypeLevel).map(d => d.df.get)))
      }
      val Ldf = () => {
        Some(LambdaOrEmpty(context, ApplyGeneral(ApplyGeneral(OMS(recDefPath / LocalName(makeName)), context map (_.toTerm)), decls.map(_.df.get))))
      }
      
      val make = makeConst(LocalName(makeName), Ltp, false, Ldf)
      //log(defaultPresenter(make)(controller))
      
      new Elaboration {
        def domain = List(make.name)
        def getO(n: LocalName) = {
          List(make) find (_.name == n) foreach (d => log(defaultPresenter(d)(controller)))
          List(make) find (_.name == n)
        }
      }
    }
  }
  
  /**
   * checks whether d.tp matches the type decl.externalTp
   * @param d the declaration whoose type to check
   * @param decl the corresponding declaration which is to be defined by d
   * @note this will return an error if d.tp doesn't match decl.externalTp
   */
  def checkDecl(d: InternalDeclaration, decl: InternalDeclaration): Unit = {//TODO: This should be implemented to provide more accurate error messages
    
  }
  
  def declaresRecords(home: ModuleOrLink) = {
    val knownDecls = home.getDeclarations.map(_.path)
    knownDecls.contains(recExpPath)
  }
  
  def elaborateToRecordExp(ctx: Context, params: List[Term])(implicit parent: GlobalName) = {
    val recordFields = ctx.filter(d=>isTypeLevel(d.tp.get)).map(_.toOML)
    Elaboration(List(makeConst(parent.name, 
        () => {OMA(OMS(recTypePath), recordFields)}, false,
        () => {Some(OMA(OMS(recExpPath), params))})))
  }
}

object RecordDefinitionRule extends StructuralFeatureRule(classOf[RecordDefinitions], "record_term")