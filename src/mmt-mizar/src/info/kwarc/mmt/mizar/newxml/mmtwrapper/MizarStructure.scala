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
import translator.{DeclarationTranslationError, ExpectedTheoryAt, TranslatingError, TranslationController}
import MMTUtils._
import MizarPrimitiveConcepts._

object MizarStructure {
  /**
   * Given the type of an ancestor of a structure, retrieve all transitive ancestors of said ancestor
   * @param str the type of the ancestor
   * @return a ist of types of ancestors (well-typed in the context consisting of the arguments of the structure)
   */
  def getTransitiveAncestors(str: Term): List[Term] = str match {
    case ApplyGeneral(OMS(strGnPostfix), params) =>
      implicit val strGn = strGnPostfix.module ? strGnPostfix.name.init
      val mod = if (TranslationController.currentTheoryPath == strGn.module) TranslationController.currentThy else TranslationController.controller.getO(strGn.module) map {
        case thy: Theory => thy
        case m => throw ExpectedTheoryAt(strGn.module)
      } get
        val prefix = structureDefRestrName("anything").init
      val args = mod.get(LocalName(recTypeName)).asInstanceOf[Constant].tp.get match {
        case PiOrEmpty(params, Univ(1)) => params
        case _ => Context.empty
      }
      (str::(mod.domain.filter(_.init == prefix).map(prefix / _) map (mod.get(_).asInstanceOf[Constant].tp.get) flatMap {
        case PiOrEmpty(_, substr) => getTransitiveAncestors(substr)
      })) map (_ ^ Substitution(args zip params map {case (a, b) => a.toTerm / b} :_*))
    case _ => throw new ImplementationError("This can never happen.")
  }
  def getTransitiveAncestors(str: GlobalName): List[Term] = {
    implicit val strGn = str.module ? str.name.init
    val mod: Theory = if (TranslationController.currentTheoryPath == strGn.module) TranslationController.currentThy else TranslationController.controller.getO(strGn.module) map {
      case thy: Theory => thy
      case m => throw ExpectedTheoryAt(strGn.module)
    } get
    val PiOrEmpty(args, Univ(1)) = mod.get(LocalName(recTypeName)).asInstanceOf[Constant].tp.get
    getTransitiveAncestors(ApplyGeneral(OMS(strGn / LocalName(recTypeName)), args map (_.toTerm)))
  }
  def elaborateAsMizarStructure(args: Context, fields: Context, substructs: List[Term], controller: Controller, notCons: List[NotationContainer], slashFunction: Option[(LocalName, LocalName) => LocalName] = None)(implicit parentTerm: GlobalName) = {
    val fieldDecls: List[OutgoingTermLevel] = fields.variables.toList map {vd =>
      val path = (parentTerm.module / parentTerm.name) ? vd.name
      new OutgoingTermLevel(path, args.map(vd => (Some(vd.name), vd.toTerm)), vd.tp.get)
    }
    val params = fieldDecls.head.argContext()._1
    elaborateContent(params, fieldDecls, substructs, controller, notCons.toArray, slashFunction)
  }

  /**
   *
   * @param params A context with the arguments to the structure (instance)
   * @param origDecls A list with the internal declarations declaring the fields (selectors) of the structure
   * @param substr A list of OMS containing referencing the type declarations (derived declarations) of the
   *               structure instances extended by this structure instance
   * @param controller
   * @param parentTerm (implicit) the path to this derived declaration
   * @return The list of constants forming the elaboration
   */
  def elaborateContent(params: Context, origDecls: List[InternalDeclaration], substr: List[Term], controller: Controller, notCons: Array[NotationContainer], slashFunction: Option[(LocalName, LocalName) => LocalName] = None)(implicit parentTerm: GlobalName): List[Constant] = {
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
    val recordElabDecls = (recordElabDeclsNoNot.zipWithIndex map {
      case (d: Constant, ind:Int) if (ind < notCons.init.length) => Constant(d.home, d.name, d.alias, d.tp, d.df, d.rl, notCons(ind))
      case (d, _) => d
    })

    val argTps = origDecls.filter(_.isTypeLevel).map(d => OMV(LocalName(d.name)) % d.internalTp)
    val l = argTps.length
    val argsTyped = MMTUtils.freeVarContext(argTps map(_.toTerm))
    def refDecl(nm: String) = OMS(parentTerm.module ? pseudoSlash1(parentTerm.name, LocalName(nm)))

    val structx = ApplyGeneral(refDecl(recTypeName), params.variables.toList.map(_.toTerm))
    val strict = VarDecl(structureStrictDeclName,PiOrEmpty(params++argsTyped,
      Lambda(LocalName("s"), structx, prop)))
    val strictProp = VarDecl(structureStrictPropName,PiOrEmpty(params++argsTyped,
      Lambda(LocalName("s"), structx, proof(Apply(refDecl(structureStrictDeclName.toString), OMV("s"))))))
      val substrRestr : List[VarDecl] = substr.zipWithIndex.flatMap {
        case (ApplyGeneral(OMS(substrGN), argParams),i) =>
          val substrPrePath = substrGN.module ? substrGN.name.init
          val substrName = substrGN.toMPath.name.init.toString
          val subselectors = origDecls.map(_.path.name.last).filter(n => TranslationController.controller.getO(substrPrePath.module ? pseudoSlash1(substrPrePath.name, LocalName(n))).isDefined) map (n => LocalName(n))
          val restrName = structureDefRestrName(substrName)
          val restr = VarDecl(parentTerm.name / restrName,PiOrEmpty(params++argsTyped,
            Pi(LocalName("s"), structx, ApplyGeneral(OMS(substrPrePath / LocalName(recTypeName)), argParams))))
          val restrSelProps = subselectors map {n =>
            VarDecl(parentTerm.name/ structureDefSubstrSelPropName(restrName,n), Pi(LocalName("s"),structx, proof(MizarPrimitiveConcepts.eq(
              Apply(referenceExtDecl(substrPrePath, n.toString), Apply(refDecl(restrName.toString), OMV("s"))),
              Apply(refDecl(n.toString), OMV("s"))))))
          }
          restr::restrSelProps
        case tm => throw ImplementationError("Expected an OMS referencing the type declaration of a substructure, but instead found the term "+tm._1.toStr(true))
    }
    val fstRestr = symbols.Constant(OMMOD(parentTerm.module), substrRestr.head.name, Nil, substrRestr.head.tp, substrRestr.head.df, None, notCons.last)
    val furtherDecls = substrRestr.tail++List(strict, strictProp) map (_.toConstant(parentTerm.module,Context.empty))
    (recordElabDecls ::: fstRestr::furtherDecls) map tr
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
    val elabDecls = MizarStructure.elaborateContent(params, origDecls, substrPaths, controller, Array(notC))(parentTerm)
    externalDeclarationsToElaboration(elabDecls, Some({c => log(defaultPresenter(c)(controller))}))
  }
}

object MizarStructureRule extends StructuralFeatureRule(classOf[MizarStructure], "mizarStructure")