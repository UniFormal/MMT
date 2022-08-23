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
import StructuralFeatureUtils._
import StructuralFeatureUtil._

object inductiveUtil {
  /**
   * name of the noConf declaration corresponding to c and d
   */
  def noConfName(a:LocalName, b:LocalName):LocalName = {LocalName("no_conf_"+a.toString+"_"+b.toString)}
  /**
   * name of the declaration corresponding to n declared in noJunks
   */
  def inductName(n: LocalName) = {LocalName("induct") / n}
  
  /** Name of the declaration generated in testers */
  def testerName(n: LocalName) = {n / LocalName("?")}
  
  /** Name of the declaration generated in unappliers */
  def unapplierName(n: LocalName) = {n / LocalName("-1")}
    
  def proofPredName(n: LocalName): LocalName = {LocalName("pred") / n}
  /** name of the declarations generated in indProofs */
  def proofName(n: LocalName): LocalName = {LocalName("ind_proof") / n}
  /** name of the applied version of the inductive definition declaration generated for constructors */
  def appliedName(n: LocalName) : LocalName = {n / LocalName("Applied")}
  def idAdditionalGenerated(c:Constant) = {
    c.name match {
      case al / b =>
        val a = LocalName(al.toString)
        val generated = List(inductName(b),testerName(a),unapplierName(a),proofPredName(a),proofName(a),appliedName(a)) contains c.name
      case nm if ((nm.toString contains "injective_") ||  (nm.toString contains "no_conf_")) => true
      case _ => false
    }
  }

  def feature = {"inductive"}
  //features generating inductive types as output, which may be referenced by inductive definitions, inductive proofs, ...
  def compatibleFeatures = {List(feature, ReflectionsUtil.feature)}

}
import inductiveUtil._

/** theories as a set of types of expressions */ 
class InductiveTypes extends StructuralFeature(inductiveUtil.feature) with ParametricTheoryLike {
  
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
    val context = Type.getParameters(dd)
    implicit val parentTerm = dd.path
        
    val decls = parseInternalDeclarations(dd, controller, Some(context))
    elaborateDeclarations(context, decls, Some({c => log(defaultPresenter(c)(controller))}))
    }
  
  /** Elaborates an derived declaration D using the inductive feature. This is used to reuse the functionality of this feature in different features, speciafically the reflection feature.
   *  @param context The context of D
   *  @param parentTerm The path to D, used as prefix for the external declarations
   *  @param decls The internal declaration of the D
   */
  def elaborateDeclarations(context: Context, decls: List[InternalDeclaration], logger: Option[Constant => Unit] = None)(implicit parentTerm: GlobalName) : Elaboration = {
    // to hold the result
    var elabDecls : List[Constant] = Nil
    
    val tpdecls = tpls(decls)
    val tmdecls = tmls(decls)
    val constrdecls = constrs(tmdecls)
    val types = tpdecls map (_.path)
    
    // copy all the declarations
    decls foreach {d => elabDecls ::= d.toConstant}

    //check them
    tmdecls foreach { tmdecl => checkTermLevel(tmdecl, types)}

    // the no confusion axioms for the data constructors
    /*
     * For dependently-typed constructors, we cannot elaborate into plain LF:
     * some of the (in)equality axioms would be ill-typed because
     * the type system already forces elements of different instances of a dependent type to be unequal and of unequal type
     */
    elabDecls = elabDecls.reverse ::: tmdecls.flatMap(x => noConf(x, tmdecls, types)(parentTerm))
    
    // the no junk axioms
    elabDecls ++= noJunks(decls, context)(parentTerm)
    
    // the testers
    elabDecls ++= testers(tmdecls, tpdecls, decls, context)(parentTerm)
    
    // the unappliers
    elabDecls ++= unappliers(constrdecls, tpdecls, decls, context)(parentTerm)
    
    // the inductive proof declarations
    elabDecls ++= indProofs(tpdecls, constrdecls, context)(parentTerm)

    externalDeclarationsToElaboration(elabDecls, logger)
}
  
  /** Check whether the TermLevel has a higher order argument of an inductively defined type
   *  In that case an error is thrown
   */
  def checkTermLevel(tc: TermLevel, types: List[GlobalName])= {
    def dependsOn(tm: Term, tp: GlobalName): Boolean = {
      val FunType(args, ret) = tm
      args exists {arg => val ApplyGeneral(tpConstr, tpArgs) = arg._2; tpConstr == OMS(tp)}  // TODO: Is this the right condition to check for?
    }
    if (tc.isConstructor) {// Check whether the constructor is outgoing
      tc.args.map(_._2).exists({x =>
        val FunType(args, ret) = x
        args.exists(arg => types.exists(dependsOn(arg._2, _)))
      })
    }
  }
  
  /**
   * Generate no junk declarations for an inductive type declaration I
   * @param decls all declarations in I
   * @param parent the URI of I
   * @param context the parameters of I
   * 
   * for type level declarations c: {G} type
   *   induct_c: {M,G} c g -> c_M (g map induct)
   * for term level declaration c: {G} A
   *   induct_c: {M,G} induct(c g) = c_M (g map induct)
   * where
   *  * M declares one (primed) variable for every declaration in I
   *  * c_M is the variable in M corresponding to c
   *  * g is the list of varibles declared in G
   *  * induct(t) is the inductive translation of t, defined in terms of the induct function corresponding to the type of t,
   *    e.g., induct_a(m,t) if t has type a for some a: type in I
   *       or induct_a(m,h,t) if t has type (a h) for some a: {H}type in I
   */
  def noJunks(decls : List[InternalDeclaration], context: Context)(implicit parent : GlobalName): List[Constant] = {
    val (repls, modelContext) = chain(decls, context)
    val model = modelContext.map(_.toTerm)
    var inductNames : List[(GlobalName,GlobalName)] = Nil
    /*
     * applies the inductive translation needed in noJunks by constructing terms that apply the appropriate induction functions
     * @param tp the type of tm
     * @param tm the term to translate
     */
    def induct(tp: Term, tm: Term): Term = tp match {
      case Univ(1) => tm match {
        case ApplyGeneral(OMS(p), args) =>
          decls find {d => d.path == p} match {
            case None => tm
            case Some(d) =>
              val dArgs = d.args
              var tSub = Substitution()
              val inductArgs = (dArgs zip args) map {case ((nO,t), a) =>
                nO foreach {n => tSub = tSub ++ (OMV(n) / a)}
                val tS = t ^? tSub 
                induct(tS, a)
              }
              utils.listmap(inductNames, p) match {
                case Some(inductP) => ApplyGeneral(OMS(inductP), inductArgs)
                case None => throw ImplementationError("Couldn't find declaration for the inductively defined type "+noLookupPresenter.asString(tp)+". This should never happen. ")
              }
          }
        case _ => if (tp != Univ(1)) log("Found kind which is not the application of a type level: "+present(tp, true)+". "); tm
      }
      case ApplyGeneral(OMS(p), args) =>
        utils.listmap(inductNames, p) match {
          case Some(inductP) => ApplyGeneral(OMS(inductP), model:::args:::List(tm))
          case None => tm
        }
      case _ => tm
    }
    decls map {d =>
      val Ltp = () => {
        val (argCon, dApplied) = d.argContext(None)
        val dAppliedInduct = induct(d.ret, dApplied)
        val dPrimed = utils.listmap(repls, d.path).get
        val dApplPrimedOutCon = ApplyGeneral(dPrimed, d.context map {vd => induct(vd.tp.get, vd.toTerm)}) // To bring this term to the same form as the analogous one
        val dAppliedPrimed = ApplyGeneral(dApplPrimedOutCon, argCon map {vd => induct(vd.tp.get, vd.toTerm)})
        val retPrimed = induct(Univ(1), d.ret)
        val ret = d match {
          case tl: TermLevel => Eq(retPrimed, dAppliedInduct, dAppliedPrimed)
          case tl: StatementLevel => Eq(retPrimed, dAppliedInduct, dAppliedPrimed)
          case tl: TypeLevel => Arrow(dAppliedInduct, dAppliedPrimed)
        }
        Pi(context ++ modelContext ++ argCon, ret)
      }
      val name = inductName(d.name)
      val c = makeConst(name, Ltp)
      inductNames ::= d.path -> c.path
      c
    }
  }
    
    /**
   * Generate no confusion/injectivity declaration for term constructor d and all term constructors of the same return type
   * @param parent the derived declaration to elaborate
   * @param tmdecls all term level declarations
   */
  def noConf(a: TermLevel, tmdecls: List[TermLevel], types: List[GlobalName])(implicit parent: GlobalName): List[Constant] = {
    var decls: List[Constant] = Nil
    // To ensure that the result is well-typed
    if (!a.isConstructor) decls else {
      if (a.isSimplyTyped()) {
        // To ensure that the result is well-typed
        tmdecls.takeWhile(_ != a) filter (_.isSimplyTyped()) foreach {b => 
          val (matches, argMatches) = matchTypeParametersInReturnType(b, a)
          if (matches) {
            // TODO: Check this doesn't generate ill-typed declarations for dependently-typed constructors
            val newName = noConfName(a.name, b.name)
            val Ltp = () => {
              val (aCtx, aApplied) = a.argContext(None)
              val (bCtx, bApplied) = b.argContext(None)
              val filteredBctx = bCtx.filterNot(arg => aCtx contains utils.listmap(argMatches, arg))
              Pi(a.context++aCtx ++ filteredBctx, Neq(a.externalRet, aApplied, bApplied ^ argMatches.map(x=> x._1 / x._2)))  // This does not type-check if ret depends on arguments
            }
            decls ::= makeConst(newName, Ltp)
          }
        }
      }
      decls = decls.reverse
      if(a.args.length > 0)
        decls ++= a.injDecls()
      decls
    }
  }
  
  /**
   * Generate tester declarations for all constructors of an inductive type declaration I
   * @param tmdecls the termlevel declarations in I
   * @param tpdecls the typelevel declarations in I
   * @param decls all internal declarations in I
   * @param context the context of I
   */
  def testers(tmdecls : List[TermLevel], tpdecls: List[TypeLevel], decls: List[InternalDeclaration], context: Context)(implicit parent : GlobalName): List[Constant] = {
    //val types = tpdecls.map(_.path)InductiveTypes
    constrs(tmdecls) map {constr =>
      val Ltp = () => {
        val (argCon, _) = constr.argContext(None)
        val tpl = constr.getTpl
        val induct = ApplyGeneral(OMS(tpl.path.copy(name=inductName(tpl.name))), context.map(_.toTerm))
        val chain = decls map {
          case d: TypeLevel => 
            val (argConTpl, dAppliedTpl) = d.argContext(None)
            PiOrEmpty(context++argConTpl, Arrow(dAppliedTpl, Bool))
          case d: TermLevel if (d.isConstructor) => PiOrEmpty(context++d.argContext(None)._1, if (d == constr) True else False)
          case d => 
            val (argConD, appliedD) = d.argContext(None)
            PiOrEmpty(context++argConD, appliedD)
        }
        PiOrEmpty(context++argCon, ApplyGeneral(induct, chain++:constr.getTplArgs))
      }
      makeConst(testerName(constr.name), Ltp)
    }
  }
    
  /**
   * Generate unapply declarations for all constructors of an inductive type declaration I
   * @param constrdecls the constructors of I
   * @param tpdecls the typelevel declarations in I
   * @param decls all internal declarations in I
   * @param context the context of I
   */
  def unappliers(constrdecls : List[Constructor], tpdecls: List[TypeLevel], decls: List[InternalDeclaration], context: Context)(implicit parent : GlobalName): List[Constant] = {
    val types = tpdecls.map(_.path)
    constrdecls map {constr =>
      val Ltp = () => {
        val (argCon, _) = constr.argContext(None)
        val tpl = constr.getTpl
        val induct = ApplyGeneral(OMS(tpl.path.copy(name=inductName(tpl.name))), context.map(_.toTerm))
        val chain = decls map {
          case d: TypeLevel => 
            val (argConTpl, dAppliedTpl) = d.argContext(None)
            PiOrEmpty(context++argConTpl, Arrow(dAppliedTpl, OPTION(dAppliedTpl)))
          case d: TermLevel if (d.isConstructor) => 
            val (argConTml, dAppliedTml) = d.argContext(None)
            PiOrEmpty(context++argConTml, if (d == constr) SOME(d.ret, dAppliedTml) else NONE(d.ret))
          case d => 
            val (argConD, appliedD) = d.argContext(None)
            PiOrEmpty(context++argConD, appliedD)
        }
        PiOrEmpty(context++argCon, ApplyGeneral(induct, chain++:constr.getTplArgs))
      }
      makeConst(unapplierName(constr.name), Ltp)
    }
  }
  
  /**
   * Generate inductive proof declarations for an inductive type declaration I
   * @param tpdecls all Type-Level declarations in I
   * @param tmdecls all constructor declarations in I
   * @param context the parameters of I
   * @param parent the URI of I
   */
  def indProofs(tpdecls : List[TypeLevel], tmdecls : List[TermLevel], context: Context)(implicit parent : GlobalName): List[Constant] = {
    val (preds, predMap, _, ctx) = InductiveTypes.inductionHypotheses(tpdecls, tmdecls, context)
    
    preds zip tpdecls map {case (pred, tpl) =>
      val (argCon, dApplied) = tpl.argContext()
      val rb = InductiveTypes.rBar(argCon, tpdecls, predMap)
      val x = newVar("x", dApplied, Some(ctx++rb))
      val tp = PiOrEmpty(ctx++rb++x, ApplyGeneral(pred.toTerm, (rb++x).map(_.toTerm)))
      makeConst(proofName(tpl.name), ()=>{tp})
    }
  }
    
  /**
   * Check whether the return types match for some parametric type parameters 
   * and if so determine the necessary parameters that need to match
   * @param a the first termlevel whoose return type is to be matched against the one of the second
   * @param b the second termlevel whoose return type is to be matched against the return type of a
   * @note postcondition: If the types match for certain arguments, a list of tuples with arguments that need to match is returned
   * if those arguments passed to the termlevels are the same, the return types will both equal a.externalRet
   */
  def matchTypeParametersInReturnType(a: TermLevel, b: TermLevel): (Boolean, List[(OMV, OMV)]) = {
    val ApplyGeneral(f1, args1) = a.ret
    val ApplyGeneral(f2, args2) = b.ret
    if (f1 != f2 || args1.length != args2.length) (false, Nil) else {
      val notMatching = new Exception("arguments can't match")
      try {
        (true, args1.zip(args2).map {
          case (a1 : OMV, a2: OMV) if (a.args contains a1) => (a2, a1)
          case _ => throw notMatching
        })
      } catch {
        case e@notMatching => (false, Nil)
      }
    }
  }
}

object InductiveTypes {
  /** Elaborates an derived declaration D using the inductive feature. This is used to reuse the functionality of this feature in different features, speciafically the reflection feature.
   *  @param context The context of D
   *  @param parentTerm The path to D, used as prefix for the external declarations
   *  @param decls The internal declaration of the D
   */
  def elaborateDeclarations(context: Context, decls: List[InternalDeclaration], controller: Controller, logger: Option[Constant=> Unit] = None)(implicit parentTerm: GlobalName) : Elaboration = {
    val indTp:InductiveTypes = controller.extman.get(classOf[StructuralFeature], inductiveUtil.feature) match {
      case Some(indtp:InductiveTypes) => indtp
      case _ => throw ImplementationError("Structural feature "+inductiveUtil.feature+" not loaded, but required here. ")
    }
    indTp.elaborateDeclarations(context, decls, logger)
  }
  
  /**
   * Tries to apply the correct predicate to the term tm of type tp
   * @return the predicate applied to this term, if a corresponding predicate is found, none otherwise
   */
  def mapTerm(tm: Term, tp: Term, tpdecls: List[TypeLevel], predMap: List[(GlobalName, (Term, Term) => Term)], ctx: Context)(implicit parent: GlobalName) : Option[Term] = tp match {
    case OMS(p) => utils.listmap(predMap, p).map(_(tm, tp))
    case ApplyGeneral(OMS(p), args) if tpdecls map (_.externalPath) contains p => utils.listmap(predMap, p).map(_(tm, tp))
    case Pi(n, ntp, body) => 
      val arg = (OMV(n)%ntp)
      val argMapped = mapTerm(OMV(n), ntp, tpdecls, predMap, ctx++arg) map (rtp => newVar("p_"+n, rtp, Some(ctx++arg)))
      argMapped match {
        case None => None
        case Some(m) => 
          val argBar = Context(arg)++m
          mapTerm(Apply(tm, arg.toTerm), body, tpdecls, predMap, ctx++argBar) map {res =>
            Lambda(argBar, res)
          }
      }
    case Arrow(a, b) => 
      val aNamed = newVar("x", a, Some(ctx))
      mapTerm(tm, Pi(aNamed, b), tpdecls, predMap, ctx)
    case _ => None
  }
   
  
  //rBar:= (x => x zip applyPred x) flatMap  {(a,Some(b)) => a, b; (a,None) => a }
  def rBar(argCon: Context, tpdecls: List[TypeLevel], indProofDeclMap: List[(GlobalName, (Term, Term) => Term)])(implicit parent: GlobalName) : Context = {
    val rPrime = argCon map {x => (x, mapTerm(x.toTerm, x.tp.get, tpdecls, indProofDeclMap, argCon))}
    rPrime.flatMap {
      case (x, Some(y)) => List(x, newVar("r_"+x.name, y, Some(argCon)))
      case (x, None) => List(x)
    }
  }
  
  /**
   * Produces the proof predicate for each Type-Level and the corresponding induction hypothesis for each constructor
   * @param tpdecls the typelevels tpl_i declaring the mutually inductively defined types (tp_i) over which to apply induction
   * @param tmdecls the termlevels leading to the inductive cases
   * @param context the outer context of the declarations
   * @return A pair consisting of
   *         1) the proof predicate (claim) pred_i for each type tp_i,
   *         2) a list of pairs (tpp_i, f_) for tpp_i the path of tpl_i,
   *         where f maps (tm, tp) |-> pred_i tm if the return type of tp is tp_i, else (tm, tp) |-> tm
   *         3) a list of pairs (intdecl, claim) for each tpl or constructor intdecl,
   *         where claim is a variable for the inductive claim for intdecl
   *         4) a context with all local names used in the function,
   *         it is used to prevent name clashes in this recursive function
   */
  def inductionHypotheses(tpdecls : List[TypeLevel], tmdecls : List[TermLevel], context: Context)(implicit parent : GlobalName) = {
    var ctx = context
    //
    var indProofDeclMap : List[(GlobalName, (Term, Term) => Term)] = Nil
    var predsMap : List[(InternalDeclaration, VarDecl)] = Nil
    
    //a context of the predicates for each inductively-defined type
    //For a tpl: Pi r. type this returns a predicate of type Pi rBar(x).tpl(x) -> pred, where
    //rBar(x):= flatMap (x => x zip applyPred x)
    //x=r
    val preds = tpdecls map {tpl =>
      val (argCon, dApplied) = tpl.argContext()
      val tp = PiOrEmpty(rBar(argCon, tpdecls, indProofDeclMap), Arrow(dApplied, Prop))
      val pred = newVar(proofPredName(tpl.name).toString(), tp, Some(ctx++argCon))

      val map : (GlobalName, (Term, Term) => Term) = (tpl.externalPath, {
        (tm, tp) => tp match {
          case OMS(p) if p == tpl.externalPath => Apply(pred.toTerm, tm)
          case ApplyGeneral(OMS(p), args) if p == tpl.externalPath => ApplyGeneral(pred.toTerm, args.+:(tm))
          case _ => throw ImplementationError(tm.toString())
        }
      })
       
      indProofDeclMap ::= map
      predsMap ::= (tpl, pred)
      ctx++=pred
      pred
    }
    
    //The required assumptions for the constructors
    val inductCases = constrs(tmdecls) map {constr =>
      val (argCon, dApplied) = constr.argContext()
      val pred = utils.listmap(predsMap, constr.getTpl).get
      val claim = ApplyGeneral(pred.toTerm, constr.getTplArgs.+:(dApplied))
      val assumptions = rBar(argCon, tpdecls, indProofDeclMap)
      val inductCase = newVar(proofPredName(constr.name), PiOrEmpty(assumptions, claim), ctx++argCon)
      predsMap ::= (constr, (inductCase))
      ctx++=inductCase; inductCase
    }
    (preds, indProofDeclMap, predsMap, ctx)
  }
}

object InductiveRule extends StructuralFeatureRule(classOf[InductiveTypes], "inductive")
