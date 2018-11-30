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

/** theories as a set of types of expressions */ 
class InductiveTypes extends StructuralFeature("inductive") with ParametricTheoryLike {
  object noLookupPresenter extends presentation.NotationBasedPresenter {
    override def getNotations(p: GlobalName) = if (! (p.doc.uri.path contains "urtheories")) Nil else super.getNotations(p)
    override def getAlias(p: GlobalName) = if (true) Nil else super.getAlias(p)
  }
  
  override def start(args: List[String]) {
    initOther(noLookupPresenter)
  }
  
  def defaultPresenter(c: Constant)(implicit con: Controller): String = c.name + ": " + noLookupPresenter.asString(c.tp.get) + (if (c.df != None) " = "+noLookupPresenter.asString(c.df.get) else "")

  /**
   * Checks the validity of the inductive type(s) to be constructed
   * @param dd the derived declaration from which the inductive type(s) are to be constructed
   */
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {}
  
  /** Check whether the TermLevel is a constructor or outgoing */
  def constructor(tc: TermLevel, types: List[GlobalName]): Boolean = tc.ret match {
    case ApplyGeneral(OMS(tpl), args) => types contains tpl
    case _ => false
  }
  
  /** Check whether the TermLevel has a higher order argument of an inductively defined type
   *  In that case an error is thrown
   */
  def checkTermLevel(tc: TermLevel, types: List[GlobalName])(implicit parent : GlobalName) = {
    def dependsOn(tm: Term, tp: GlobalName): Boolean = {
      val FunType(args, ret) = tm
      args exists {arg => val ApplyGeneral(tpConstr, tpArgs) = arg._2; tpConstr == OMS(tp)}  // TODO: Is this the right condition to check for?
    }
    if (constructor(tc, types)) {// Check whether the constructor is outgoing
      tc.args.map(_._2).exists({x =>
        val FunType(args, ret) = x
        args.exists(arg => types.exists(dependsOn(arg._2, _)))
      })
    }
  }
  
  /**
   * Elaborates an declaration of one or multiple mutual inductive types into their declaration, 
   * as well as the corresponding no confusion and no junk axioms
   * Constructs a structure whose models are exactly the (not necessarily initial) models of the declared inductive types
   * @param parent The parent module of the declared inductive types
   * @param dd the derived declaration to be elaborated
   */
  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) = {
    val context = Type.getParameters(dd) 
    implicit val parentTerm = dd.path
    // to hold the result
    var elabDecls : List[Constant] = Nil
    implicit var tmdecls : List[TermLevel]= Nil
    implicit var statdecls : List[StatementLevel]= Nil
    implicit var tpdecls : List[TypeLevel]= Nil
    val decls = dd.getDeclarations map {
      case c: Constant =>
        val intDecl = InternalDeclaration.fromConstant(c, controller, Some(context))
        intDecl match {
          case d @ TermLevel(_, _, _, _, _,_) => tmdecls :+= d; intDecl 
          case d @ TypeLevel(_, _, _, _,_) => tpdecls :+= d; intDecl
          case d @ StatementLevel(_, _, _, _,_) => statdecls :+= d; intDecl 
         }
      case _ => throw LocalError("unsupported declaration")
    }

    // copy all the declarations
    decls foreach {d => elabDecls ::= d.toConstant}
    tmdecls foreach { tmdecl => checkTermLevel(tmdecl, tpdecls map (_.path))}
        
    // the no confusion axioms for the data constructors
    /*
     * For dependently-typed constructors, we cannot elaborate into plain LF:
     * some of the (in)equality axioms would be ill-typed because
     * the type system already forces elements of different instances of a dependent type to be unequal and of unequal type
     */
    elabDecls = elabDecls.reverse ::: tmdecls.flatMap(x => noConf(x, tmdecls)(dd.path))
    
    // the no junk axioms
    elabDecls ++= noJunks(decls, context)(dd.path)
    
    //elabDecls foreach {d =>log(defaultPresenter(d)(controller))}    //This typically prints all external declarations several times
    new Elaboration {
      def domain = elabDecls map {d => d.name}
      def getO(n: LocalName) = {
        elabDecls.find(_.name == n) foreach(c => log(defaultPresenter(c)(controller)))
        elabDecls.find(_.name == n)
      }
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
        case _ => log("Found kind which is not the application of a type level: "+present(tm, true)+". "); tp
      }
      case ApplyGeneral(OMS(p), args) =>
        utils.listmap(inductNames, p) match {
          case Some(inductP) => ApplyGeneral(OMS(inductP), model:::args:::List(tm))
          case None => tm
        }
      case _ => log("Found term of non inductively-defined type "+noLookupPresenter.asString(tp)+". "); tm
    }
    decls map {d =>
      val Ltp = () => {
        val (argCon, dApplied) = d.argContext(None)
        val dAppliedInduct = induct(d.ret, dApplied)
        val dPrimed = utils.listmap(repls, d.path).get
        val dAppliedPrimed = ApplyGeneral(dPrimed, argCon map {vd => induct(vd.tp.get, vd.toTerm)})
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
   * name of the declaration corresponding to n declared in noJunks
   */
  private def inductName(n: LocalName) = LocalName("induct_" + n.toPath)
  
    /**
   * Generate no confusion/injectivity declaration for term constructor d and all term constructors of the same return type
   * @param parent the derived declaration to elaborate
   * @param tmdecls all term level declarations
   */
  def noConf(d: TermLevel, tmdecls: List[TermLevel])(implicit parent: GlobalName): List[Constant] = {
    var decls: List[Constant] = Nil
    //println("\n\nProducing the no-confusion declarations for "+d.name+": ")
    // To ensure that the result is well-typed
    if (d.isNotDependentTyped()) {
      // To ensure that the result is well-typed
      tmdecls.takeWhile(_ != d) filter (_.isNotDependentTyped()) foreach {b => 
        if (b.tp == d.tp) {
          // TODO: Check this doesn't generate ill-typed declarations for dependently-typed constructors
          val newName = uniqueLN("no_conf_" + d.name.last+ "_" + b.name.last)
          val Ltp = () => {
            val (aCtx, aApplied) = d.argContext(None)
            val (bCtx, bApplied) = b.argContext(None)
            Pi(d.context++aCtx ++ bCtx, Neq(b.tp, aApplied, bApplied))  // This does not type-check if ret depends on arguments
          }
          decls ::= makeConst(newName, Ltp)
        }
      }
    }
    decls = decls.reverse
    if(d.args.length > 0)
      //println("Producing the injectivity axiom for "+d.name+": ")
      decls ++= d.injDecls
    decls
  }
}

object InductiveRule extends StructuralFeatureRule(classOf[InductiveTypes], "inductive")