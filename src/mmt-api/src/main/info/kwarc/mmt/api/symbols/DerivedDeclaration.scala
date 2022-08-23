package info.kwarc.mmt.api.symbols

import info.kwarc.mmt.api._
import modules._
import frontend._
import checking._
import uom.{ElaboratedElement, ExtendedSimplificationEnvironment, SimplificationEnvironment}
import objects._
import notations._

import scala.xml.Elem
import Theory._
import info.kwarc.mmt.api.modules.diagrams.InstallDiagram
import info.kwarc.mmt.api.utils.MMT_TODO


/** A [[DerivedContentElement]] unifies feature of [[Constant]] and [[Theory]] but without a commitment to the semantics.
 *  The semantics is defined by the corresponding [[StructuralFeature]]
 */
trait DerivedContentElement extends AbstractTheory with HasType with HasNotation {
  override def getComponents = TypeComponent(tpC) :: DefComponent(dfC) :: notC.getComponents

  /** always empty for now, could be relaxed in the future */
  def parameters = Context.empty

  def outerString: String = {
    val s = {
      name match {
        case LocalName(ComplexStep(p) :: Nil) => ""
        case _ => " " + name
      }
    }
    feature + s + tpC.get.map(" : " + _.toString).getOrElse("")
  }
 
  override def toNode : Elem = {
    <derived feature={feature} name={name.toPath} base={parent.toPath}>
      {getMetaDataNode}
      {tpNode}
      {dfNode}
      {notNode}
      {getDeclarations.filter(d => !d.isGenerated) map (_.toNode)}
    </derived>
  }
  // override def toNodeElab
  override def toNode(rh: presentation.RenderingHandler): Unit = {
    rh << s"""<derived feature="$feature" name="${name.toPath}" base="${parent.toPath}">"""
    (getMetaDataNode++tpNode++dfNode++notNode) foreach {n =>
      rh(n)
    }
    getDeclarations foreach(d => if (!d.isGenerated) d.toNode(rh))
    rh << "</derived>"
  }
}

/** a module that is elaborated into more primitive module */
class DerivedModule(val feature: String, p: DPath, n: LocalName, val meta: Option[MPath], val tpC: TermContainer, val dfC : TermContainer, val notC: NotationContainer)
   extends Module(p,n) with DerivedContentElement {
   def translate(newNS: DPath, newName: LocalName, tl: Translator, con: Context): DerivedModule = {
     new DerivedModule(feature, newNS, newName, meta, translateTp(tl, con), translateDf(tl, con), notC.copy())
   }
}


/** a declarations that is elaborated into more primitive declarations */
class DerivedDeclaration(val home: Term, val name: LocalName, val feature: String, val tpC: TermContainer,
                         val notC: NotationContainer, val dfC : TermContainer = TermContainer(None)) extends Declaration with DerivedContentElement {
  type ThisType = DerivedDeclaration
  
  def meta = None
  
  @MMT_TODO("redundant: every DerivedDeclaration is module-like now")
  val module = this // left over from old definition via NestedModule

  override def translate(newHome: Term, prefix: LocalName, tl: Translator, con : Context) = {
     val tpT = translateTp(tl, con)
     val dfT = translateDf(tl,con)
     val res = new DerivedDeclaration(newHome, prefix/name, feature, tpT, notC.copy(), dfT)
     val icont = con ++ getInnerContext
     getDeclarations.foreach {d =>
       val dTranslated = d.translate(res.toTerm, LocalName.empty, tl, icont)
       if (this.feature == patterns.Instance.feature) d match {
         case c: Constant if (c.rl == Some("mainDecl")) => res.add(Constant(c.home, c.name, c.alias, c.tp, c.df, None, this.notC))
       } else
         res.add(dTranslated)
     }
     res
   }
  
   // see also NestedModule
   def merge(that: Declaration) = mergeError(that)
}


/**
 * a rule that legitimizes a [[StructuralFeature]]
 */
case class StructuralFeatureRule(cls: Class[_ <: StructuralFeature], feature: String) extends Rule {
  override def toString = "rule for feature " + feature
}


/**
 * A StructureFeature defines the semantics of a [[DerivedDeclaration]]
 *
 * The semantics consists of a set of declarations that are injected into the parent theory after the [[DerivedDeclaration]]
 * These are called the 'outer declarations'.
 *
 * All methods that take a dd:DerivedDeclaration can assume
 * - dd.feature == this.feature
 * - dd.getComponents has the same components as this.expectedComponents and in the same order
 */
abstract class GeneralStructuralFeature[Level <: DerivedContentElement](val feature: String) extends FormatBasedExtension {
   final def isApplicable(s: String) = s == feature

   val bodyDelim = "="

   lazy val mpath = SemanticObject.javaToMMT(getClass.getCanonicalName)

   /** the notation for the header */
   def getHeaderNotation: List[Marker]

   /** the parse rule for the header */
   def getHeaderRule = parser.ParsingRule(mpath, Nil, TextNotation(Mixfix(getHeaderNotation), Precedence.integer(0), None, false))

   /** parses the header term of a derived declaration into its name and type
    *  by default it is interpreted as OMA(mpath, name :: args) where OMA(mpath, args) is the type
    */
   def processHeader(header: Term): (LocalName,Term) = {
     header match {
       case OMA(OMMOD(`mpath`), OML(name, None, None,_,_)::args) =>
         val tp = OMA(OMMOD(mpath), args)
         (name, tp)
     }
   }

   /** inverse of processHeader */
   def makeHeader(dd: Level): Term = {
     dd.tpC.get match {
       case Some(OMA(OMMOD(`mpath`), args)) => OMA(OMMOD(mpath), OML(dd.name, None, None) :: args)
     }
   }

   /**
    * the term components that declarations of this feature must provide and strings for parsing/presenting them
    *
    * also defines the order of the components
    */
   def expectedComponents: List[(String,ObjComponentKey)] = Nil

   /** compute the expected type of a constant inside a derived element of this feature
    *  none by default, override as needed
    */
   def expectedType(dd: Level, c: Constant): Option[Term] = None

  /** called after checking components and inner declarations for additional feature-specific checks */
   def check(dd: Level)(implicit env: ExtendedCheckingEnvironment): Unit

  /**
    * Computes the modules into which the feature elaborates.
    *
    * Override as needed.
    *
    * @param dd The derived content element, e.g. for [[ModuleLevelFeature module-level structural features]] a [[DerivedModule]].
    */
   def modules(dd: Level, rules: Option[RuleSet], env: SimplificationEnvironment): List[Module] = Nil
}

/**
  * A declaration-level structural feature: a structural feature that can occur within modules (often theories)
  * and elaborotes into multiple so-called derived ("generated") declarations.
  *
  * @param feature The feature string uniquely identifying the structural feature at hand
  */
abstract class StructuralFeature(feature: String) extends GeneralStructuralFeature[DerivedDeclaration](feature) {
  /** additional context relative to which to interpret the body of a derived declaration */
  def getInnerContext(dd: DerivedDeclaration): Context = dd.getInnerContext

   /**
    * defines the outer perspective of a derived declaration
    *
    * @param parent the containing module
    * @param dd the derived declaration
    */
   def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration)(implicit env: Option[ExtendedSimplificationEnvironment] = None): Elaboration

   def elaborateInContext(prev: Context, dv: VarDecl): Context = prev
   def checkInContext(prev: Context, dv: VarDecl): Unit = {}

   /** for creating/matching variable declarations of this feature */
   object VarDeclFeature extends DerivedVarDeclFeature(feature)
   /** returns the rule constant for using this feature in a theory */
   def getRule = StructuralFeatureRule(getClass, feature)

  /**
    * Computes no modules: declaration-level structural features do not elaborate into modules.
    *
    * @see [[elaborate()]]
    */
   final override def modules(dd: DerivedDeclaration, rules: Option[RuleSet], env: SimplificationEnvironment): List[Module] = Nil
}

/**
  * A module-level structural feature: a structural feature that can occur in documents and elaborates
  * into modules.
  *
  * A classic example is the [[InstallDiagram]] structural feature installing the result of diagram operators
  * applied on diagrams.
  *
  * @param feature A feature string, see docs of [[GeneralStructuralFeature]].
  */
abstract class ModuleLevelFeature(feature: String) extends GeneralStructuralFeature[DerivedModule](feature)

/**
 * the return type of elaborating a [[DerivedDeclaration]] by a [[StructuralFeature]]
 */
abstract class Elaboration extends ElementContainer[Declaration] {
    def domain: List[LocalName]
    /**
     * default implementation in terms of the other methods
     * may be overridden for efficiency
     */
    def getDeclarations = {
      domain.map {n => getO(n).getOrElse {throw ImplementationError(n.toString + " is said to occur in domain of elaboration but retrieval failed")}}
    }
    def getMostSpecific(name: LocalName): Option[(Declaration,LocalName)] = {
      domain.reverse.foreach {n =>
         name.dropPrefix(n) foreach {suffix =>
           return getO(n) map {d => (d,suffix)}
         }
      }
      return None
    }
}

object Elaboration {
  /** constructs a non-lazy Elaboration */
  def apply(ds: List[Declaration]) = {
    new Elaboration {
      val elabDecls = ds
      def domain = getDeclarations.map(_.name)
      def getO(name: LocalName) = getDeclarations.view.reverse.find(_.name == name)
    }
  } 
}

/** for structural features with unnamed declarations whose type is an instance of a named theory */
trait IncludeLike {self: StructuralFeature =>
  private def error = {
    throw LocalError("no domain path found")
  }

  def getHeaderNotation = List(SimpArg(1))

  override def processHeader(header: Term) = header match {
    case OMA(OMMOD(`mpath`), (t @ OMPMOD(p,_))::Nil) => (LocalName(p), t)
  }

  override def makeHeader(dd: DerivedDeclaration) = OMA(OMMOD(`mpath`), dd.tpC.get.get :: Nil)

  /** the type (as a theory) */
  def getDomain(dd: DerivedDeclaration): Term = dd.tpC.get.get
}

/** for structural features that are parametric theories with special meaning, e.g., patterns, inductive types */
trait ParametricTheoryLike extends StructuralFeature {
   val Type = ParametricTheoryLike.Type(getClass)

   def getHeaderNotation = List(LabelArg(2, LabelInfo.none), Delim("("), Var(1, true, Some(Delim(","))), Delim(")"))

   override def getInnerContext(dd: DerivedDeclaration) = {
     val parameters = Type.getParameters(dd)
     parameters ++ Context(dd.modulePath)
   }

   override def processHeader(header: Term) = header match {
     case OMBIND(OMMOD(`mpath`), cont, OML(name,None,None,_,_)) => (name, Type(cont))
     case OMA(OMMOD(`mpath`), List(OML(name,None,None,_,_))) => (name, Type(Context.empty))
     case _ => throw InvalidObject(header, "ill-formed header")
   }
   override def makeHeader(dd: DerivedDeclaration) = dd.tpC.get match {
     case Some(Type(cont)) => OMBIND(OMMOD(mpath), cont, OML(dd.name, None,None))
   }

   def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = {
     //TODO check IsContext here
   }
   
  object noLookupPresenter extends presentation.NotationBasedPresenter {
    override def getNotations(p: GlobalName) = if (! (p.doc.uri.path contains "urtheories")) Nil else super.getNotations(p)
    override def getAlias(p: GlobalName) = if (true) Nil else super.getAlias(p)
  }
  
  override def start(args: List[String]): Unit = {
    initOther(noLookupPresenter)
  }
  
  def defaultPresenter(c: Constant)(implicit con: Controller): String = c.name.toString + ": " + noLookupPresenter.asString(c.tp.get) + (if (c.df != None) " = "+noLookupPresenter.asString(c.df.get) else "")
}

/** helper object */
object ParametricTheoryLike {
   /** official apply/unapply methods for the type of a ParametricTheoryLike derived declaration */
   case class Type(cls: Class[_ <: ParametricTheoryLike]) {
     val mpath = SemanticObject.javaToMMT(cls.getCanonicalName)

     def apply(params: Context) = OMBINDC(OMMOD(mpath), params, Nil)
     def unapply(t: Term) = t match {
       case OMBINDC(OMMOD(this.mpath), params, _) => Some((params))
       case _ => None
     }

     /** retrieves the parameters */
     def getParameters(dd: DerivedDeclaration) = {
       dd.tpC.get.flatMap(unapply).getOrElse(Context.empty)
     }
   }
}

trait Untyped {self : StructuralFeature =>
  def getHeaderNotation: List[Marker] = List(LabelArg(1,LabelInfo.none))
  override def processHeader(header: Term) = header match {
    case OMA(OMMOD(`mpath`), List(OML(name,_,_,_,_))) => (LocalName(name),OMMOD(mpath))
    // Type is completely useless here, but for some reason it nees to return SOME term...
  }
  override def makeHeader(dd: DerivedDeclaration) = OMA(OMMOD(mpath), List(OML(dd.name,None,None)))
  def elaborate(parent: Module, dd: DerivedDeclaration)(implicit env: Option[ExtendedSimplificationEnvironment] = None): Elaboration = {
    new Elaboration {
      def domain: List[LocalName] = dd.getDeclarations.map(_.name)
      def getO(nm: LocalName): Option[Declaration] = dd.getDeclarations.find(_.name == nm).map {
        case c: Constant => Constant(parent.toTerm,c.name,c.alias,c.tp,c.df,Some(self.feature.toString),c.notC)
      }
    }
  }
  def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = {}
}

trait UnnamedUntyped {self : StructuralFeature =>
  def getHeaderNotation: List[Marker] = Nil
  override def processHeader(header: Term) = (LocalName(self.feature),OMMOD(mpath))
    // Type is completely useless here, but for some reason it nees to return SOME term...
  override def makeHeader(dd: DerivedDeclaration) = OMMOD(mpath)

  def elaborate(parent: Module, dd: DerivedDeclaration)(implicit env: Option[ExtendedSimplificationEnvironment] = None): Elaboration = {
    new Elaboration {
      def domain: List[LocalName] = dd.getDeclarations.map(_.name)
      def getO(nm: LocalName): Option[Declaration] = dd.getDeclarations.find(_.name == nm).map {
        case c: Constant => Constant(parent.toTerm,c.name,c.alias,c.tp,c.df,Some(self.feature.toString),c.notC)
      }
    }
  }
  def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = {}
}

trait TypedConstantLike {self: StructuralFeature =>
  def getHeaderNotation: List[Marker] = List(LabelArg(1,LabelInfo.none),Delim(":"),SimpArg(2))
  override def processHeader(header: Term) = header match {
    case OMA(OMMOD(`mpath`), List(OML(name,_,_,_,_),t)) => (LocalName(name),t)// (name, Type(cont))
    case _ => throw ImplementationError("unexpected header")
  }
  override def makeHeader(dd: DerivedDeclaration) = dd.tpC.get match {
    case Some(t) => OMA(OMMOD(mpath), List(OML(dd.name,None,None),t))
    case None => throw ImplementationError("no type present")
  }
  def getType(dd: DerivedDeclaration): Term = dd.tpC.get.get
  def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = {
    // TODO env.objectChecker(CheckingUnit())
  }
}

trait TheoryLike extends StructuralFeature {
  object Type {
    val mpath = SemanticObject.javaToMMT(getClass.getCanonicalName)

    def apply(params: Context) = OMBINDC(OMMOD(mpath), params, Nil)
    def unapply(t: Term) = t match {
      case OMBINDC(OMMOD(this.mpath), params, Nil) => Some((params))
      case _ => None
    }

    /** retrieves the parameters */
    def getParameters(dd: DerivedDeclaration) = {
      dd.tpC.get.flatMap(unapply).getOrElse(Context.empty)
    }
  }
  def getHeaderNotation: List[Marker] = List(LabelArg(1,LabelInfo.none))
  override def processHeader(header: Term) = header match {
    case OMBIND(OMMOD(`mpath`), cont, OML(name,None,None,_,_)) => (name, Type(cont))
    case OMA(OMMOD(`mpath`), List(OML(name,None,None,_,_))) => (name, Type(Context.empty))
    case _ => throw InvalidObject(header, "ill-formed header")
  }
  override def makeHeader(dd: DerivedDeclaration) = dd.tpC.get match {
    case Some(Type(cont)) => OMBIND(OMMOD(mpath), cont, OML(dd.name, None,None))
  }
  def getType(dd: DerivedDeclaration): Term = dd.tpC.get.get
  def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = {
    // TODO env.objectChecker(CheckingUnit())
  }
}

/** for structural features that take both parameters and a type
 *  Examples are structural features which build structures defined via a derived declaration of another structural feature
 *  like inductively-defined functions or proofs by induction over an inductively-defined type or terms of a record
 *  In such a case the type is the other derived declaration instantiated with values for its parameters
 */
trait TypedParametricTheoryLike extends StructuralFeature with ParametricTheoryLike {
  val ParamType = TypedParametricTheoryLike.ParamType(getClass)
  override val Type = ParametricTheoryLike.Type(getClass)

  override def getHeaderNotation = List(LabelArg(2, LabelInfo.none), Delim("("), Var(1, true, Some(Delim(","))), Delim(")"), Delim(":"),
      SimpArg(3), Delim("("), SimpSeqArg(4, Delim(","), CommonMarkerProperties.noProps), Delim(")"))

  override def getInnerContext(dd: DerivedDeclaration) = {
    val params = ParamType.getParameters(dd)
    params ++ Context(dd.modulePath)
  }

  override def processHeader(header: Term) = header match {
    case OMA(OMMOD(`mpath`), List(OML(name,_,_,_,_),t)) => 
      val p = getHeadPath(t)
      (name, ParamType(p, Context.empty, Nil))
    case OMA(OMMOD(`mpath`), OML(name,_,_,_,_)::t::args) => 
      val p = getHeadPath(t)
      (name, ParamType(p, Context.empty, args))
    case OMBINDC(OMMOD(`mpath`), cont, List(OML(name,_, _,_,_), t)) => 
      val p = getHeadPath(t)
      (name, ParamType(p, cont, Nil))
    case OMBINDC(OMMOD(`mpath`), cont, OML(name,_, _,_,_)::t::args) =>
      val p = getHeadPath(t)
      (name, ParamType(p, cont, args))
    case hdr => throw InvalidObject(header, "ill-formed header")
  }
  override def makeHeader(dd: DerivedDeclaration) = dd.tpC.get match {
    case Some(ParamType(p, cont, args)) => OMBINDC(OMMOD(mpath), cont, OML(dd.name, None,None)::OMS(p)::args)
  }
  def getHeadPath(t: Term) : GlobalName = t match {
    case OMS(p) => p
    case OMMOD(p) => p.toGlobalName
    case _ => throw InvalidObject(t, "ill-formed header")
  }
  
  def parseTypedDerivedDeclaration(dd: DerivedDeclaration, expectedFeature: Option[List[String]]=None) : (Context, List[Term], DerivedDeclaration, Context) = {
    val (indDefPath, context, indParams) = ParamType.getParams(dd)
    val indD = controller.library.get(indDefPath) match {
    case indD: DerivedDeclaration if (expectedFeature.isEmpty || expectedFeature.get.contains(indD.feature)) => indD
    case d: DerivedDeclaration => throw LocalError("the referenced derived declaration is not among the features "+expectedFeature.get+" but of the feature "+d.feature+".")
    case _ => throw LocalError("Expected definition of corresponding inductively-defined types at "+indDefPath.toString()
          +" but no derived declaration found at that location.")
    }
    
    val indCtx = controller.extman.get(classOf[StructuralFeature], indD.feature).getOrElse(throw LocalError("Structural feature "+indD.feature+" not found.")) match {
      case f: ParametricTheoryLike => f.Type.getParameters(indD)
      case _ => Context.empty
    }
    (context, indParams, indD, indCtx)
  }
  
  def checkParams(indCtx: Context, indParams: List[Term], context: Context, env: ExtendedCheckingEnvironment) : Unit = {
		//check the indParams match the indCtx at least in length
    if (indCtx .length != indParams.length) {
      throw LocalError("Incorrect length of parameters for the referenced derived declaration .\n"+
          "Expected "+indCtx.length+" parameters but found "+indParams.length+".")}

    //check whether their types also match
    indCtx zip indParams map {case (vd, tm) =>
      vd.tp map {expectedType =>
        val tpJudgement = Typing(Stack.empty, tm, expectedType)
        val typeComponent = tm.governingPath map (CPath(_, DefComponent))
        val cu = CheckingUnit.apply(typeComponent, context, Context.empty, tpJudgement)
        val (objChecker, rules) = (env.objectChecker, env.rules)
        objChecker(cu, rules)(env.ce)
      }
    }
  }
}

/** helper object */
object TypedParametricTheoryLike {
  /** official apply/unapply methods for the type of a TypedParametricTheoryLike derived declaration */
   case class ParamType(cls: Class[_ <: TypedParametricTheoryLike]) {
     val mpath = SemanticObject.javaToMMT(cls.getCanonicalName)

     def apply(p: GlobalName, params: Context, args: List[Term]) = OMBINDC(OMMOD(mpath), params, OMS(p)::args)
     def unapply(t: Term) = t match {
       case OMBINDC(OMMOD(this.mpath), params, OMS(p)::args) => Some(p, params, args)
       case _ => None
     }

     /** retrieves the parameters and arguments */
     def getParams(dd: DerivedDeclaration): (GlobalName, Context, List[Term]) = {
       dd.tpC.get.get match {
         case OMBINDC(OMMOD(_), pars, OMS(p)::args) => (p, pars, args)
       }
     }
     /** retrieves the parameters */
     def getParameters(dd: DerivedDeclaration) = {getParams(dd)._2}
   }
}

trait ReferenceLikeTypedParametricTheoryLike extends StructuralFeature with TypedParametricTheoryLike {
  // override val ParamType = TypedParametricTheoryLike.ParamType(getClass)
  // override val Type = ParametricTheoryLike.Type(getClass)

  /**
   * parse the derived declaration into its components
   * @param dd the derived declaration
   * @return returns a pair containing the mpath of the derived declaration, the declarations defined in the referenced theory,
   *         the argument context of this derived declaration, the arguments provided to the referenced theory and the outer context
   */
  def getDecls(dd: DerivedDeclaration): (GlobalName, List[Declaration], Context, List[Term], Context) = {
    val (indDefPathGN, context, indParams) = ParamType.getParams(dd)
    val (indCtx, decs) : (Context, List[StructuralElement])= controller.getO(indDefPathGN) match {
      case Some(str) => str match {
        case t: Theory => (t.parameters, t.getDeclarations)
        case t: StructuralElement if (t.feature =="theory") =>
          val decs = t.getDeclarations
          //We shouldn't have to do something this ugly, for this match to work out
          //TODO: There should be a better method provided by the api
          val params = t.headerInfo match {
            case HeaderInfo("theory", _, List(_, params: Context)) => params
            case _ => Context.empty
          }
          (params, decs)
        case m : ModuleOrLink => (Context.empty, m.getDeclarations)
        case t => throw GeneralError("reflection over unsupported target (expected a theory)"+t.path+" of feature "+t.feature + ": "+t)
      }
      case None => throw GeneralError("target of reflection not found at "+indDefPathGN)
    }
    (indDefPathGN, decs.map({d => d match {case d: Declaration => d case _ => throw LocalError("unsupported structural element at "+d.path)}}), indCtx, indParams, context)
  }
}

/**
 * Generative, definitional functors/pushouts with free instantiation
 * called structures in original MMT
 */
class GenerativePushout extends StructuralFeature("generative") with IncludeLike {

  def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration)(implicit env: Option[ExtendedSimplificationEnvironment] = None) = {
      val parentThy = parent match {
        case thy: Theory => thy
        case _ => throw LocalError("generative pushout must occur in theory")
      }
      val dom = getDomain(dd)
      val context = parent.getInnerContext
      val body = controller.simplifier.materialize(context, dom, None, None)
      new Elaboration {
        /** the morphism dd.dom -> parent of the pushout diagram: it maps every n to dd.name/n */
        private val morphism = View(parentThy.parent, parent.name/dd.name, dom, parent.toTerm, false)
        /** precompute domain and build the morphism */
        val domain = body.getDeclarationsElaborated.map {d =>
          val ddN = dd.name / d.name
          val assig = Constant(morphism.toTerm, d.name, Nil, None, Some(OMS(parentThy.path ? ddN)), None)
          morphism.add(assig)
          ddN
        }
        // translate each declaration and merge the assignment (if any) into it
        private val translator = ApplyMorphism(controller.globalLookup, morphism.toTerm)
        def getO(name: LocalName): Option[Declaration] =
          if (name.steps.startsWith(dd.name.steps)) {
            val rest = name.drop(dd.name.steps.length)
            body.getO(rest) map {
              case d: Declaration =>
                val dT = d.translate(parent.toTerm, dd.name, translator,Context.empty)
                val dTM = dd.getO(rest) match {
                  case None => dT
                  case Some(a) => dT merge a
                }
                dTM
              case ne => throw LocalError("unexpected declaration in body of domain: " + ne.name)
            }
          } else None
      }
   }

   def check(d: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = {}
}

// Binds theory parameters using Lambda/Pi in an include-like structure
class BoundTheoryParameters(id : String, pi : GlobalName, lambda : GlobalName, applys : GlobalName) extends StructuralFeature(id) with IncludeLike {
  override def checkInContext(prev : Context, dv: VarDecl): Unit = dv match {
    case VarDeclFeature(LocalName(ComplexStep(p) :: Nil), OMMOD(q), None) if p == q => checkpath(p)
    case _ =>
  }


  private def bindPi(t : Term)(implicit vars : Context) = if (vars.nonEmpty) OMBIND(OMS(pi),vars,t) else t
  private def bindLambda(t : Term)(implicit vars : Context) = if (vars.nonEmpty) OMBIND(OMS(lambda),vars,t) else t

  private def applyParams(body : Theory,toTerm : LocalName => Term)(vars : Context) = new StatelessTraverser {
    val varsNames = vars.map(_.name)
    def traverse(t: Term)(implicit con: Context, state: State) : Term = t match {
      case OMS(p) if p.module == body.path && p.name.steps.head.isInstanceOf[SimpleStep] => //dd.module.path =>
        vars.foldLeft[Term](toTerm(LocalName(body.path) / p.name))((tm,v) => OMA(OMS(applys),List(tm,OMV(v.name))))
      case OMS(p) if p.module == body.path =>
        toTerm(p.name)
      case OMV(ln @ LocalName(ComplexStep(mp) :: rest)) =>
        toTerm(ln)
      case OMBINDC(bind, bvars, scps) =>
        // rename all bound variables that are among the parameters to avoid capture
        val (bvarsR, bvarsSub) = Context.makeFresh(bvars, varsNames ::: con.domain)
        OMBINDC(traverse(bind), traverseContext(bvarsR), scps map {s => traverse(s ^? bvarsSub)(con++bvarsR, state)})
      case _ => Traverser(this,t)
    }
  }
  private def mkTranslator(body : Theory, toTerm : LocalName => Term)(implicit vars : Context) = new Translator {
    val applyPars = applyParams(body,toTerm)(vars)
    def applyType(c: Context, t: Term) = bindPi(applyPars(t, c))
    def applyDef(c: Context, t: Term) = bindLambda(applyPars(t, c))
    def applyPlain(c: Context, t: Term) = applyDef(c,t) // This is probably not correct, but recovers original behavior.
  }

  override def elaborateInContext(context: Context, dv: VarDecl): Context = dv match {
    case VarDeclFeature(LocalName(ComplexStep(dom) :: Nil), OMMOD(q), None) if dom == q && !context.contains(dv) =>
      val thy = controller.simplifier.getBody(context, OMMOD(dom)) match {
        case t : Theory => t
        case _ => throw GetError(dom, "variable feature declaration does not refer to a theory")
      }
      controller.simplifier.apply(thy)
      implicit val vars = thy.parameters.filter(_.feature.isEmpty)
      val translator = mkTranslator(thy,n => OMV(n))(vars)
      val prefix = ComplexStep(dom)
      val pdecs = thy.getDerivedDeclarations(feature)
      val fin = thy.parameters.flatMap {
        case ndv @ DerivedVarDeclFeature(_,_,_,_) => ndv :: elaborateInContext(context,ndv)
        case _ => Nil
      } ::: thy.getDerivedDeclarations(feature).flatMap {
        case d : DerivedDeclaration if d.feature == feature =>
          val nd = VarDeclFeature(d.name, d.tpC.get.get, None)//(dd.home,d.name,feature,d.tpC,d.notC)
          nd :: elaborateInContext(context,nd)
      } ::: thy.getDeclarationsElaborated.flatMap {
        case d : DerivedDeclaration if d.feature == feature => Nil
          /*
            val nd = DerivedVarDecl(d.name,id,mpath,d.tpC.get.toList)//(dd.home,d.name,feature,d.tpC,d.notC)
            nd :: elaborateInContext(context,nd)
            */
        case d if pdecs.exists(i => d.getOrigin == ElaborationOf(i.path)) => Nil
        case d : Constant =>
          val ret = VarDecl(prefix / d.name, None, d.tp.map {translator.applyType(context,_)},
                                                   d.df.map {translator.applyDef(context,_)}, d.not)//d.translate(parent.toTerm, prefix, translator)
          List(ret)
      }
      fin.indices.collect{
        case i if !(fin.take(i) contains fin(i)) => fin(i)
      }.toList
    case _ => Context.empty
  }
  def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration)(implicit env: Option[ExtendedSimplificationEnvironment] = None) : Elaboration = {
    // println(parent.name + " <- " + dd.name)
    val dom = getDomain(dd)
    val parenth = parent match {
      case th : Theory => th
      case _ => ???
    }
    val context = controller.simplifier.elaborateContext(Context.empty,parenth.getInnerContext)
    val body = controller.simplifier.getBody(context, dom) match {
      case t : Theory => t
      case _ => throw GetError(dd.path, "domain of derived declaration is not a theory")
    }
    controller.simplifier.apply(body)
    val parentContextIncludes = context.collect{
      case VarDeclFeature(LocalName(ComplexStep(n) :: rest2),_,_) => n
    }
    /*
    val checker = controller.extman.get(classOf[Checker], "mmt").getOrElse {
      throw GeneralError(s"no mmt checker found")
    }.asInstanceOf[MMTStructureChecker]
    val bodycont = checker.elabContext(body)(new CheckingEnvironment(new ErrorLogger(report), RelationHandler.ignore,new MMTTask{}))
    */
    val bodycont = controller.simplifier.elaborateContext(Context.empty,body.getInnerContext)
    def parentDerDecls = parenth.getDerivedDeclarations(feature).filterNot(_ == dd)//
    def parentDeclIncludes = parentDerDecls.map(s => getDomain(s).toMPath)
    def dones = parentDerDecls.indices.collect{
      case i if ElaboratedElement.isPartially(parentDerDecls(i)) => parentDeclIncludes(i)
    }
    def doFirsts = (body.getDerivedDeclarations(feature).map(s => getDomain(s).toMPath) ::: bodycont.collect{
      case DerivedVarDeclFeature(LocalName(ComplexStep(n) :: rest2),`feature`,_,_) => n
    }).filterNot(s => parentContextIncludes.contains(s) || dones.contains(s)).headOption
    var doFirst = doFirsts
    while (doFirst.isDefined) {
      // println("  -  " + doFirst.get)
      val old = parentDerDecls.find(d => getDomain(d).toMPath == doFirst.get)
      if (old.isDefined) controller.simplifier.apply(old.get) else {
        val nd = new DerivedDeclaration(
          dd.home,
          LocalName(doFirst.get),
          feature,
          TermContainer(OMMOD(doFirst.get)),
          NotationContainer(None)
        )
        nd.setOrigin(ElaborationOf(dd.path))
        controller.add(nd,After(dd.name))
        controller.simplifier.apply(nd)
        // Thread.sleep(1000)
      }
      doFirst = doFirsts
    }

    val prefix = dd.name
    def toTerm(ln : LocalName) = ln match {
      case LocalName(ComplexStep(cs) :: rest) if parentContextIncludes contains cs =>
        OMV(ln)
      case _ => OMS(dd.parent ? ln)
    }
    val tr = mkTranslator(body,toTerm)(Context.empty)
    implicit val vars = body.parameters.filter(_.feature.isEmpty).map {vd =>
        tr.applyVarDecl(Context.empty,vd)
    }

    val translator = mkTranslator(body,n => toTerm(n))(vars)

    new Elaboration {
      val decls = body.getDeclarationsElaborated.flatMap {
        case d : DerivedDeclaration if d.feature == feature =>
          Nil // case can probably be eliminated
        case d if body.getDerivedDeclarations(feature).exists(i => d.getOrigin == ElaborationOf(i.path)) => Nil
        case d =>
          val ret = d.translate(parent.toTerm, prefix, translator,Context.empty)
          List(ret)//d.translate(parent.toTerm, prefix, translator))
      }
      def domain: List[LocalName] = decls.map(_.name)
      def getO(name: LocalName): Option[Declaration] = decls.find(_.name == name)
    }
    /*


    if (body.name.toString == "nat_types") {
      print("")
    }

    val prefix = dd.name
    val parth = parent.asInstanceOf[DeclaredTheory] // TODO
    val pvars = parth.parameters.collect{
      case DerivedVarDecl(LocalName(ComplexStep(n) :: rest2),`feature`,_,_) => n
    }
    def toTerm(ln : LocalName) = ln match {
      case LocalName(ComplexStep(cs) :: rest) if pvars contains cs =>
        OMV(ln)
      case _ => OMS(dd.parent ? ln)
    }

    val translator = mkTranslator(body,n => toTerm(n))(vars)

    val elab = new Elaboration {
      var ndecs : List[DerivedDeclaration] = Nil
      def pdecs = parth.getDerivedDeclarations(feature) ::: ndecs
      val alldecls = body.parameters.flatMap{
        case DerivedVarDecl(mp,`feature`,_,args) =>
          val old = pdecs.find(_.name == mp)
          if (old.isEmpty) {
            val nd = new DerivedDeclaration(dd.home,mp,feature,TermContainer(args.head),NotationContainer(None))
            ndecs ::= nd
            val ret = elaborate(parent,nd).getDeclarations
            ElaboratedElement.set(nd)
            ndecs = ndecs ::: ret collect {
              case in : DerivedDeclaration if in.feature == feature => in
            }
            nd :: ret
          } else {
            controller.simplifier.apply(old.get)
            Nil
          }
        case _ => Nil
      } ::: body.getDerivedDeclarations(feature).flatMap(d => {
        val old = pdecs.find(_.name == d.name)
        if (old.isEmpty) {
          val nd = new DerivedDeclaration(dd.home,d.name,feature,d.tpC,d.notC)
          ndecs ::= nd
          val ret = elaborate(parent,nd).getDeclarations
          ElaboratedElement.set(nd)
          ndecs = ndecs ::: ret collect {
            case in : DerivedDeclaration if in.feature == feature => in
          }
          nd :: ret
        }
        else {
          controller.simplifier.apply(old.get)
          Nil
        }
      }) ::: body.getDeclarationsElaborated.flatMap {
        case d : DerivedDeclaration if d.feature == feature =>
          Nil // case can probably be eliminated
        case d if body.getDerivedDeclarations(feature).exists(i => d.getOrigin == ElaborationOf(i.path)) => Nil
        case d =>
          val ret = d.translate(parent.toTerm, prefix, translator,Context.empty)
          List(ret)//d.translate(parent.toTerm, prefix, translator))
      }
      val decls = alldecls.indices.collect{
        case i if !alldecls.take(i).exists(d => d.name == alldecls(i).name) &&
          !parth.getDeclarationsElaborated.exists(d => d.name == alldecls(i).name) => alldecls(i)
      }.toList
      //body
      def domain: List[LocalName] = decls.map(_.name)
      def getO(name: LocalName): Option[Declaration] = decls.find(_.name == name)
    }
    elab
    */
  }
  private def checkpath(mp : MPath) = controller.get(mp)
  // def modules(d: DerivedDeclaration): List[Module] = Nil
  def check(d: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = {}
}

object StructuralFeatureUtil {
  def externalDeclarationsToElaboration(decls: List[Constant], log: Option[Constant => Unit] = None) = {
    new Elaboration {
      val elabDecls = decls
      def domain = elabDecls map {d => d.name}
      def getO(n: LocalName) = {
        log map (l => elabDecls.find(_.name == n) map (c => l(c)))
        elabDecls.find(_.name == n)
        
      }
    }
  }
  def singleExternalDeclaration(d: Constant) = {
    externalDeclarationsToElaboration(List(d))
  }
}
