package info.kwarc.mmt.api.symbols

import info.kwarc.mmt.api._
import modules._
import frontend._
import checking._
import uom.ElaboratedElement
import objects._
import notations._

import scala.xml.Elem
import Theory._
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
      {getDeclarations map (_.toNode)}
    </derived>
  }
  // override def toNodeElab
  override def toNode(rh: presentation.RenderingHandler) {
    rh << s"""<derived feature="$feature" name="${name.toPath}" base="${parent.toPath}">"""
    (getMetaDataNode++tpNode++dfNode++notNode) foreach {n =>
      rh(n)
    }
    getDeclarations foreach(_.toNode(rh))
    rh << "</derived>"
  }
}

/** a module that is elaborated into more primitive module */
class DerivedModule(val feature: String, p: DPath, n: LocalName, val meta: Option[MPath], val tpC: TermContainer, val dfC : TermContainer, val notC: NotationContainer)
   extends Module(p,n) with DerivedContentElement {
   def translate(newNS: DPath, newName: LocalName, tl: Translator, con: Context): DerivedModule = {
     new DerivedModule(feature, newNS, newName, meta, translateTp(tl, con), translateDf(tl, con), notC.copy)
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
     val res = new DerivedDeclaration(newHome, prefix/name, feature, tpT, notC.copy, dfT)
     val icont = con ++ getInnerContext
     getDeclarations.foreach {d =>
       res.add(d.translate(res.toTerm, LocalName.empty, tl, icont))
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
   def isApplicable(s: String) = s == feature

   val bodyDelim = "="

   lazy val mpath = SemanticObject.javaToMMT(getClass.getCanonicalName)

   /** the notation for the header */
   def getHeaderNotation: List[Marker]

   /** the parse rule for the header */
   def getHeaderRule = parser.ParsingRule(mpath, Nil, TextNotation(Mixfix(getHeaderNotation), Precedence.integer(0), None))

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

   /** called after checking components and inner declarations for additional feature-specific checks */
   def check(dd: Level)(implicit env: ExtendedCheckingEnvironment): Unit

   /** override as needed */
   def modules(dd: Level): List[Module] = Nil
}

abstract class StructuralFeature(f: String) extends GeneralStructuralFeature[DerivedDeclaration](f) {
  /** additional context relative to which to interpret the body of a derived declaration */
  def getInnerContext(dd: DerivedDeclaration): Context = dd.getInnerContext

   /**
    * defines the outer perspective of a derived declaration
    *
    * @param parent the containing module
    * @param dd the derived declaration
    */
   def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration): Elaboration

   def elaborateInContext(prev: Context, dv: VarDecl): Context = prev
   def checkInContext(prev: Context, dv: VarDecl) {}

   /** for creating/matching variable declarations of this feature */
   object VarDeclFeature extends DerivedVarDeclFeature(feature)
   /** returns the rule constant for using this feature in a theory */
   def getRule = StructuralFeatureRule(getClass, feature)
}

abstract class ModuleLevelFeature(f: String) extends GeneralStructuralFeature[DerivedModule](f)

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
      domain.map {n => getO(n).getOrElse {throw ImplementationError(n + " is said to occur in domain of elaboration but retrieval failed")}}
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

   def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {
     //TODO check IsContext here
   }
   
  object noLookupPresenter extends presentation.NotationBasedPresenter {
    override def getNotations(p: GlobalName) = if (! (p.doc.uri.path contains "urtheories")) Nil else super.getNotations(p)
    override def getAlias(p: GlobalName) = if (true) Nil else super.getAlias(p)
  }
  
  override def start(args: List[String]) {
    initOther(noLookupPresenter)
  }
  
  def defaultPresenter(c: Constant)(implicit con: Controller): String = c.name + ": " + noLookupPresenter.asString(c.tp.get) + (if (c.df != None) " = "+noLookupPresenter.asString(c.df.get) else "")
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
  def elaborate(parent: Module, dd: DerivedDeclaration): Elaboration = {
    new Elaboration {
      def domain: List[LocalName] = dd.getDeclarations.map(_.name)
      def getO(nm: LocalName): Option[Declaration] = dd.getDeclarations.find(_.name == nm).map {
        case c: Constant => Constant(parent.toTerm,c.name,c.alias,c.tp,c.df,Some(self.feature.toString),c.notC)
      }
    }
  }
  def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {}
}

trait UnnamedUntyped {self : StructuralFeature =>
  def getHeaderNotation: List[Marker] = Nil
  override def processHeader(header: Term) = (LocalName(self.feature),OMMOD(mpath))
    // Type is completely useless here, but for some reason it nees to return SOME term...
  override def makeHeader(dd: DerivedDeclaration) = OMMOD(mpath)

  def elaborate(parent: Module, dd: DerivedDeclaration): Elaboration = {
    new Elaboration {
      def domain: List[LocalName] = dd.getDeclarations.map(_.name)
      def getO(nm: LocalName): Option[Declaration] = dd.getDeclarations.find(_.name == nm).map {
        case c: Constant => Constant(parent.toTerm,c.name,c.alias,c.tp,c.df,Some(self.feature.toString),c.notC)
      }
    }
  }
  def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {}
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
  def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {
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
  def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {
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
         case OMBINDC(OMMOD(mpath), pars, OMS(p)::args) => (p, pars, args)
       }
     }
     /** retrieves the parameters */
     def getParameters(dd: DerivedDeclaration) = {getParams(dd)._2}
   }
}

/**
 * Generative, definitional functors/pushouts with free instantiation
 * called structures in original MMT
 */
class GenerativePushout extends StructuralFeature("generative") with IncludeLike {

  def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration) = {
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
        private val translator = new ApplyMorphism(morphism.toTerm)
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

   def check(d: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {}
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
  }

  override def elaborateInContext(context: Context, dv: VarDecl): Context = dv match {
    case VarDeclFeature(LocalName(ComplexStep(dom) :: Nil), OMMOD(q), None) if dom == q && !context.contains(dv) =>
      val thy = controller.simplifier.getBody(context, OMMOD(dom)) match {
        case t : Theory => t
        case _ => throw GetError("Not a declared theory: " + dom)
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
  def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration) : Elaboration = {
    // println(parent.name + " <- " + dd.name)
    val dom = getDomain(dd)
    val parenth = parent match {
      case th : Theory => th
      case _ => ???
    }
    val context = controller.simplifier.elaborateContext(Context.empty,parenth.getInnerContext)
    val body = controller.simplifier.getBody(context, dom) match {
      case t : Theory => t
      case _ => throw GetError("Not a theory: " + dom)
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
  def check(d: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {}
}
