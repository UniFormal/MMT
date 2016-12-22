package info.kwarc.mmt.api.symbols

import info.kwarc.mmt.api.{LocalName, _}
import modules._
import frontend._
import checking._
import uom.ElaboratedElement
import objects._
import notations._

import scala.xml.Elem

/** A [[DerivedDeclaration]] is a syntactically like a nested theory.
 *  Its semantics is defined by the corresponding [[StructuralFeature]]
 */
class DerivedDeclaration(h: Term, name: LocalName, override val feature: String, val tpC: TermContainer, val notC: NotationContainer) extends {
   private val t = new DeclaredTheory(h.toMPath.parent, h.toMPath.name/name, None)
} with NestedModule(h, name, t) with HasNotation {
   // overriding to make the type stricter
  override def module: DeclaredModule = t
  def modulePath = module.path

  override def getComponents = TypeComponent(tpC) :: notC.getComponents
  
  private def tpAttNode = tpC.get.map {t => backend.ReadXML.makeTermAttributeOrChild(t, "type")}.getOrElse((null,Nil))
  override def toNode : Elem = {
    val (tpAtt, tpNode) = tpAttNode
    <derived feature={feature} name={name.toPath} base={t.parent.toPath} type={tpAtt}>
      {tpNode}
      {notC.toNode}
      {t.getDeclarations map (_.toNode)}
    </derived>
  }
  // override def toNodeElab
  override def toNode(rh: presentation.RenderingHandler) {
    val (tpAtt, tpNode) = tpAttNode
    rh << s"""<derived feature="$feature" name="${name.toPath}" base="${t.parent.toPath}">"""
    tpC.get.foreach {tp =>
      rh << "<type>"
      rh(tp.toNode)
      rh << "</type>"
    }
    rh(notC.toNode)
    t.getDeclarations foreach(_.toNode(rh))
    rh << "</derived>"
  }

  override def toString = {
    val s1 = {
      name match {
        case LocalName(ComplexStep(p) :: Nil) => ""
        case _ => " " + name
      }
    }
    val s2 = if (t.getDeclarations.nonEmpty) " =\n" + t.innerString else ""
    feature + s1 + tpC.get.map(" " + _.toString).getOrElse("") + s2
  }

  override def translate(newHome: Term, prefix: LocalName, translator: Translator): DerivedDeclaration = {
     // translate this as a [[NestedModue]], then extend the result to a DerivedDeclaration
     val superT = super.translate(newHome, prefix, translator) // temporary, will be used to build result
     val tpT = tpC.get map {tp => translator.applyType(Context.empty, tp)}
     // splice super in to res
     val res = new DerivedDeclaration(superT.home, superT.name, feature, TermContainer(tpT), notC)
     superT.module.getDeclarations.foreach {d =>
       res.module.add(d)
     }
     res
   }
}


/**
 * a rule that legitimizes a [[StructuralFeature]]
 */
case class StructuralFeatureRule(feature: String) extends Rule {
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
abstract class StructuralFeature(val feature: String) extends FormatBasedExtension {
   def isApplicable(s: String) = s == feature
   
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
       case OMA(OMMOD(`mpath`), OML(name, None, None)::args) =>
         val tp = OMA(OMMOD(mpath), args)
         (name, tp)
     }
   }
   
   /** inverse of processHeader */
   def makeHeader(dd: DerivedDeclaration): Term = {
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
  
   /** additional context relative to which to interpret the body of a derived declaration */ 
   def getInnerContext(dd: DerivedDeclaration): Context = Context.empty

   /** called after checking components and inner declarations for additional feature-specific checks */
   def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit
   def checkInContext(prev : Context, dv : VarDecl)

   /**
    * defines the outer perspective of a derived declaration
    *
    * @param parent the containing module
    * @param dd the derived declaration
    */
   def elaborate(parent: DeclaredModule, dd: DerivedDeclaration): Elaboration
   def elaborateInContext(prev : Context, dv : VarDecl) : Context

   /** override as needed */
   def modules(dd: DerivedDeclaration): List[Module] = Nil
   
   /** returns the rule constant for using this feature in a theory */
   def getRule = StructuralFeatureRule(feature)
}

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
trait ParametricTheoryLike {self: StructuralFeature =>
   val Type = ParametricTheoryLike.Type(getClass)
   
   def getHeaderNotation = List(LabelArg(2, false, false), Delim("("), Var(1, true, Some(Delim(","))), Delim(")")) 

   override def getInnerContext(dd: DerivedDeclaration) = Type.getParameters(dd)
   
   override def processHeader(header: Term) = header match {
     case OMBIND(OMMOD(`mpath`), cont, OML(name,None,None)) => (name, Type(cont))
   }
   override def makeHeader(dd: DerivedDeclaration) = dd.tpC.get match {
     case Some(Type(cont)) => OMBIND(OMMOD(mpath), cont, OML(dd.name, None,None))
   }

   def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {
     //TODO check IsContext here
   }
}

/** helper object */
object ParametricTheoryLike {
   /** official apply/unapply methods for the type of a ParametricTheoryLike derived declaration */ 
   case class Type[A <: ParametricTheoryLike](cls: Class[A]) {
     val mpath = SemanticObject.javaToMMT(cls.getCanonicalName)
     
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
}

/**
 * Generative, definitional functors/pushouts with free instantiation
 * called structures in original MMT
 */
class GenerativePushout extends StructuralFeature("generative") with IncludeLike {

  def checkInContext(prev : Context, dv: VarDecl): Unit = {}
  def elaborateInContext(prev: Context, dv: VarDecl): Context = Context.empty
  
  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) = {
      val dom = getDomain(dd)
      val context = parent.getInnerContext
      val body = controller.simplifier.materialize(context, dom, true, None) match {
        case m: DeclaredModule => m
        case m: DefinedModule => throw ImplementationError("materialization failed")
      }
      
      new Elaboration {
        /** the morphism dd.dom -> parent of the pushout diagram: it maps every n to dd.name/n */
        private val morphism = new DeclaredView(parent.parent, parent.name/dd.name, dom, parent.toTerm, false)
        /** precompute domain and build the morphism */
        val domain = body.getDeclarationsElaborated.map {d =>
          val ddN = dd.name / d.name
          val assig = Constant(morphism.toTerm, d.name, Nil, None, Some(OMS(parent.path ? ddN)), None)
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
                val dT = d.translate(parent.toTerm, dd.name, translator)
                val dTM = dd.module.getO(rest) match {
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
  def checkInContext(prev : Context, dv: VarDecl): Unit = dv match {
    case DerivedVarDecl(LocalName(ComplexStep(p) :: Nil),`id`,`mpath`,List(OMMOD(q))) if p == q => checkpath(p)
    case _ =>
  }


  private def bindPi(t : Term)(implicit vars : Context) = if (vars.nonEmpty) OMBIND(OMS(pi),vars,t) else t
  private def bindLambda(t : Term)(implicit vars : Context) = if (vars.nonEmpty) OMBIND(OMS(lambda),vars,t) else t

  private def applyParams(body : DeclaredTheory,toTerm : LocalName => Term)(vars : Context) = new StatelessTraverser {
    val varsNames = vars.map(_.name)
    def traverse(t: Term)(implicit con: Context, state: State) : Term = t match {
      case OMS(p) if p.module == body.path => //dd.module.path =>
        vars.foldLeft[Term](toTerm(LocalName(body.path) / p.name))((tm,v) => OMA(OMS(applys),List(tm,OMV(v.name))))
      case OMBINDC(bind, bvars, scps) =>
        // rename all bound variables that are among the parameters to avoid capture
        val (bvarsR, bvarsSub) = Context.makeFresh(bvars, varsNames ::: con.domain)
        OMBINDC(traverse(bind), traverseContext(bvarsR), scps map {s => traverse(s ^? bvarsSub)(con++bvarsR, state)})
      case _ => Traverser(this,t)
    }
  }
  private def mkTranslator(body : DeclaredTheory, toTerm : LocalName => Term)(implicit vars : Context) = new Translator {
    val applyPars = applyParams(body,toTerm)(vars)
    def applyType(c: Context, t: Term) = bindPi(applyPars(t, c))
    def applyDef(c: Context, t: Term) = bindLambda(applyPars(t, c))
  }

  def elaborateInContext(context: Context, dv: VarDecl): Context = dv match {
    case DerivedVarDecl(LocalName(ComplexStep(dom) :: Nil), `id`,`mpath`, List(OMMOD(q))) if dom == q =>
      val body = controller.simplifier.getBody(context, OMMOD(dom)) match {
        case t : DeclaredTheory => t
        case _ => throw GetError("Not a declared theory: " + dom)
      }
      controller.simplifier.apply(body)
      implicit val vars = body.parameters
      val translator = mkTranslator(body,n => OMV(n))
      val prefix = ComplexStep(dom)
      body.getDeclarationsElaborated.flatMap {
        case d : DerivedDeclaration if d.feature == feature =>
          if (!body.getDerivedDeclarations(feature).map(_.name).contains(d.name)) {
            val nd = DerivedVarDecl(d.name,id,mpath,d.tpC.get.toList)//(dd.home,d.name,feature,d.tpC,d.notC)
            nd :: elaborateInContext(context,nd)
          }
          else Nil
        case d : FinalConstant =>
          val ret = VarDecl(prefix / d.name,d.tp.map(translator.applyType(context,_)),
            d.df.map(translator.applyDef(context,_)),d.not)//d.translate(parent.toTerm, prefix, translator)
          List(ret)
      }
    case _ => Context.empty
  }

  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) : Elaboration = {
    val dom = getDomain(dd)
    val context = parent.getInnerContext
    val body = controller.simplifier.getBody(context, dom) match {
      case t : DeclaredTheory => t
      case _ => throw GetError("Not a declared theory: " + dom)
    }
    controller.simplifier.apply(body)
    implicit val vars = body.parameters
    /*
    if (vars.isEmpty) return new Elaboration {
      val includes = PlainInclude(parent.path,body.path) :: body.getIncludes.map(PlainInclude(parent.path,_))
      def domain = includes.map(_.name)
      def getO(name: LocalName): Option[Declaration] = includes.find(_.name == name)
    }
    */
    val translator = mkTranslator(body,n => OMS(dd.parent ? n))

    val prefix = dd.name
    val elab = new Elaboration {
      val decls = body.getDeclarationsElaborated.flatMap {
        case d : DerivedDeclaration if d.feature == feature =>
          if (!body.getDerivedDeclarations(feature).map(_.name).contains(d.name)) {
            val nd = new DerivedDeclaration(dd.home,d.name,feature,d.tpC,d.notC)
            nd :: elaborate(parent,nd).getDeclarations
          }
          else Nil
        case d =>
          // println(controller.presenter.asString(d))
          val ret = d.translate(parent.toTerm, prefix, translator)
          // println(controller.presenter.asString(ret))
          List(ret)//d.translate(parent.toTerm, prefix, translator))
      /*  DM's old code
        case c: Constant =>
          //TODO @DM I don't think it's correct to treat generated constants differently here --FR 
          val tpE = if (c.isGenerated) c.tp else c.tp.map(t => bindPi(applyPars(t, Context.empty)))
          val dfE = if (c.isGenerated) c.df else c.df.map(t => bindLambda(applyPars(t, Context.empty)))
          val cE = Constant(parent.toTerm, prefix/c.name, c.alias.map(prefix/_),tpE, dfE, c.rl, c.notC)
          List(cE)
        case s : DeclaredStructure =>
          //TODO @DM This does not look right. We may have to drop the structure if vars.nonEmpty --FR 
          val ns = DeclaredStructure(parent.toTerm, LocalName(s.parent)/s.name, s.from, s.isImplicit)
          ElaboratedElement.set(ns)
          List(ns)
        case decl => throw LocalError("TODO: BoundTheoryParameters/Elaboration Declaration case " + decl.getClass)
      */
      }
      //body
      def domain: List[LocalName] = decls.map(_.name)
      def getO(name: LocalName): Option[Declaration] = decls.find(_.name == name)
    }
    // println(vars)
    elab
  }
  private def checkpath(mp : MPath) = controller.get(mp)
  // def modules(d: DerivedDeclaration): List[Module] = Nil
  def check(d: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = {}
}