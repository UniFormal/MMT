package info.kwarc.mmt.api.modules

import info.kwarc.mmt.api._
import frontend._
import symbols._
import objects._

/*
 library should store computedModules and computedGeneralizedMorphisms (= ExpressionTransformers)
 StructureTransformer is applicable to some terms
 in that case, it produces any number of the above and stores them in library
 library checks its tables to lookup terms, call StructureTransformer if necessary to materialize
 a generalized morphism is given by its action on Terms and can be composed like other morphisms
 lookup in a generalized morphism and morphism application are done by calling the ExpressionTransformer
*/

/**
  * maps extensions of theory 'from' to extensions of theory 'to'
  *
  */
abstract class StructureTransformer[ET <: ExpressionTransformer] extends Extension {
  def from: MPath
  def to: MPath

  def applyModuleName(s: String): String

  /** caches the generated modules */
  protected val translatedModules = new scala.collection.mutable.HashMap[MPath,List[Module]]
  /** caches the generated expression transformations */
  // We don't use ET here because a structure transformer might generate multiple expression transformer of different types
  protected val exprTrans = new scala.collection.mutable.HashMap[(MPath,MPath),ExpressionTransformer]

  /** central method for adding a declaration to the controller */
  protected def registerDeclaration(d: Declaration, dT: Declaration): Unit = {
    controller.add(dT)
  }

  /** returns an expression translator
    * a new translator is generated for every module
    */
  def makeExprTrans(from: MPath, to:MPath): ET

  /** translates a module path without building the translated theory */
  def applyModulePath(mp: MPath): MPath = {
    val h = mp.name.head
    val hN = h match {
      case SimpleStep(n) => LocalName(applyModuleName(n))
      case ComplexStep(_) => SimpleStep(applyModuleName("")) / h
    }
    mp.doc ? (hN / mp.name.tail)
  }

  /** translates a path and builds the desired theory */
  def applyModule(mp: MPath): Unit = {
    if (mp == from) return to // optimization
    val m = controller.getAs(classOf[Module],mp)
    applyModule(m)
  }

  def applyModule(module: Module): Unit = {
    if (module.path == from) return // monomorphic base becomes polymorphic base
    translatedModules.get(module.path).foreach {return} // already translated
    // skip modules that do not depend on from
    module match {
      case t: Theory =>
    }
    // otherwise rename the module
    val p = module.path
    val pT = applyModulePath(p)
    val ae = makeExprTrans(p, pT)
    val cont = Context.empty
    def aeMod(t: Term) = ae.applyModule(cont,t)
    val moduleT: Module = module match {
      case t: Theory =>
        val paramT = t.paramC.map(par => ae.applyContext(cont,par))
        val dfT = t.dfC map aeMod
        Theory(pT.doc,pT.name,t.meta,paramT,dfT)
      case v: View =>
        val fromT = v.fromC map aeMod
        val toT = v.toC map aeMod
        val dfT = v.dfC map aeMod
        new View(pT.doc,pT.name,fromT,toT,dfT,v.isImplicit)
    }
    controller.add(moduleT)
    translatedModules(module.path) ::= moduleT
    exprTrans((p,pT)) = ae
    module.getDeclarations.foreach {d => applyDeclaration(module,moduleT,d,ae)}
  }

  // called on every Declaration
  def applyDeclaration(in: Module,out: Module,d: Declaration,ae: ET): Unit = {
    d match {
      case c: Constant =>
        applyConstant(in,out,c,ae)
      case s: Structure =>
        applyStructure(in,out,s,ae)
    }
  }

  // called for every Constant, factored out for easy overriding
  def applyConstant(in: Module,out: Module,c: Constant,ae: ET): Unit = {
    val cT = c.translate(out.toTerm,LocalName.empty,ae,Context.empty)
    registerDeclaration(c,cT)
  }
  // called for every Structure, factored out for easy overriding
  def applyStructure(in: Module,out: Module, s: Structure,ae: ET): Unit = {
    val sT = s.translate(out.toTerm,LocalName.empty,ae,Context.empty)
    registerDeclaration(s,sT)
  }
}

/**
  * expression translation used by a [StructureTransformer]
  *
  * This is a generalization of morphisms:
  * expressions over 'from' are mapped to expressions over 'to'
  */
abstract class ExpressionTransformer extends Translator {
  def from: MPath
  def to: MPath
}

/**
  * turns monomorphic theories based on form{term: type} into polymorphic ones based on to{tp: type, tm: tp --> type}
  */
/* Note: not applicable to
   * features that create new tp's: no monomorphic analogue
   * equality: no way to translated (term --> term) to (tm a --> tm b)
     failing ideas:
     * standard translation: works for eq but not eqcong
     * one tm ?a per occurrence of term, then type check to reduce unknowns (works for eqcong but not eq)
 */
class PolymorphifyStructure(val from: MPath, term: GlobalName, val to: MPath, tp: GlobalName, tm: GlobalName)
  extends StructureTransformer[PolimorphifyExpr] {self =>
  // all theories included into the meta-theory of from
  private val metaPaths: List[MPath] = Nil //TODO

  def applyModuleName(s: String) = "Typed"
  /** t: A --->  [u:tp] t' : {u:tp} t' where t' replaces c with c u if alsoPoly(c) was called
    */
  def makeExprTrans(f: MPath,t: MPath) = new PolimorphifyExpr(f,t, this)
  def getExprTrans(f: MPath):PolimorphifyExpr = {
    exprTrans.get(f,applyModulePath(f)) match {
      case Some(et: PolimorphifyExpr) => et
      case None =>
        applyModule(f)
        getExprTrans(f)
    }
  }

  override def applyModule(module: Module): Unit = {
    if (metaPaths contains module.path) return // meta-theory remains unchanged
    super.applyModule(module)
  }

  override def applyConstant(in:Module, out: Module, c: Constant, ae: PolimorphifyExpr): Unit = {
    // avoid needlessly polymorphifying auxiliary constants
    val polymorphify = c.tp.exists(_.paths contains term) || c.df.exists(_.paths contains term)
    if (polymorphify) {
      super.applyConstant(in,out,c,ae)
      ae.madePoly ::= c.path
    } else {
      val cT = c.translate(out.toTerm,LocalName.empty,IdentityTranslator,Context.empty)
      registerDeclaration(c,cT)
    }
  }
}

class PolimorphifyExpr(val from: MPath, val to: MPath, st: PolymorphifyStructure) extends ExpressionTransformer {
  var madePoly: List[GlobalName] = Nil
  /** the variable of the binding */
  // TODO prevent shadowing
  private val u = OMV("u")

  private val tr = OMSReplacer {p =>
    if (st.getExprTrans(p.module).madePoly contains p) {
      // the polymorphic type for u
      val pT = OMA(OMS(p), List(u)) // should be LF apply
      Some(pT)
    } else
      None
  }
  def applyPlain(c: Context, t: Term) = tr(t,c)
  def applyType(c: Context, t: Term) = t // Pi(u, tp, applyPlain(c++ u%tp, t))
  def applyDef(c: Context, t: Term) = t // Lambda(u, tp, applyPlain(c ++ u%tp, t))
  override def applyAtomicModule(p: MPath) = {
    // this call automatically generates the module if it has not been generated yet
    st.applyModule(p)
    st.applyModulePath(p)
  }
}


/**
  * @param sort the name of the sort to use
  */
// class FOLToSFOL(sort: GlobalName) extends Functor {}

/** pushout is the special case of Functor where expressions are translated along a morphism */
abstract class Pushout(val from: MPath, val to: MPath, mor: Term) extends StructureTransformer {
  // def applyExpr = ApplyMorphs(mor)
}
