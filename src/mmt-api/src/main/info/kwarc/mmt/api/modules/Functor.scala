package info.kwarc.mmt.api.modules

import info.kwarc.mmt.api._
import frontend._
import symbols._
import objects._

/**
  * expression translation used by a [Functor]
  */
abstract class ExpressionTransformation extends Translator {
  private val translatedDecls = new scala.collection.mutable.HashMap[GlobalName,List[Declaration]]
  def translated(in: Declaration, out: Declaration) {
    translatedDecls(in.path) ::= out
  }
}

/**
  * maps extensions of theory 'from' to extensions of theory 'to'
  *
  */
abstract class Functor extends Extension {
  def from: MPath
  def to: MPath

  def applyModuleName(s: String): String

  /** caches the translations of modules */
  private val translatedModules = new scala.collection.mutable.HashMap[MPath,List[Module]]
  def registerDeclaration(d: Declaration, dT: Declaration, ae: ExpressionTransformation) {
    controller.add(dT)
    ae.translated(d,dT)
  }

  /** returns an expression translator
    * a new translator is generated for every module
    */
  def applyExpr: ExpressionTransformation

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
  def applyModule(mp: MPath) {
    if (mp == from) return to // optimization
    val m = controller.getAs(classOf[Module],mp)
    applyModule(m)
  }

  def applyModule(module: Module) {
    if (module.path == from) return // monomorphic base becomes polymorphic base
    translatedModules.get(module.path).foreach {return} // already translated
    // skip modules that do not depend on from
    module match {
      case t: Theory =>
    }
    // otherwise rename the module
    val ae = applyExpr
    val cont = Context.empty

    def aeMod(t: Term) = ae.applyModule(cont,t)

    val pT = applyModulePath(module.path)
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
    module.getDeclarations.foreach {d => applyDeclaration(module,moduleT,d,ae)}
    translatedModules(module.path) ::= moduleT
    moduleT
  }

  // called on every Declaration
  def applyDeclaration(in: Module,out: Module,d: Declaration,ae: ExpressionTransformation) {
    d match {
      case c: Constant =>
        applyConstant(in,out,c,ae)
      case s: Structure =>
        applyStructure(in,out,s,ae)
    }
  }

  // called for every Constant, factored out for easy overriding
  def applyConstant(in: Module,out: Module,c: Constant,ae: ExpressionTransformation) {
    val cT = c.translate(out.toTerm,LocalName.empty,ae,Context.empty)
    registerDeclaration(c,cT,ae)
    ae.translated(c,cT)
  }
  // called for every Structure, factored out for easy overriding
  def applyStructure(in: Module,out: Module, s: Structure,ae: ExpressionTransformation) {
    val sT = s.translate(out.toTerm,LocalName.empty,ae,Context.empty)
    registerDeclaration(s,sT,ae)
  }
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
class Polymorphify(val from: MPath, term: GlobalName, val to: MPath, tp: GlobalName, tm: GlobalName) extends Functor {self =>
  // all theories included into the meta-theory of from
  private val metaPaths: List[MPath] = Nil //TODO

  def applyModuleName(s: String) = "Typed"
  /** t: A --->  [u:tp] t' : {u:tp} t' where t' replaces c with c u if alsoPoly(c) was called
    */
  class ApplyExpr extends ExpressionTransformation {
    private var madePoly: List[GlobalName] = Nil
    /** the variable of the binding */
    // TODO prevent shadowing
    private val u = OMV("u")
    // the polymorphic type for u
    def poly(p: GlobalName) = OMA(OMS(p), List(u)) // should be LF apply
    private val tr = OMSReplacer {p =>
      if (madePoly contains p) Some(poly(p)) else None
    }
    def applyPlain(c: Context, t: Term) = tr(t,c)
    def applyType(c: Context, t: Term) = t // Pi(u, tp, applyPlain(c++ u%tp, t))
    def applyDef(c: Context, t: Term) = t // Lambda(u, tp, applyPlain(c ++ u%tp, t))
    override def applyAtomicModule(p: MPath) = {
      // this call automatically generates the module if it has not been generated yet
      self.applyModule(p)
      self.applyModulePath(p)
    }
  }
  def applyExpr = new ApplyExpr

  override def applyModule(module: Module) {
    if (metaPaths contains module.path) return // meta-theory remains unchanged
    super.applyModule(module)
  }

  override def applyConstant(in:Module, out: Module, c: Constant, ae: ExpressionTransformation) {
    // avoid needlessly polymorphifying auxiliary constants
    val polymorphify = c.tp.exists(_.paths contains term) || c.df.exists(_.paths contains term)
    if (polymorphify) {
      super.applyConstant(in,out,c,ae)
    } else {
      val cT = c.translate(out.toTerm,LocalName.empty,IdentityTranslator,Context.empty)
      registerDeclaration(c,cT,ae)
    }
  }
}

/**
  * @param sort the name of the sort to use
  */
// class FOLToSFOL(sort: GlobalName) extends Functor {}

/** pushout is the special case of Functor where expressions are translated along a morphism */
abstract class Pushout(val from: MPath, val to: MPath, mor: Term) extends Functor {
  // def applyExpr = ApplyMorphs(mor)
}
