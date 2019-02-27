package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import frontend._
import checking._
import utils._
import documents._
import modules._
import symbols._
import patterns._
import objects._
import notations._
import libraries.AlreadyDefined

import Theory._

import collection.immutable.{HashMap, HashSet}
import scala.util.{Success, Try}

/** used by [[MMTStructureSimplifier]] */
@MMT_TODO("needs review")
case class ByStructureSimplifier(home: Term, view: Term) extends Origin

/**
 * information about elaboration status
 * 0 or absent: no information, assume not elaborated
 * 2: partially: header has been elaborated, some elements in body (e.g., new elements) body must still be elaborated
 * 3: fully: applyElementEnd has been called
 * -1: in progress, apply or applyElementBegin has been called
 * -2: in progress and header already elaborated 
 */
object ElaboratedElement extends ClientProperty[StructuralElement,Int](utils.mmt.baseURI / "clientProperties" / "controller" / "elaborated") {
  def getDefault(t: StructuralElement) = get(t).getOrElse(0)
  def setInprogress(t: StructuralElement) = put(t, -get(t).getOrElse(1).abs)
  def isInprogress(t: StructuralElement) = getDefault(t) < 0
  def setPartially(t: StructuralElement) = put(t, get(t).getOrElse(1).signum * 2)
  def isPartially(t : StructuralElement): Boolean = getDefault(t).abs >= 2
  def setFully(t: StructuralElement) = put(t,3)
  def isFully(t : StructuralElement) : Boolean = getDefault(t).abs >= 3
}


/**
 * the primary class for all flattening, materialization, enriching of theories
 * 
 * *internal* flattening of s: change the body of s by flattening all its components and children.
 * *external* flattening of s: change the parent of s by adding all declarations induced by s.
 * Internal flattening includes the recursive internal and external flattening of its children.
 * 
 * Most declarations are subject to either internal (modules and links) or external (includes, structures, derived declarations) flattening.
 * Only non-include structures are subject to both.  
 */
class ElaborationBasedSimplifier(oS: uom.ObjectSimplifier) extends Simplifier(oS) with ChangeListener {
  private lazy val memory = controller.memory
  private lazy val lup = controller.globalLookup
  private lazy val rci = new RuleConstantInterpreter(controller)

  override def logPrefix = "structure-simplifier"

  // for efficiency, we use variants where the parent and its rules are already known; the main interface methods just defer to them
  def applyChecked(s: StructuralElement)(implicit env: SimplificationEnvironment) {applyWithParent(s, None, None)}
  def applyElementBegin(s: StructuralElement)(implicit env: SimplificationEnvironment) {applyElementBeginWithParent(s, None, None)}
  
  // internal and external flattening of s
  // equivalent to calling applyElementBegin and (if applicable) applyElementEnd
  private def applyWithParent(s: StructuralElement, parentO: Option[ModuleOrLink], rulesO: Option[RuleSet])(implicit env: SimplificationEnvironment) {
    if (ElaboratedElement.isInprogress(s) || ElaboratedElement.isFully(s))
      return
    applyElementBeginWithParent(s, parentO, rulesO)
    s match {
      case m: ContainerElement[_] =>
        applyElementEnd(m)
      case _ =>
    }
  }
  
  // internal and external flattening of s except for (in the case of container elements) those parts performed in applyElementEnd
  private def applyElementBeginWithParent(s: StructuralElement, parentO: Option[ModuleOrLink], rulesO: Option[RuleSet])(implicit env: SimplificationEnvironment) {
    if (ElaboratedElement.isInprogress(s) || ElaboratedElement.isFully(s))
      return
    log("flattening " + s.path)
    ElaboratedElement.setInprogress(s)
    // internal flattening
    s match {
      case Include(_, _, _) =>
        // no need to flatten inside an include (this case is needed so that the next case can handle declared modules and strucutres together)
      case _: DerivedDeclaration =>
        // nothing to do, but would otherwise be caught be next case
      case m: ModuleOrLink =>
        // flatten header and call flattenDeclaration on every child
        val rules = RuleSet.collectRules(controller, m.getInnerContext)
        if (!ElaboratedElement.isPartially(s)) {
          flattenDefinition(m, Some(rules))
          m match {
            case t: AbstractTheory =>
              t.meta foreach apply
            case v: Link =>
              applyChecked(materialize(Context.empty,v.from,None, Some(v.fromC)))
              v match {
                case v: View => applyChecked(materialize(Context.empty,v.to,None,Some(v.toC)))
                case _: Structure =>
              }
            case _ =>
          }
          ElaboratedElement.setPartially(s)
          // TODO flattenContext(m.parameters)
        }
        // recurse into all children
        m.getDeclarations.foreach {d =>
          applyWithParent(d, Some(m), Some(rules))
        }
      case nm: NestedModule =>
        applyElementBegin(nm.module)
      case _ =>
    }
    // external flattening
    s match {
      case d: Declaration => 
        d match {
          case _:ContainerElement[_] =>
            // done in applyElementEnd, i.e., when all children have been added
          case d =>
            flattenExternally(d, parentO, rulesO)
        }
      case _ =>
    }
     // mark as elaborated (unless applyElementEnd is still called later)
    s match {
      case _: ContainerElement[_] =>
      case _ =>
        ElaboratedElement.setFully(s)
        env.task.reportProgress(Elaborated(s))
    }
  }

  def applyElementEnd(s: ContainerElement[_])(implicit env: SimplificationEnvironment) {
    if (ElaboratedElement.isFully(s))
      return
    log("finalize flattening of " + s.path)
    s match {
      case dm: DerivedModule =>
        flattenDerivedModule(dm, None)
      case m: Module =>
      case d: Declaration =>
        // external flattening of structures, declared declarations
        flattenExternally(d, None, None)
      case _ =>
    }
    log("done flattening " + s.path)
    ElaboratedElement.setFully(s)
    env.task.reportProgress(Elaborated(s))
  }

  @MMT_TODO("needs to be reviewed")
  def elaborateContext(outer: Context, con: Context) : Context = {
    var ret = Context.empty
    def currentContext = outer ++ ret
    con.foreach {vd =>
      val r = vd match {
        case dv @ DerivedVarDeclFeature(name,feat,_,_) =>
          val sfOpt = controller.extman.get(classOf[StructuralFeature], feat)
          if (sfOpt.isDefined) {
            // sfOpt.get.checkInContext(currentContext,dv)
            vd :: sfOpt.get.elaborateInContext(currentContext,dv)
          } else List(vd)
        case v => List(v)
      }
      ret = ret ++ r
    }
    ret
  }

  /** elaborates the definition into a context and adds the corresponding declarations */
  private def flattenDefinition(mod: ModuleOrLink, rulesOpt: Option[RuleSet] = None) {
    lazy val rules = rulesOpt.getOrElse {
      RuleSet.collectRules(controller, mod.getInnerContext)
    }
    val at = new RepeatedAdd(AtBegin)
    def add(d: Declaration) {
      d.setOrigin(ElaborationOfDefinition)
      controller.add(d, at.getNextFor(d))
      log("flattening yields " + d.path)
    }
    mod.dfC.normalize {d =>
      val dS = objectLevel(d, SimplificationUnit(mod.getInnerContext, false, true), rules)
      log("normalizing definiens to " + controller.presenter.asString(dS))
      dS
    }
    mod match {
      case thy: Theory =>
        thy.dfC.normalized.foreach {
          //TODO mod.getInnerContext is too small for nested theories
            case ComplexTheory(cont) =>
              cont.asDeclarations(mod.toTerm).foreach {d =>
                add(d)
              }
            case AnonymousTheoryCombinator(at) =>
              if (at.mt.isDefined) {
                val mtTerm = OMMOD(at.mt.get)
                // awkward: library must be explicitly notified about update of meta-theory because changes to meta-theory cannot go through controller.add
                thy.metaC.analyzed = Some(mtTerm)
                controller.memory.content.addImplicit(mtTerm, thy.toTerm, OMIDENT(mtTerm))
              }
              var translations = Substitution() // replace all OML's with corresponding OMS's
              // TODO this replaces too many OML's if OML-shadowing occurs
              def translate(tm: Term) = (OMLReplacer(translations)).apply(tm, Context.empty)
              at.decls foreach {o =>
                o match {
                  case IncludeOML(mp, _) =>
                    val d = PlainInclude(mp,thy.path)
                    add(d)
                    // we assume all references to included symbols already use OMS, i.e., do not have to be translated
                  case RealizeOML(mp, _) =>
                    try {
                      //TODO this fails because meta-includes composed with structuralinclude do not properly reduce to identity
                      //controller.memory.content.addImplicit(OMMOD(mp), thy.toTerm, OMStructuralInclude(mp,thy.path))
                    } catch {case ad @ AlreadyDefined(f,t,o,n) =>
                      logError("skipping realization because " + ad.toString)
                    }
                  case o =>
                    val tpT = o.tp map translate
                    val dfT = o.df map translate
                    val ntT = NotationContainer(o.nt)
                    translations ++= Sub(o.name, OMS(thy.path ? o.name))
                    val c = Constant(thy.toTerm, o.name, Nil, tpT, dfT, None, ntT)
                    add(c)
                }
              }
            case _ =>
        }
      case link: Link =>
        link.dfC.normalized foreach {
          case AnonymousMorphismCombinator(am) =>
            val to = link.to.toMPath
            val replacer = new OMLReplacer(l => Some(OMS(to ? l)))
            def translate(tm: Term) = replacer(tm, Context.empty)
            //TODO add identity maps for skipped declarations
            am.decls foreach {o =>
              val tpT = o.tp map translate
              val dfT = o.df map translate
              val ntT = NotationContainer(o.nt)
              val c = Constant(link.toTerm, o.name, Nil, tpT, dfT, None, ntT)
              add(c)
            }
          case _ =>
        }
      case dm: DerivedModule =>
        // TODO do we need to do something here?
    }
  }

  /**
   * flattens a derived module by calling the respective [[ModuleLevelFeature]] and adding all generated modules 
   */
  private def flattenDerivedModule(dm: DerivedModule, rulesO: Option[RuleSet])(implicit env: SimplificationEnvironment) {
     controller.extman.get(classOf[ModuleLevelFeature], dm.feature) match {
       case None => Nil
       case Some(sf) =>
          val elab = sf.modules(dm)
          val docO = dm.parentDoc
          elab.foreach {e =>
            e.setOrigin(ElaborationOf(dm.path))
            log("flattening of " + dm.name + " yields " + e)
            controller.add(e)
            docO.foreach {d =>
              val mr = MRef(d, e.path)
              mr.setOrigin(ElaborationOf(dm.path))
              controller.add(mr)
            }
            applyChecked(e)
          }
          ElaboratedElement.setFully(dm)
     }
  }
  
  /** adds elaboration of d to parent
   *  for efficiency, parent and its rules may be passed if they are already known
   *  
   *  this method recurses into apply for dependency closure
   */
  private def flattenExternally(dOrig: Declaration, parentO: Option[ModuleOrLink], rulesO: Option[RuleSet])(implicit env: SimplificationEnvironment) {
    if (ElaboratedElement.isFully(dOrig))
      return
    val parent = parentO getOrElse {
      dOrig.home match {
       case OMID(p) =>
         val par = controller.globalLookup.get(p)
         par match {
           case par: ModuleOrLink => par
           case _ =>
             return   // we don't elaborate in derived declarations
         }
       case _ =>
         return // TODO materialize
      }
    }
    val innerCont = parent.getInnerContext
    val parentMPath = parent.path.toMPath
    lazy val rules = rulesO.getOrElse {
      RuleSet.collectRules(controller, innerCont)
    }
    val inTheory = parent.isInstanceOf[Theory]
    /* We treat theories as a special case of views as follows:
     *  - If mod is a theory that includes OMPMOD(p,as), we say that mod includes OMINST(p,as).
     *  - (source,target) = (mod,mod) if mod is a theory and = (mod.from,mod.to) if mod is a view.
     * 
     * @param from a theory included into source
     * @param mor a morphism from-->target included into mod
     * @param alreadyIncluded list of other (p,m) such that p--m-->target that are included into mod
     * @return the list of includes resulting from flattening (from,mor) 
     */
    def flattenInclude(from: MPath, mor: Term, alreadyIncluded: List[(MPath,Term)]): List[Declaration] = {
      val fromThy = lup.getAs(classOf[Theory], from)
      applyChecked(fromThy)
      val fromIncls = fromThy.getAllIncludes
      fromIncls.flatMap {case (p, pArgs) =>
        // p --OMINST(p,pArgs)--> fromThy --mor--> target
        // we generate a new include p --newMor--> target by composition
        // newMor is the morphism out of p that maps variable x_i to mor(args_i) and constant c to mor(c)
        // in theories: mor=OMINST(from, fromArgs), and newMor=OMINST(p, newArgsN) using substitution; newInclude is a declared structure from OMPMOD(p,newArgsN)
        //    we could generalize this to defined includes (i.e., implicit morphisms); then newInclude would be a defined structure
        // in views v:  mor = v|_fromThy where p --OMINST(p,pArgs)--> fromThy --OMINST--> v.from --v--> target; newIndlude is a defined structure
        val newMor = OMCOMP(OMINST(p,pArgs), mor)
        val newMor1 = Morph.simplify(newMor)(lup)
        val newMorN = oS(newMor1, SimplificationUnit(innerCont, false, true), rules)
        val newInclude = parent match {
          case thy: Theory =>
            val newArgs = newMorN match {
              case OMCOMP(Nil) | OMIDENT(_) => Nil
              case OMINST(np,nas) => if (np == p) nas else Nil // np != p occurs when (p,pArgs) = OMIDENT(p) and thus newMor = mor
              case _ => throw ImplementationError("composition of includes must yield include")
            }
            Include(parent.toTerm, p, newArgs)
          case _: Link =>
            LinkInclude(parent.toTerm, p, newMorN)
        }
        ElaboratedElement.setFully(newInclude) // recursive elaboration already handled by recursively elaborating fromThy
        utils.listmap(alreadyIncluded, p) match {
          // if an include for domain p already exists, we have to check equality
          case Some(existingMor) =>
            val existingMorN = oS(existingMor, SimplificationUnit(innerCont, false, true), rules)
            val eq = Morph.equal(existingMorN,newMorN, OMMOD(p))(lup)
            if (eq) {
              // if equal, we can ignore the new include
              Nil
            } else {
              // otherwise, it is an error
              val List(newStr, exStr) = List(existingMorN,newMorN) map {m => controller.presenter.asString(m)}
              val msg = if (inTheory) {
                "parametric theory included twice with different arguments"
              } else {
                "two unequal morphisms included for the same theory"
              }
              env.errorCont(InvalidElement(dOrig, s"$msg: $newStr != $exStr"))
              Nil
            }
          // otherwise, we add the new include
          case None =>
            List(newInclude)
        }
      }
    }
    var addAfter = true
    val dElab: List[Declaration] = (parent, dOrig) match {
      // ************ includes
      case (thy: Theory, Include(_, from, fromArgs)) =>
        addAfter = false // generated includes are placed before the generating include so that they occur in dependency order
        // plain includes: copy (only) includes (i.e., transitive closure of includes)
        // from.meta is treated like any other include into from (in particular: skipped if from.meta included into thy.meta)
        val mor = OMINST(from,fromArgs)
        // compute all includes into thy or any of its meta-theories
        val thyMetas = TheoryExp.metas(thy.toTerm)(lup).map(p => lup.getAs(classOf[Theory], p))
        val alreadyIncluded = (thy::thyMetas).flatMap(_.getAllIncludes).map {case (p,as) => (p,OMINST(p,as))}
        flattenInclude(from, mor, alreadyIncluded)
      case (link: Link, LinkInclude(_, from, mor)) =>
        // includes in views are treated very similarly; we compose mor with includes into from and check equality of duplicates
        // from.meta is treated like any other include into from (in particular: mor should include the intended meta-morphism out of from.meta)
        // TODO there is no need to flatten IdentityIncludes, transitive closure is enough; this is analogous to how we handle includes in theories
        val alreadyIncluded = link.getIncludes
        flattenInclude(from, mor, alreadyIncluded)
      // ************ structures
      // in theories: copy and translate all declarations from struc.from and its included theories; for every include, generate a composition
      // in views: generate an assignment for each of those declarations; for every include, this is an assignment formed by restriction
      // Definition of semantics: the meta-theory of the domain of a structure does not become a part of containing theory.
      //  - Thus, in theories:
      //     (a) if struc.from.meta is visible to mod.meta via mor, lookup reduces struc|_from.meta to mor anyway
      //     (b) otherwise, well-formed struc must contain a LinkInclude for it, which translates struc.from.meta-constants away when performing the lookup
      //         if it does not contain one, this yields lookup errors when translating those constants
      //  - in views:
      //     (a) mod must anyway contain a LinkInclude for it.
      //     (b) struc.from.meta can be ignored.
      case (_, struc: Structure) =>
        val fromThy = struc.from match {
          case OMPMOD(p,_) =>
            val t = lup.getAs(classOf[Theory], p)
            t
          case exp =>
            // TODO also materialize pushout if a nested theory is visible via an implicit morphism
            materialize(Context(parentMPath),exp,None, Some(struc.tpC))
        }
        applyChecked(fromThy)
        // copy all declarations in theories p reflexive-transitively included into fromThy
        // no need to consider the instantiation arguments because the lookup methods are used to obtain the declarations in the elaboration
        val sElab = (fromThy.getAllIncludesWithoutMeta.map(_._1) ::: List(fromThy.path)).flatMap {p =>
          // in theories:
          //   !refl: pThy --OMINST--> fromThy --struc--> thy
          //   refl:  pThy == fromThy --struc--> thy
          // in views:
          //   !refl: pThy--OMINST--> fromThy --struc--> vw.from --vw--> vw.to with vw|_fromThy = struc.df
          //   refl:  pThy == fromThy --struc--> vw.from --vw--> vw.to with vw|_fromThy = struc.df
          val (pThy,refl) = if (p == fromThy.path) (fromThy,true) else (lup.getAs(classOf[Theory], p), false)
          // pThy is already elaborated at this point
          val prefix = if (refl) struc.name else struc.name / ComplexStep(p)
          // the defined structure with morphism pThy-->target that arises by composing the include with struc
          val structure = if (refl) {
            Nil // this would be struc itself
          } else {
            val ds = lup.getAs(classOf[Declaration], parentMPath ? prefix)
            List(ds)
          }
          // copies of the local declarations of p
          val decls = pThy.getDeclarations.flatMap {
            case Include(_, _, _) =>
              // because pThy is already flattened transitively, we do not have to do a transitive closure
              Nil
            case d: Declaration =>
              if (skipWhenFlattening(d.getOrigin))
                Nil
              else {
                val sdname = prefix / d.name
                val sd = lup.getAs(classOf[Declaration], parentMPath ? sdname)
                List(sd)
              }
          }
          structure ::: decls
        }
        // TODO in links we have to check equality in case of overlap with more specific instantiations; currently that yields AddErrors
        sElab foreach {d => ElaboratedElement.setFully(d)} // recursive elaboration already handled by recursively elaborating fromThy
        sElab
      // ************** derived declarations: elaborate
      case (thy: Theory, dd: DerivedDeclaration) =>
         controller.extman.get(classOf[StructuralFeature], dd.feature) match {
           case None => Nil
           case Some(sf) =>
              val elab = sf.elaborate(thy, dd)
              // val simp = oS.toTranslator(rules, false)
             /*
             val checker = controller.extman.get(classOf[Checker], "mmt").getOrElse {
               throw GeneralError(s"no mmt checker found")
             }.asInstanceOf[MMTStructureChecker]
             var cont = checker.elabContext(parent)(new CheckingEnvironment(new ErrorLogger(report), RelationHandler.ignore,new MMTTask{}))
              */
             /* This throws errors if the declarations are mutually dependent!!
             val contE = elaborateContext(Context.empty,innerCont)
              elab.getDeclarations.map {d =>
                //println(d)
                val dS = d.translate(simp,contE)
                dS
              }
              */
             elab.getDeclarations
         }
      // the treatment of derived declarations in links has not been specified yet
      case (link: Link, dd: DerivedDeclaration) =>
        Nil //TODO 
      case (_, nm: NestedModule) =>
        // nested modules do not affect the semantics of their parent except when they are used, i.e., no external elaboration
        Nil
      case (_,_: Constant) =>
        //TODO maybe allow constants to be marked in a way that forces the normalization of its definiens
        // base case: no elaboration
        Nil
      case (_,_: RuleConstant) =>
        // base case: no elaboration
        Nil
    }
    // if we add after dOrig, we have to reverse dElab to make sure the declarations occur in the same order as in dElab
    val simp = oS.toTranslator(rules, false)
    val contE = elaborateContext(Context.empty,innerCont)

    val atFirst = if (addAfter) After(dOrig.name) else Before(dOrig.name)
    val pos = new RepeatedAdd(atFirst)
    dElab.foreach {e =>
      e.setOrigin(ElaborationOf(dOrig.path))
      val at = pos.getNextFor(e)
      log("flattening of " + dOrig.name + " yields " + e + " at " + at)
      // special case: if we created a new rule, we try to create the rule
      // TODO this is experimental because rules are often not translatable, e.g., along structures
      e match {
        case rc: RuleConstant if rc.df.isEmpty =>
          rci.createRule(rc)
        case _ =>
      }
      val eS = e.translate(simp,contE)
      controller.add(e, at)
    }
    ElaboratedElement.setFully(dOrig)
 }
  
 /** depending on how a declaration was generated, we may or may not want to copy it when flattening a structure
  *  for example, generated rules should be regenerated in the importing theory rather than copied
  */
 private def skipWhenFlattening(o: Origin) = o match {
   case Original => false
   case ElaborationOf(_) => false
   case ElaborationOfDefinition => false
   case _ => true
 }
 
 // TODO change management does not propagate to other theories yet

  /** deletes all declarations that were added by elaborating se */
  override def onDelete(se: StructuralElement) {
     if (! ElaboratedElement.isPartially(se))
       return
     se match {
       case d: Declaration =>
         controller.get(d.home.toMPath) match {
           case thy: Theory =>
             thy.getDeclarations.foreach {e =>
               if (e.getOrigin == ElaborationOf(d.path))
                 thy.delete(e.name)
             }
           case v: Link =>
             v.getDeclarations.foreach {e =>
               if (e.getOrigin == ElaborationOf(d.path))
                 v.delete(e.name)
             }
           case b : DerivedDeclaration =>
             b.getDeclarations.foreach {e =>
               if (e.getOrigin == ElaborationOf(d.path))
                 b.delete(e.name)
           }
           case any =>
             //This should never happen, but simplifies catching other bugs
             throw ImplementationError("Match error while trying to delete old structural element: "+any.toString())
         }
       case _ =>
     }
  }
  /** re-elaborates if old element was */
  override def onUpdate(old: StructuralElement, nw: StructuralElement) {
    onDelete(old)
    if (ElaboratedElement.isPartially(old))
      apply(nw)
  }

  /** recursively mark fully elaborated parents to be only partially elaborated */
  private def markParentUnelaborated(c: StructuralElement) {
    val parent = c.parent match {
      case cp : ContentPath => controller.get(cp)
      case _ => return
    }
    // adding a declaration to a structure or derived declaration is not incremental: it may change the previous elaborations
    // to be safe, we remove the entire elaboration; more efficient solutions could re-elaborate exactly the changed elaborations
    parent match {
      case _: Structure | _: DerivedDeclaration =>
        onDelete(parent)
      case _ =>
    }
    if (ElaboratedElement.isFully(parent)) {
      ElaboratedElement.setPartially(parent)
      markParentUnelaborated(parent)
    }
  }
  
  /** change management */
  override def onAdd(c: StructuralElement) {
    // no need to act if the elaborator added this itself
    c.getOrigin match {
      case ElaborationOf(_) | ElaborationOfDefinition => return
      case _ =>
    }
    // when adding an element into a fully elaborated parent, we have to mark the parent as partially elaborated 
    markParentUnelaborated(c)
  }
  /* unclear old code of onAdd
    // if the parent was generated by a derived declaration, re-elaborate it; TODO: why would this be necessary?
    parent.getOrigin match {
      case GeneratedBy(dp : GlobalName) =>
        val ddOpt = controller.getO(dp)
        ddOpt match {
          case Some(dd : DerivedDeclaration) =>
            val thOpt = controller.getO(dd.parent)
            thOpt match {
              case Some(th : DeclaredModule) =>
                onDelete(dd)
                ElaboratedElement.erase(dd)
                flattenDeclaration(th,dd)
              case _ =>
            }
          case _ =>
        }
      case _ =>
    } 
    // change management for incrementally checked+elaborated parents
    if (!ElaboratedElement.is(parent)) return
    (parent,c) match {
      case (s : DeclaredStructure, dec : Declaration) =>
        controller.getO(s.parent) match {
          case Some(t : DeclaredTheory) =>
            onDelete(s)
            ElaboratedElement.erase(s)
            flattenDeclaration(t,s)
          case _ =>
        }
      case (t : DeclaredTheory, dec : Declaration) =>
        flattenDeclaration(t,dec)
      case (v : DeclaredView, dec : Declaration) =>
        flattenDeclaration(v, dec)
      case (dd : DerivedDeclaration, dec : Declaration) =>
        controller.getO(dd.parent) match {
          case Some(t : DeclaredTheory) =>
            onDelete(dd)
            ElaboratedElement.erase(dd)
            flattenDeclaration(t,dd)
          case _ =>
        }
      case _ =>
    }
  }*/

  private var materializeCounter = 0
  private def newName: MPath = {
     val i = materializeCounter
     materializeCounter += 1
     utils.mmt.mmtbase ? LocalName("")/"E"/i.toString
  }
  /** Elaborate a theory expression into a module
   *
   *  @param exp the theory expression
   *  @param pathOpt the path to use if a new theory has to be created
   *  @param tcOpt the term container hold exp
   */
  def materialize(context: Context, exp: Term, pathOpt: Option[MPath], tcOpt: Option[TermContainer]): Theory = {
    // return previously materialized theory
    tcOpt foreach {tc =>
      tc.normalized foreach {tcN =>
        return materialize(context, tcN, pathOpt, None)
      }
    }
    val (dt, isNew) = exp match {
      case OMMOD(p: MPath) =>
        val d = lup.getTheory(p)
        (d, false)
      case OMPMOD(p, args) =>
        //TODO DefinedTheory (deprecated anyway)
        val t = lup.getTheory(p)
        val path = pathOpt.getOrElse(newName)
        val sub = (t.parameters / args).getOrElse {throw InvalidObject(exp, "wrong number of arguments")}
        val transl = new ApplySubs(sub)
        val ret = new Theory(path.doc, path.name, t.meta, Theory.noParams, Theory.noBase)
        t.getDeclarations.map {d =>
          val dT = d.translate(OMMOD(path), LocalName.empty, transl, context)
          ret.add(dT)
        }
        (ret, true)
      case _ =>
        // create a new theory and return it
        val path = pathOpt.getOrElse(newName)
        if (exp.freeVars.nonEmpty) {
          val msg = "materialization of module with free variables not implemented yet: " + controller.presenter.asString(exp)
          throw GeneralError(msg)
        }
        val ret = new Theory(path.parent, path.name, noMeta, noParams, TermContainer(exp))
        flattenDefinition(ret)
        (ret,true)
    }
    if (isNew) {
      dt.setOrigin(Materialized(exp))
      controller.add(dt)
      tcOpt.foreach {tc => tc.normalized = Some(OMMOD(dt.path))}
    }
    dt
  }

  //TODO move to library if it's not doing anything else
  def getBody(context: Context, moduleExp: Term): ElementContainer[NamedElement] = moduleExp match {
     case OMMOD(p) => lup.getTheory(p)
     //TODO OMPMOD
     case ComplexTheory(cont) => cont
  }

   /* everything below here is Mihnea's enrichment code, which may be outdated or incomplete */

  /** auxiliary method of enriching */
  private lazy val loadAll = {
    memory.ontology.getInds(ontology.IsTheory) foreach {p =>
      controller.get(p)
    }
    memory.ontology.getInds(ontology.IsView) foreach {p =>
      controller.get(p)
    }
  }
  private lazy val modules = controller.memory.content.getModules

  /**
   * adds declarations induced by views to all theories
   */
  def enrich(t : Theory) : Theory =  {
    loadAll
    val tbar = new Theory(t.parent, t.name, t.meta, t.paramC, t.dfC)
    t.getDeclarations foreach {d =>
      tbar.add(d)
    }
    val views = modules collect {
      case v : View if v.to == t.toTerm => v
    } // all views to T

    views foreach { v =>
      val s = v.from
      implicit val rules = makeRules(v)
      modules collect {
        case sprime : Theory if memory.content.visible(sprime.toTerm).toSet.contains(s) =>
          // here we have v : s -> t and sprime includes s -- (include relation is transitive, reflexive)
          // therefore we make a structure with sprime^v and add it to tbar
          /*
          val str = SimpleDeclaredStructure(tbar.toTerm, (LocalName(v.path) / sprime.path.toPath), sprime.path, false)
          sprime.getDeclarations foreach {d =>
            str.add(rewrite(d))
          }
          tbar.add(str)
          */
          //conceptually this should be a structure, but adding the declarations directly is more efficient
          sprime.getDeclarations foreach {
            case c : Constant => tbar.add(rewrite(c, s.toMPath, tbar.path, t.getInnerContext))
            case _ => //nothing for now //TODO handle structures
          }
      }
    }
    tbar
  }
  //Flattens by generating a new theory for every view, used for flatsearch
  def enrichFineGrained(t : Theory) : List[Theory] = {
    loadAll
    var thys : List[Theory] = Nil
    val tbar = new Theory(t.parent, t.name, t.meta, t.paramC, t.dfC)
    t.getDeclarations foreach {d =>
      tbar.add(d)
    }
    thys ::= tbar
    val views = modules collect {
      case v : View if v.to == t.toTerm => v
    }
    views foreach { v=>
      val s = v.from
      implicit val rules = makeRules(v)
      modules collect {
        case sprime : Theory if memory.content.visible(sprime.toTerm).toSet.contains(s) =>
          val tvw = new Theory(t.parent, sprime.name / v.name, t.meta, t.paramC, t.dfC)
          sprime.getDeclarations foreach {
            case c : Constant => tvw.add(rewrite(c, v.path, tbar.path, t.getInnerContext))
            case _ => //nothing for now //TODO handle structures
          }
          thys ::= tvw
      }

    }

    thys
  }


  private def makeRules(v : View) : HashMap[Path, Term] = {
    val path = v.from.toMPath
    var rules = new HashMap[Path,Term]
    val decl = v.getDeclarations

    v.getDeclarations foreach {
      case c : Constant =>
        c.df.foreach {t =>
          rules += (path ? c.name -> t)
        }
      case s : Structure => s.df.foreach {df =>
        try {
          controller.get(df.toMPath) match {
            case v : View => rules ++= makeRules(v)
            case _ => //nothing to do
          }
        } catch {
          case e : Error => // println(e)//nothing to do
          case e : Exception => // println(e)//nothing to do
        }
      }
    }
    rules
  }

  private def rewrite(d : Declaration, vpath : MPath, newhome : MPath, context : Context)(implicit rules : HashMap[Path, Term]) : Declaration = {
      val tl = new UniformTranslator {
        def apply(c: Context, t: Term) = apply(c, rewrite(t))
      }
      val dT = d.translate(OMMOD(newhome), LocalName(vpath.toPath) / d.home.toMPath.toPath, tl, context)
      dT.setOrigin(ByStructureSimplifier(d.home, OMID(vpath)))
      dT
  }


  private def rewrite(t : Term)(implicit rules : HashMap[Path, Term]) : Term = {
    t match {
    case OMID(p) =>
      if (rules.isDefinedAt(p)) rules(p) else t
    case OMA(f, args) => OMA(rewrite(f), args.map(rewrite))
    case OMBINDC(b, con, bodies) => OMBINDC(rewrite(b), rewrite(con), bodies.map(rewrite))
    case _ => t
  }}

  private def rewrite(con : Context)(implicit rules : HashMap[Path, Term]) : Context = {
    con.mapTerms {case (_,t) => rewrite(t)}
  }
}
