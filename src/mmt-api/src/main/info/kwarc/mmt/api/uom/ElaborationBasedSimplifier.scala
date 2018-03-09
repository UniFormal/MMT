package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import frontend._
import checking._
import utils._
import modules._
import symbols._
import patterns._
import objects._
import notations._

import utils.MyList.fromList
import Theory._

import collection.immutable.{HashMap, HashSet}
import scala.util.{Success, Try}
import info.kwarc.mmt.api.libraries.AlreadyDefined
import sun.management.MappedMXBeanType.InProgress

/** used by [[MMTStructureSimplifier]] */
@deprecated("needs review","")
case class ByStructureSimplifier(home: Term, view: Term) extends Origin

/**
 * information about elaboration status
 * 0 or absent: no information, assume not elaborated
 * 2: partially: header has been elaborated, some elements in body (e.g., new elements) body must still be elaborated
 * 3: fully: applyElementEnd has been called
 * -1: in progress, apply or applyElementBegin has been called
 * -2: in progress and header already elaborated 
 */
//TODO mark InProgress to avoid infinite loops
// TODO rewrite getDeclarationsElaborated; that method needs to know which declarations can be skipped because their external declarations are present
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

  override def logPrefix = "structure-simplifier"

  // for efficiency, we use variants where the parent and its rules are already known; the main interface methods just defer to them
  def applyChecked(s: StructuralElement)(implicit env: SimplificationEnvironment) {applyWithParent(s, None, None)}
  def applyElementBegin(s: StructuralElement)(implicit env: SimplificationEnvironment) {applyElementBeginWithParent(s, None, None)}
  
  // internal and external flattening of s
  // equivalent to calling applyElementBegin and (if applicable) applyElementEnd
  private def applyWithParent(s: StructuralElement, parentO: Option[Body], rulesO: Option[RuleSet])(implicit env: SimplificationEnvironment) {
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
  private def applyElementBeginWithParent(s: StructuralElement, parentO: Option[Body], rulesO: Option[RuleSet])(implicit env: SimplificationEnvironment) {
    if (ElaboratedElement.isInprogress(s) || ElaboratedElement.isFully(s))
      return
    log("flattening " + s.path)
    ElaboratedElement.setInprogress(s)
    // internal flattening
    s match {
      case Include(_) =>
        // no need to flatten inside an include (this case is needed so that the next case can handle declared modules and strucutres together)
      case m: Body =>
        // flatten header and call flattenDeclaration on every child
        val rules = RuleSet.collectRules(controller, m.getInnerContext)
        if (!ElaboratedElement.isPartially(s)) {
          flattenDefinition(m, Some(rules))
          m match {
            case t: DeclaredTheory =>
              t.meta foreach apply
            case v: DeclaredLink =>
              applyChecked(materialize(Context.empty,v.from,None, Some(v.fromC)))
              v match {
                case v:DeclaredView => applyChecked(materialize(Context.empty,v.to,None,Some(v.toC)))
                case _: DeclaredStructure =>
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
      case d: DefinedModule => // TODO materialize
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
    }
  }

  def applyElementEnd(s: ContainerElement[_])(implicit env: SimplificationEnvironment) {
    if (ElaboratedElement.isFully(s))
      return
    log("finalize flattening of " + s.path)
    s match {
      case m: DeclaredModule =>
      case d: Declaration =>
        // external flattening of structures, declared declarations
        flattenExternally(d, None, None)
      case _ =>
    }
    log("done flattening " + s.path)
    ElaboratedElement.setFully(s)
  }

  @deprecated("needs to be reviewed","")
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
  private def flattenDefinition(mod: Body, rulesOpt: Option[RuleSet] = None) {
    lazy val rules = rulesOpt.getOrElse {
      RuleSet.collectRules(controller, mod.getInnerContext)
    }
    mod match {
      case v: DeclaredLink => return //TODO
      case thy: DeclaredTheory =>
        val at = new RepeatedAdd(AtBegin)
        def add(d: Declaration) {
          d.setOrigin(ElaborationOfDefinition)
          controller.add(d, at.getNextFor(d))
          log("flattening yields " + d.path)
        }
        var previous: Option[LocalName] = None
        thy.dfC.normalize {d => objectLevel(d, mod.getInnerContext, rules)}
        thy.dfC.normalized.foreach {dfS =>
          //TODO mod.getInnerContext is too small for nested theories
          dfS match {
            case ComplexTheory(cont) =>
              cont.asDeclarations(mod.toTerm).foreach {d =>
                add(d)
              }
            case AnonymousTheory(mt,omls) =>
              if (mt.isDefined) {
                val mtTerm = OMMOD(mt.get)
                // awkward: library must be explicitly notified about update of meta-theory because changes to meta-theory cannot go through controller.add
                thy.metaC.analyzed = Some(mtTerm)
                controller.memory.content.addImplicit(mtTerm, thy.toTerm, OMIDENT(mtTerm))
              }
              var translations = Substitution() // replace all OML's with corresponding OMS's
              // TODO this replaces too many OML's if OML-shadowing occurs
              def translate(tm: Term) = (new OMLReplacer(translations)).apply(tm, Context.empty)
              omls foreach {o =>
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
        }
    }
  }

  /** adds elaboration of d to parent
   *  for efficiency, parent and its rules may be passed if they are already known
   *  
   *  this method recurses into apply for dependency closure
   */
  private def flattenExternally(dOrig: Declaration, parentO: Option[Body], rulesO: Option[RuleSet])(implicit env: SimplificationEnvironment) {
    if (ElaboratedElement.isFully(dOrig))
      return
    val parent = parentO getOrElse {
      dOrig.home match {
       case OMID(p) =>
         val par = controller.globalLookup.get(p)
         par match {
           case par: Body => par
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
      fromThy match {
        case d: DefinedTheory =>
          Nil//TODO (deprecated anyway)
        case fromThy: DeclaredTheory =>
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
            val newMorN = oS(newMor1, innerCont, rules)
            val newInclude = parent match {
              case thy: DeclaredTheory =>
                val newArgs = newMorN match {
                  case OMCOMP(Nil) | OMIDENT(_) => Nil
                  case OMINST(_,nas) => nas
                  case _ => throw ImplementationError("composition of includes must yield include")
                }
                Include(parent.toTerm, p, newArgs)
              case _: DeclaredLink =>
                LinkInclude(parent.toTerm, p, newMorN)
            }
            ElaboratedElement.setFully(newInclude) // recursive elaboration already handled by recursively elaborating fromThy
            utils.listmap(alreadyIncluded, p) match {
              // if an include for domain p already exists, we have to check equality
              case Some(existingMor) =>
                val existingMorN = oS(existingMor, innerCont, rules)
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
    }
    var addAfter = true
    val dElab: List[Declaration] = (parent, dOrig) match {
      // ************ includes
      case (thy: DeclaredTheory, Include(_, from, fromArgs)) =>
        addAfter = false // generated includes are placed before the generating include so that they occur in dependency order
        // plain includes: copy (only) includes (i.e., transitive closure of includes)
        // from.meta is treated like any other include into from (in particular: skipped if from.meta included into thy.meta)
        val mor = OMINST(from,fromArgs)
        val alreadyIncluded = thy.getAllIncludes.map {case (p,as) => (p,OMINST(p,as))}
        flattenInclude(from, mor, alreadyIncluded)
      case (link: DeclaredLink, LinkInclude(_, from, mor)) =>
        // includes in views are treated very similarly; we compose mor with includes into from and check equality of duplicates
        // from.meta is treated like any other include into from (in particular: mor should include the intended meta-morphism out of from.meta)
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
            val t = lup.getAs(classOf[DeclaredTheory], p)
            t
          case exp =>
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
          val (pThy,refl) = if (p == fromThy.path) (fromThy,true) else (lup.getAs(classOf[DeclaredTheory], p), false)
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
            case Include(_) =>
              // because pThy is already flattened transitively, we do not have to do a transitive closure
              Nil
            case d: Declaration =>
              val sdname = prefix / d.name
              val sd = lup.getAs(classOf[Declaration], parentMPath ? sdname)
              List(sd)
          }
          structure ::: decls
        }
        // TODO in links we have to check equality in case of overlap with more specific instantiations; currently that yields AddErrors
        sElab foreach {d => ElaboratedElement.setFully(d)} // recursive elaboration already handled by recursively elaborating fromThy
        sElab
      // ************** derived declarations: elaborate
      case (thy: DeclaredTheory, dd: DerivedDeclaration) =>
         controller.extman.get(classOf[StructuralFeature], dd.feature) match {
           case None => Nil
           case Some(sf) =>
              val elab = sf.elaborate(thy, dd)
              dd.module.setOrigin(GeneratedBy(dd.path))
              val simp = oS.toTranslator(rules)
             /*
             val checker = controller.extman.get(classOf[Checker], "mmt").getOrElse {
               throw GeneralError(s"no mmt checker found")
             }.asInstanceOf[MMTStructureChecker]
             var cont = checker.elabContext(parent)(new CheckingEnvironment(new ErrorLogger(report), RelationHandler.ignore,new MMTTask{}))
              */
             val contE = elaborateContext(Context.empty,innerCont)
              elab.getDeclarations.map {d =>
                //println(d)
                val dS = d.translate(simp,contE)
                dS
              }
         }
      // the treatment of derived declarations in links has not been specified yet
      case (link: DeclaredLink, dd: DerivedDeclaration) =>
        Nil //TODO 
      case (_, nm: NestedModule) =>
        // nested module do not affect the semantics of their parent except when they are used, i.e., no external elaboration
        Nil
      case (_,_: Constant) =>
        // base case: no elaboration
        Nil
      case (_,_: RuleConstant) =>
        // base case: no elaboration
        Nil
    }
    // if we add after dOrig, we have to reverse dElab to make sure the declarations occur in the same order as in dElab
    val atFirst = if (addAfter) After(dOrig.name) else Before(dOrig.name)
    val pos = new RepeatedAdd(atFirst)
    dElab.foreach {e =>
      e.setOrigin(ElaborationOf(dOrig.path))
      val at = pos.getNextFor(e)
      log("flattening of " + dOrig.name + " yields " + e + " at " + at)
      controller.add(e, at)
    }
    ElaboratedElement.setFully(dOrig)
 }
 
 // TODO change management does not propagate to other theories yet

  /** deletes all declarations that were added by elaborating se */
  override def onDelete(se: StructuralElement) {
     if (! ElaboratedElement.isPartially(se))
       return
     se match {
       case d: Declaration =>
         controller.get(d.home.toMPath) match {
           case thy: DeclaredTheory =>
             thy.getDeclarations.foreach {e =>
               if (e.getOrigin == ElaborationOf(d.path))
                 thy.delete(e.name)
             }
           case v: DeclaredLink =>
             v.getDeclarations.foreach {e =>
               if (e.getOrigin == ElaborationOf(d.path))
                 v.delete(e.name)
             }
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
      case _: DeclaredStructure | _: DerivedDeclaration =>
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
  def materialize(context: Context, exp: Term, pathOpt: Option[MPath], tcOpt: Option[TermContainer]): DeclaredTheory = {
    // return previously materialized theory
    tcOpt foreach {tc =>
      tc.normalized foreach {tcN =>
        return materialize(context, tcN, pathOpt, None)
      }
    }
    val (dt, isNew) = exp match {
      case OMMOD(p: MPath) => lup.getTheory(p) match {
        case d: DefinedTheory =>
          return materialize(context, d.df, pathOpt, tcOpt)
        case d: DeclaredTheory =>
          (d, false)
      }
      case OMPMOD(p, args) =>
        //TODO DefinedTheory (deprecated anyway)
        val t = lup.getTheory(p).asInstanceOf[DeclaredTheory]
        val path = pathOpt.getOrElse(newName)
        val sub = (t.parameters / args).getOrElse {throw InvalidObject(exp, "wrong number of arguments")}
        val transl = new ApplySubs(sub)
        val ret = new DeclaredTheory(path.doc, path.name, t.meta, Theory.noParams, Theory.noBase)
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
        val ret = new DeclaredTheory(path.parent, path.name, noMeta, noParams, TermContainer(exp))
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
     case OMMOD(p) => lup.getTheory(p) match {
       case m: DefinedModule => getBody(context, m.df)
       case m: DeclaredModule => m
     }
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
  def enrich(t : DeclaredTheory) : DeclaredTheory =  {
    loadAll
    val tbar = new DeclaredTheory(t.parent, t.name, t.meta, t.paramC, t.dfC)
    t.getDeclarations foreach {d =>
      tbar.add(d)
    }
    val views = modules collect {
      case v : DeclaredView if v.to == t.toTerm => v
    } // all views to T

    views foreach { v =>
      val s = v.from
      implicit val rules = makeRules(v)
      modules collect {
        case sprime : DeclaredTheory if memory.content.visible(sprime.toTerm).toSet.contains(s) =>
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
  def enrichFineGrained(t : DeclaredTheory) : List[DeclaredTheory] = {
    loadAll
    var thys : List[DeclaredTheory] = Nil
    val tbar = new DeclaredTheory(t.parent, t.name, t.meta, t.paramC, t.dfC)
    t.getDeclarations foreach {d =>
      tbar.add(d)
    }
    thys ::= tbar
    val views = modules collect {
      case v : DeclaredView if v.to == t.toTerm => v
    }
    views foreach { v=>
      val s = v.from
      implicit val rules = makeRules(v)
      modules collect {
        case sprime : DeclaredTheory if memory.content.visible(sprime.toTerm).toSet.contains(s) =>
          val tvw = new DeclaredTheory(t.parent, sprime.name / v.name, t.meta, t.paramC, t.dfC)
          sprime.getDeclarations foreach {
            case c : Constant => tvw.add(rewrite(c, v.path, tbar.path, t.getInnerContext))
            case _ => //nothing for now //TODO handle structures
          }
          thys ::= tvw
      }

    }

    thys
  }


  private def makeRules(v : DeclaredView) : HashMap[Path, Term] = {
    val path = v.from.toMPath
    var rules = new HashMap[Path,Term]
    val decl = v.getDeclarations

    v.getDeclarations foreach {
      case c : Constant =>
        c.df.foreach {t =>
          rules += (path ? c.name -> t)
        }
      case d : DefinedStructure =>
        try {
          controller.get(d.df.toMPath) match {
            case d : DeclaredView => rules ++= makeRules(d)
            case x => //nothing to do
          }
        } catch {
          case e : Error => // println(e)//nothing to do
          case e : Exception => // println(e)//nothing to do
        }
    }
    rules
  }

  private def rewrite(d : Declaration, vpath : MPath, newhome : MPath, context : Context)(implicit rules : HashMap[Path, Term]) : Declaration = d match {
    case c : Constant =>
      val newtpC = TermContainer(c.tp.map(t => controller.simplifier.apply(rewrite(t), context)))
      val newdfC = TermContainer(c.df.map(rewrite))
      val newname = LocalName(vpath.toPath) / c.home.toMPath.toPath / c.name
      val newCons = new FinalConstant(OMMOD(newhome), newname, c.alias, newtpC, newdfC, c.rl, c.notC)
      newCons.setOrigin(ByStructureSimplifier(c.home, OMID(vpath)))
      newCons
    case x => x
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
