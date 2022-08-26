package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import frontend._
import utils._
import documents._
import modules._
import symbols._
import objects._
import notations._
import libraries.AlreadyDefined
import Theory._
import info.kwarc.mmt.api.parser.SourceRef

import collection.immutable.{HashMap}

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
  def setPartially(t: StructuralElement) = put(t, get(t).getOrElse(1).sign * 2)
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
  def applyChecked(s: StructuralElement)(implicit env: SimplificationEnvironment): Unit = {applyWithParent(s, None, None)}
  def applyElementBegin(s: StructuralElement)(implicit env: SimplificationEnvironment): Unit = {applyElementBeginWithParent(s, None, None)}
  
  // internal and external flattening of s
  // equivalent to calling applyElementBegin and (if applicable) applyElementEnd
  private def applyWithParent(s: StructuralElement, parentO: Option[ModuleOrLink], rulesO: Option[RuleSet])(implicit env: SimplificationEnvironment): Unit = {
    if (ElaboratedElement.isInprogress(s) || ElaboratedElement.isFully(s)) {
      return
    }
    applyElementBeginWithParent(s, parentO, rulesO)
    s match {
      case m: ContainerElement[_] =>
        applyElementEnd(m)
      case _ =>
    }
  }
  
  // internal and external flattening of s except for (in the case of container elements) those parts performed in applyElementEnd
  private def applyElementBeginWithParent(s: StructuralElement, parentO: Option[ModuleOrLink], rulesO: Option[RuleSet])(implicit env: SimplificationEnvironment): Unit = {
    if (ElaboratedElement.isInprogress(s) || ElaboratedElement.isFully(s))
      return
    log("flattening " + s.path)
    ElaboratedElement.setInprogress(s)
    // internal flattening
    s match {
      case Include(_) =>
        // no need to flatten inside an include (this case is needed so that the next case can handle declared modules and structures together)
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
                case v: View =>
                  v.to match {
                    case TUnion(ats) => ats.foreach {case (p,args) =>
                      // if the codomain is a union of atomic theories, we can handle them individually without materializing
                      val pT = controller.get(p)
                      applyChecked(pT)
                    }
                    case _ =>
                      applyChecked(materialize(Context.empty,v.to,None,Some(v.toC)))
                  }
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

  def applyElementEnd(s: ContainerElement[_])(implicit env: SimplificationEnvironment): Unit = {
    if (ElaboratedElement.isFully(s))
      return
    log("finalize flattening of " + s.path)
    s match {
      case dm: DerivedModule =>
        flattenDerivedModule(dm, None)
      case m: Module =>
      // case PlainInclude(_,_) =>
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
  private def flattenDefinition(mod: ModuleOrLink, rulesOpt: Option[RuleSet] = None): Unit = {
    lazy val rules = rulesOpt.getOrElse {
      RuleSet.collectRules(controller, mod.getInnerContext)
    }
    val at = new RepeatedAdd(AtBegin)
    def add(d: Declaration): Unit = {
      d.setOrigin(ElaborationOfDefinition)
      controller.add(d, at.getNextFor(d))
      log("flattening yields " + d.path)
    }
    mod.dfC.normalize {d =>
      val dS = objectLevel(d, SimplificationUnit(mod.getInnerContext, false,false, true), rules)
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
                thy.metaC.get foreach {old =>
                  SourceRef.get(old).foreach(r => SourceRef.update(mtTerm,r))
                }
                thy.metaC.analyzed = Some(mtTerm)
                controller.memory.content.update(thy) // update is redundant except for recomputing implicits
              }
              var translations = Substitution() // replace all OML's with corresponding OMS's
              // TODO this replaces too many OML's if OML-shadowing occurs
              def translate(tm: Term) = (OMLReplacer(translations)).apply(tm, Context.empty)
              at.decls foreach {o =>
                o match {
                  case IncludeOML(OMPMOD(mp,args), _ ) =>
                    val d = Include(thy.toTerm, mp, args)
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
  private def flattenDerivedModule(dm: DerivedModule, rulesO: Option[RuleSet])(implicit env: SimplificationEnvironment): Unit = {
     controller.extman.get(classOf[ModuleLevelFeature], dm.feature) match {
       case None => /*Nil*/
       case Some(sf) =>
          val elab = sf.modules(dm, rulesO, env)
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
  private def flattenExternally(dOrig: Declaration, parentO: Option[ModuleOrLink], rulesO: Option[RuleSet])(implicit env: SimplificationEnvironment): Unit = {
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
     * @param ID an include ID.from-->target declared in mod; for both theories and views, ID.from is included into source
     * @param alreadyIncluded list of other includes id.from --id--> target that are included into mod
     * @param target as above
     * @return the list of includes resulting from flattening (from,mor) 
     */
    def flattenInclude(ID: IncludeData, alreadyIncluded: List[IncludeData], target: Term)(implicit env: SimplificationEnvironment): List[Declaration] = {
      val fromThy = lup.getAs(classOf[AbstractTheory], ID.from)
      applyChecked(fromThy)
      val fromIncls = fromThy.getAllIncludes
      fromIncls.flatMap {id =>
        // id.from --id--> ID.from --ID--> target
        // we generate a new include: id.from --idID--> target by composition
        // id and in theories also ID need not be defined, .asMorphism may return OMINST or OMStructuralInclude
        // idID has a definiens if id or ID has
        // in theories, if id is an OMINST, idID is the morphism out of id.from that maps variable x_i to mor(id.args_i) and constant c to mor(c)
        // in views v:  ID.df = v|_ID.from where id.from --id--> ID.from --include--> source --v--> target
        // compose and simplify
        val newMor = OMCOMP(id.asMorphism, ID.asMorphism)
        val newMor1 = Morph.simplify(newMor)(lup)
        val newMorN = oS(newMor1, SimplificationUnit(innerCont, false,false, true), rules)
        // extract the new include data
        // if either include is defined, so is the composition; a realization id can be considered defined from outside the theory 
        val newDf = if (id.df.isDefined || id.isRealization || ID.df.isDefined) {
          Some(newMorN)
        } else {
          None
        }
        // if both id and ID are undefined and id has instantiation, than so is idID, and we produce an undefined include with arguments
        // np != id.from occurs id is not an instantiation but ID is
        val newArgs = if (newDf.isDefined) Nil else newMorN match {
            case OMINST(np, _, nas) if np == id.from =>
               nas
            case _ =>
              Nil
        }
        // the composed include is only postulated if ID is, i.e., it can only be used as a morphism once mod is closed
        // if id is defined or a realization, this will produce a defined realization
        val newPost = ID.isRealization
        val idID = IncludeData(parent.toTerm, id.from, newArgs, newDf, newPost)
        val idIDDecl = idID.toStructure
        ElaboratedElement.setFully(idIDDecl) // recursive elaboration already handled by recursively elaborating fromThy
        // if an include for id.from already exists in mod, idID is only allowed if it is redundant
        // i.e., it is either dropped or an error
        alreadyIncluded.find(_.from == idID.from) match {
          case Some(exid) =>
            /* TODO if id.df.isEmpty && exid.isRealization, the implicit morphism will be generated (by composition)
             * even if we id here. That will lead to an AlreadyDefined error when the realization is finally checked at the end of the theory.
             * That makes sense: a realization may only be used (e.g. here: to absorb an include) if it has been checked.
             * But it's unclear what the best design is to check the totality of the realization in time
             * (i.e., right now before the generated includes are added to the implicit-graph).
             */
            idID.df match {
              case Some(dfMor) =>
                // if idID has a definiens, we have to check equality with the existing include
                val existingMor = Morph.simplify(exid.asMorphism)(lup)
                val existingMorN = oS(existingMor,SimplificationUnit(innerCont,false,false,true),rules)
                val eq = Morph.equal(existingMorN,dfMor,OMMOD(exid.from),target)(lup)
                if (!eq) {
                  // otherwise, it is an error
                  val List(exStr,newStr) = List(existingMorN,newMorN) map {m => controller.presenter.asString(m)}
                  val parStr = parent.name.toString
                  val msg = if (inTheory) {
                    s"theory included twice into $parStr with different definitions or parameters"
                  } else {
                    s"two unequal morphisms included into $parStr for the same theory"
                  }
                  env.errorCont(InvalidElement(dOrig,s"$msg: $newStr != $exStr are the morphisms"))
                }
              case None =>
                // if idID has no definiens, we have almost nothing to check:
                // - idID is an include: idID is redundant because a more specific version of id.from has already been included
                // - idID is a realization: idID is redundant because it is already satisfied because of exid
                // The only exception is if exid is a realization (which may or may not be total at this point) and idID is not.
                // In that case, exid may not be used yet to justify dropping idID.
                // Otherwise, it  could lead to a cycle because later declarations in ID.from may have already used as of yet unrealized declarations of id.from.
                if (exid.isRealization && !idID.isRealization) {
                  val msg = s"conflict between plain include from ${idID.from}, which has previously been declared as a realization" +
                    " (If the realization is not total at this point, this could lead to a dependency cycle. If the realization is already total, this error can be avoided by closing the theory and including it into a new one.)"
                  env.errorCont(InvalidElement(dOrig,msg))
                }
            }
            Nil
          // otherwise, we add the new include
          case None =>
            List(idIDDecl)
        }
      }
    }
    var addAfter = true
    val dElab: List[Declaration] = (parent, dOrig) match {
      // ************ includes
      case (thy: Theory, Include(id)) =>
        // plain includes (possibly defined): copy (only) includes (i.e., transitive closure of includes), composing the definitions (if any)
        addAfter = id.isRealization // generated includes are placed before the generating include so that they occur in dependency order
        // from.meta is treated like any other include into from (in particular: skipped if from.meta included into thy.meta)
        // compute all includes into thy or any of its meta-theories
        val thyMetas = TheoryExp.metas(thy.toTerm)(lup).map(p => lup.getAs(classOf[Theory], p))
        val alreadyIncluded = (thy::thyMetas).reverse.flatMap(_.getAllIncludes)
        flattenInclude(id, alreadyIncluded, thy.toTerm)
      case (link: Link, Include(id)) =>
        // includes in views are treated very similarly; we compose mor with includes into from and check equality of duplicates
        // from.meta is treated like any other include into from (in particular: mor should include the intended meta-morphism out of from.meta)
        // TODO there is no need to flatten IdentityIncludes, transitive closure is enough; this is analogous to how we handle includes in theories
        val alreadyIncluded = link.getAllIncludes
        flattenInclude(id, alreadyIncluded, link.to)
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
            val t = lup.getAs(classOf[AbstractTheory], p)
            t
          case exp =>
            // TODO also materialize pushout if a nested theory is visible via an implicit morphism
            materialize(Context(parentMPath),exp,None, Some(struc.tpC))
        }
        applyChecked(fromThy)
        // copy all declarations in theories p reflexive-transitively included into fromThy
        // no need to consider the instantiation arguments or definitions because the lookup methods are used to obtain the declarations in the elaboration
        // TODO this generates declarations out of order if the includes of fromThy are not at the beginning
        val sElab = (fromThy.getAllIncludesWithoutMeta.map(_.from) ::: List(fromThy.modulePath)).flatMap {p =>
          val refl = p == fromThy.modulePath
          // in theories:
          //   !refl: pThy --OMINST/df--> fromThy --struc--> thy
          //   refl:  pThy == fromThy --struc--> thy
          // in views:
          //   !refl: pThy--OMINST/df--> fromThy --struc--> vw.from --vw--> vw.to with vw|_fromThy = struc.df
          //   refl:  pThy == fromThy --struc--> vw.from --vw--> vw.to with vw|_fromThy = struc.df
          val pThy = if (refl) fromThy else lup.getAs(classOf[AbstractTheory], p)
          // pThy is already elaborated at this point
          val prefix = if (refl) struc.name else struc.name / ComplexStep(p)
          // the defined structure with morphism pThy-->target that arises by composing the include with struc
          val structure = if (refl) {
            Nil // this would be struc itself
          } else {
            log("retrieving " + parentMPath ? prefix)
            val ds = lup.getAs(classOf[Declaration], parentMPath ? prefix)
            List(ds)
          }
          // copies of the local declarations of p
          val decls = pThy.getDeclarations.flatMap {
            case Include(_) =>
              // because pThy is already flattened transitively, we do not have to do a transitive closure
              Nil
            case nm: NestedModule => Nil // TODO compute pushout
            case d: Declaration =>
              if (skipWhenFlattening(d.getOrigin))
                Nil
              else if (d.name.head.isInstanceOf[ComplexStep]) {
                // definitions of realized constants must be skipped because they are already handled by the realized theory (which is part of the includes)
                Nil
              } else {
                val sdname = prefix / d.name
                try {
                  val sd = lup.getAs(classOf[Declaration], parentMPath ? sdname)
                  List(sd)
                } catch {case e: Error =>
                  val eS = InvalidElement(dOrig, "error while generating " + sdname).setCausedBy(e)
                  env.errorCont(eS)
                  Nil
                }
              }
          }
          structure ::: decls
        }
        // TODO in links we have to check equality in case of overlap with more specific instantiations; currently that yields AddErrors
        sElab foreach {d => ElaboratedElement.setFully(d)} // recursive elaboration already handled by recursively elaborating fromThy
        sElab
      // ************** derived declarations: elaborate
      case (thy: AbstractTheory, dd: DerivedDeclaration) =>
         controller.extman.get(classOf[StructuralFeature], dd.feature) match {
           case None => Nil
           case Some(sf) =>
             var i = 0
             while (dd.getDeclarations.isDefinedAt(i)) { // awkward, but necessary to elaborate generated things as well
               try { apply(dd.getDeclarations(i)) } catch {
                 case _:GetError =>
               }
               i+=1
             }
             val elab = sf.elaborate(thy, dd)(Some(ExtendedSimplificationEnvironment(env, this.objectLevel, rules)))
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
      e match {
        case ce: ContainerElement[_] => controller.endAdd(ce)
        case _ =>
      }
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
  override def onDelete(se: StructuralElement): Unit = {
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
  override def onUpdate(old: StructuralElement, nw: StructuralElement): Unit = {
    onDelete(old)
    if (ElaboratedElement.isPartially(old))
      apply(nw)
  }

  /** recursively mark fully elaborated parents to be only partially elaborated */
  private def markParentUnelaborated(c: StructuralElement): Unit = {
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
  override def onAdd(c: StructuralElement): Unit = {
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
      case GeneratedFrom(dp : GlobalName, _) =>
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
  def materialize(context: Context, exp: Term, pathOpt: Option[MPath], tcOpt: Option[TermContainer]): AbstractTheory = {
    // return previously materialized theory
    tcOpt foreach {tc =>
      tc.normalized foreach {tcN =>
        return materialize(context, tcN, pathOpt, None)
      }
    }
    val (dt, isNew) = exp match {
      case OMMOD(p: MPath) =>
        val d = lup.getAs(classOf[AbstractTheory],p)
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
      tcOpt.foreach {tc => tc.normalized = Some(OMMOD(dt.modulePath))}
    }
    dt
  }

  //TODO move to library if it's not doing anything else
  def getBody(context: Context, moduleExp: Term): ElementContainer[NamedElement] = moduleExp match {
     case OMMOD(p) => lup.getTheory(p)
     //TODO OMPMOD
     case ComplexTheory(cont) => cont
  }
}
