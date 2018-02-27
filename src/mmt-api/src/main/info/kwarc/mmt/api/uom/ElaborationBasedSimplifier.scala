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

/** used by [[MMTStructureSimplifier]] */
case class ByStructureSimplifier(home: Term, view: Term) extends Origin

/**
 * if set, the element has been elaborated
 * None: no information
 * Some(x): has been elaborated
 *   for container elements: x is false if the body has been elaborated only partially
 */
object ElaboratedElement extends ClientProperty[StructuralElement,Option[Boolean]](utils.mmt.baseURI / "clientProperties" / "controller" / "elaborated") {
  def isPartially(t : StructuralElement) : Boolean = get(t).getOrElse(None).isDefined
  def isFully(t : StructuralElement) : Boolean = get(t).contains(Some(true))
  def setPartially(t: StructuralElement) {
    put(t, Some(false))
  }
  def setFully(t : StructuralElement) = put(t,Some(true))
}


/**
 * the primary class for all flattening, materialization, enriching of theories
 *
 * code in [[Closer]] should be merged into here
 */
class ElaborationBasedSimplifier(oS: uom.ObjectSimplifier) extends Simplifier(oS) with ChangeListener {
  private lazy val memory = controller.memory
  private lazy val lup = controller.globalLookup

  override def logPrefix = "structure-simplifier"

  // we elaborate only inside modules; elaboration of declarations changes the parent; no elaboration inside structures and derived declarations
  def apply(s: StructuralElement) {
    if (ElaboratedElement.isFully(s))
      return
    log("simplifying " + s.path)
    applyElementBegin(s)
    s match {
      case m: ContainerElement[_] =>
        applyElementEnd(m)
      case _ =>
    }
  }
  
  def applyElementBegin(s: StructuralElement) {
    if (ElaboratedElement.isFully(s))
      return
    s match {
      case m: DeclaredModule =>
        log("flattening " + m.path)
        val rules = RuleSet.collectRules(controller, m.getInnerContext)
        if (!ElaboratedElement.isPartially(s)) {
          flattenDefinition(m, Some(rules))
          m match {
            case t: DeclaredTheory =>
              t.meta foreach apply
            case v: DeclaredView =>
              apply(materialize(Context.empty,v.from,true,None))
              apply(materialize(Context.empty,v.to,true,None))
            case _ =>
          }
          // TODO flattenContext(m.parameters)
        }
        m.getDeclarations.foreach {d => flattenDeclaration(m, d, Some(rules))}
      case t: DefinedTheory =>
         val context = t.superModule match {
           case None => Context.empty
           case Some(smP) => controller.get(smP) match {
             case sm: DeclaredModule => sm.getInnerContext
             case sm: DefinedModule => Context.empty // should never occur
           }
         }
         val body = materialize(context,t.df,expandDefs = false,Some(t.path))
      case d: DefinedModule => // TODO materialize
      case nm: NestedModule =>
        applyElementBegin(nm.module)
      case d: Declaration =>
        d match {
          case _:ContainerElement[_] =>
            // structures, derived declarations etc. are flattened in applyElementEnd, i.e., when they are complete
          case d =>
            flattenDeclaration(d)
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

  def applyElementEnd(s: ContainerElement[_]) {
    s match {
      case m: DeclaredModule =>
      case d: Declaration =>
        flattenDeclaration(d)
      case _ =>
    }
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
  private def flattenDefinition(mod: DeclaredModule, rulesOpt: Option[RuleSet] = None) {
    lazy val rules = rulesOpt.getOrElse {
      RuleSet.collectRules(controller, mod.getInnerContext)
    }
    mod match {
      case v: DeclaredView => return //TODO
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

  /** convenience entry method */
  private def flattenDeclaration(dOrig: Declaration) {
    dOrig.home match {
       case OMMOD(p) =>
         controller.globalLookup.get(p) match {
           case mod: DeclaredModule =>
             flattenDeclaration(mod, dOrig, None)
           case _ =>
             // we don't elaborate inside structures, derived declarations
         }
       case _ => // TODO materialize
    }
  }

  /** adds elaboration of d to parent */
  private def flattenDeclaration(parent: DeclaredModule, dOrig: Declaration, rulesOpt: Option[RuleSet] = None) {
    if (ElaboratedElement.isFully(dOrig))
      return
    lazy val rules = rulesOpt.getOrElse {
      RuleSet.collectRules(controller, parent.getInnerContext)
    }
    val dElab: List[Declaration] = (parent, dOrig) match {
      // plain includes: copy (only) includes (i.e., transitive closure of includes)
      case (thy: DeclaredTheory, Include(_, from, fromArgs)) =>
        val alreadyIncluded = thy.getAllIncludes 
        val fromThy = lup.getAs(classOf[Theory], from)
        apply(fromThy)
        fromThy match {
          case d: DefinedTheory =>
            Nil//TODO (deprecated anyway)
          case fromThy: DeclaredTheory =>
            val fromIncls = fromThy.getAllIncludes
            fromIncls.flatMap {case (p, pArgs) =>
              // p --pArgs--> fromThy --fromArgs--> parent
              // compose the two includes
              val fromArgsSub = (fromThy.parameters / fromArgs).getOrElse(return)
              val pArgsS = pArgs map {a => a ^? fromArgsSub}
              val cont = thy.getInnerContext
              val pArgsSN = pArgsS map {a => oS(a, cont, rules)}
              lazy val composedInclude = Include(thy.toTerm, p, pArgsSN)
              utils.listmap(alreadyIncluded, p) match {
                // if an include of p already exists, we have to check equality
                case Some(existingArgs) =>
                  val existingArgsN = existingArgs map {a => oS(a, cont, rules)}
                  if (pArgsSN != existingArgsN) {
                    val fromStr = controller.presenter.asString(OMPMOD(p,existingArgs))
                    val pSStr = controller.presenter.asString(OMPMOD(p,pArgsSN))
                    throw InvalidElement(dOrig, s"parametric theory included twice with different arguments: $fromStr != $pSStr") 
                  } else
                    Nil
                // otherwise, we add the new include
                case None =>
                  List(composedInclude)
              }
            }
        }
      // any other import (including includes of complex theories): copy and translate all declarations
      case (thy: DeclaredTheory, s: Structure) =>
        /* @param incl a theory reflexive-transitively included into s.from
         * @param prefix the prefix to use for declarations from that theory (None for s.from itself) 
         */
        def doDeclsInIncludedTheory(incl: Term, prefix: Option[LocalName]): List[Declaration] = {
          val dom = materialize(Context(parent.path),incl,true,None).asInstanceOf[DeclaredTheory]
          apply(dom)
          dom.getDeclarations.flatMap {
            case PlainInclude(i,_) =>
              // because includes are already flattened transitively, we only have to recurse one level
              if (prefix.isEmpty) {
                val si = lup.getAs(classOf[Declaration], parent.path ? s.name / ComplexStep(i))
                si :: doDeclsInIncludedTheory(OMMOD(i),Some(LocalName(i)))
              } else
                Nil
            case d: Declaration =>
              val name = prefix match {
                case Some(p) => s.name / p / d.name
                case _ => s.name / d.name
              }
              if (s.name.toString == "mult" && name.toString.endsWith("comp"))
                  true
              val sd = lup.getAs(classOf[Declaration], parent.path ? name)
              List(sd)
          }
        }
        val sElab = doDeclsInIncludedTheory(s.from, None)
        // because s.from and its includes were already flattened recursively, we must avoid recursively elaborating the declarations in sElab
        sElab foreach {d => ElaboratedElement.setFully(d)}
        sElab
      // derived declarations: elaborate
      case (thy: DeclaredTheory, dd: DerivedDeclaration) =>
         controller.extman.get(classOf[StructuralFeature], dd.feature) match {
           case None => Nil
           case Some(sf) =>
              val elab = sf.elaborate(parent, dd)
              dd.module.setOrigin(GeneratedBy(dd.path))
              val simp = oS.toTranslator(rules)
             /*
             val checker = controller.extman.get(classOf[Checker], "mmt").getOrElse {
               throw GeneralError(s"no mmt checker found")
             }.asInstanceOf[MMTStructureChecker]
             var cont = checker.elabContext(parent)(new CheckingEnvironment(new ErrorLogger(report), RelationHandler.ignore,new MMTTask{}))
              */
             val cont = elaborateContext(Context.empty,parent.getInnerContext)
              elab.getDeclarations.map {d =>
                //println(d)
                val dS = d.translate(simp,cont)
                dS
              }
         }
      /* includes in views TODO this is a first step that only covers the case where the target of the assignment is another DeclaredView
         more generally, ds is of the form 'n : FROM = TARGET', and its elaboration is the list of assignments
          'n = OMM(n,TARGET)' for all constants n
          's = OMCOMP(s, TARGET)' for all structures s
         in the domain of FROM
         
         Alternatively, all declarations d vw.from can simply be translated by looking up vw.path ? d.name.
         But that would overlap with the existing declarations in vw.

         The treatment of derived declarations and nested modules in FROM has not been specified yet.
      */
      case (vw: DeclaredView, ds : DefinedStructure) =>
        ds.df match {
          case OMMOD(mp) =>
            lup.getO(mp) match {
              case Some(v : DeclaredView) =>
                // include!
                apply(v)
                v.getDeclarations.map {
                  case s : DefinedStructure =>
                    DefinedStructure(parent.toTerm,s.name,s.from,s.df,false)
                  case c : Constant =>
                    Constant(parent.toTerm,c.name,c.alias,c.tp,c.df,c.rl,c.notC)
                }
              case _ => Nil
            }
          case _ => Nil
        }
      case (_, nm: NestedModule) =>
        apply(nm.module)
        Nil
      case _ =>
        Nil
    }
    dElab.reverseMap {e =>
       e.setOrigin(ElaborationOf(dOrig.path))
       log("flattening yields " + e.path)
       controller.add(e, After(dOrig.name))
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
           case v: DeclaredView =>
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
   *  @param expandDefs materialize all DefinedTheory's and return a DeclaredTheory
   *  @param pathOpt the path to use if a new theory has to be created
   */
  def materialize(context: Context, exp: Term, expandDefs: Boolean, pathOpt: Option[MPath]): Theory = {
    exp match {
      case OMMOD(p: MPath) => lup.getTheory(p) match {
         case d: DefinedTheory if expandDefs => materialize(context, d.df, expandDefs, None)
         case dt : DeclaredTheory =>
           dt.getDeclarationsElaborated
           apply(dt)
           dt
         case d => d
      }
      case OMPMOD(p, args) => // materialization of instances of parametric theories
         val t = lup.getTheory(p).asInstanceOf[DeclaredTheory]
         apply(t)
         val con = Context(t.parameters.indices.map(i => t.parameters(i).copy(df = Some(args(i)))):_*)
         //new InstantiatedTheory(t, args)
         val ret = new DeclaredTheory(p.doc,p.name,t.meta,ContextContainer(con),TermContainer(exp))
         t.getDeclarations.map(d => lup.get(exp,LocalName(d.parent) / d.name,s => throw GetError(s))).foreach(d => ret.add(d))
         ret
      case _ => // create a new theory and return it
        val path = pathOpt.getOrElse(newName)
        /*
        if (exp.freeVars.nonEmpty)
          throw GeneralError("materialization of module with free variables not implemented yet (" + exp.freeVars.mkString(", ") + ") in " +
          controller.presenter.asString(exp)) */
        val thy = new DeclaredTheory(path.parent, path.name, noMeta, noParams, TermContainer(exp))
        flattenDefinition(thy)
        thy.setOrigin(Materialized(exp))
        apply(thy)
        thy
    }
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
