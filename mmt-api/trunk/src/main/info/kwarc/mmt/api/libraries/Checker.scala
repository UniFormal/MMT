package info.kwarc.mmt.api.libraries
import info.kwarc.mmt.api._
import documents._
import modules._
import symbols._
import objects._
import patterns._
import ontology._
import utils._
import moc._
import frontend._

import utils.MyList.fromList
import objects.Conversions._

/**
 * A StructureChecker traverses structural elements and checks them structurally.
 * 
 * Deriving classes may override unitCont and reCont to customize the behavior.
 */
  //Note: Even Library.addUnchecked checks that declarations can only be added to atomic declared theories/views.
  //Therefore, the home modules are not checked here.
  //Body.add checks that no two declarations of the same name exist. Therefore, names are not checked here.
  //Names that are prefixes of other names in the same body are permitted.
  //  This works well for links, for theories it is questionable. Refusing declaration with non-primitive names might be forbidden.
  //  When retrieving, more specific entries overrule the more general ones.
class StructureChecker(controller: Controller) extends Logger {
   private val extman = controller.extman
   private lazy val content = controller.globalLookup
   val report = controller.report
   val logPrefix = "checker"

   private var nrThys : Int = 0
   private var nrViews : Int = 0
   private var nrIncls : Int = 0
   private var nrDecls : Int = 0

   def printStatistics() = {
     println("nrThys : " + nrThys)
     println("nrViews : " + nrViews)
     println("nrIncls : " + nrIncls)
     println("nrDecls : " + nrDecls)
   }

  /** called on every validation unit, empty by default */
  def unitCont(vu: ValidationUnit) {}
  /** called on every relational element produced, empty by default */
  def reCont(re: RelationalElement) {}

  def errorCont(er: Invalid) {
     errors ::= er
  }
  private var errors: List[Invalid] = Nil
  
  /** checks a StructuralElement, given by its URI */
  def apply(p: Path): List[Invalid] = {
      apply(controller.get(p))
  }

   /** checks a StructuralElement
    * @param e the element to check
    */
   def apply(e : StructuralElement): List[Invalid] = {
      errors = Nil
      check(e)
      errors.reverse
   }
   private def check(p: Path) {
      check(controller.get(p))
   }
   private def check(e : StructuralElement) {
      val path = e.path
      log("checking " + path)
      implicit val rel = (p: Path) => reCont(RefersTo(path, p))
      implicit val lib = controller.globalLookup
      e match {
         case d: Document => d.getLocalItems foreach check
         case r: XRef => if (r.isGenerated) check(r.target)
         case t: DeclaredTheory =>
            nrThys += 1
            nrIncls += controller.memory.content.visible(OMMOD(t.path)).size
            nrDecls += t.getConstants.length
            t.meta map {mt =>
              checkTheory(OMMOD(mt))
            }
            t.getPrimitiveDeclarations foreach {
               d => check(d)
            }
         case t: DefinedTheory =>
            checkTheory(t.df)
         case v: DeclaredView =>
            nrViews += 1
            checkTheory(v.from)
            checkTheory(v.to)
            v.getPrimitiveDeclarations foreach check
         case v: DefinedView =>
            checkTheory(v.from)
            checkTheory(v.to)
            checkMorphism(v.df, v.from, v.to)
         case s: DeclaredStructure =>
            checkTheory(s.from)
            s.getPrimitiveDeclarations foreach check
         case s: DefinedStructure =>
            checkTheory(s.from)
            checkMorphism(s.df, s.from, s.home)
         case c : Constant =>
            val parR = checkContext(c.home, Context(), c.parameters)
            //TODO reconstruction in parameters
            c.tp foreach {t => 
               val (unknowns,tU) = parser.AbstractObjectParser.splitOffUnknowns(t)
               val (tR,valid) = checkTermTop(c.home, c.parameters ++ unknowns, tU)
               if (valid) {
                  OMMOD.unapply(c.home) foreach {p =>
                     val j = Universe(Stack(p, c.parameters), tR)
                     unitCont(ValidationUnit(c.path $ TypeComponent, unknowns, j))
                  }
               }
            }
            c.df foreach {d => 
               val (unknowns,dU) = parser.AbstractObjectParser.splitOffUnknowns(d)
               val (dR, valid) = checkTermTop(c.home, c.parameters ++ unknowns, dU)
               if (valid) {
                  OMMOD.unapply(c.home) foreach {p =>
                     c.tp foreach {tp =>
                         val j = Typing(Stack(p, c.parameters), dR, tp)
                         unitCont(ValidationUnit(c.path $ DefComponent, unknowns, j))
                     }
                  }
               }
            }
            // apply every applicable RoleHandler
            c.rl foreach {r =>
              controller.extman.getRoleHandler(r) foreach { h =>
                 h.apply(c)
              }
            }
            /*
            val foundation = extman.getFoundation(c.parent).getOrElse(throw Invalid("no foundation found for " + c.parent))
            c.tp map {t =>
               val trace = foundation.tracedTyping(None, Some(t))
               trace foreach {t => reCont(DependsOn(c.path $ "type", t))}
            }
            c.df map {t =>
               val trace = foundation.tracedTyping(c.df, c.tp)
               trace foreach {t => reCont(DependsOn(c.path $ "definition", t))}
            }
            */
         //TODO: compatibility of multiple assignments to the same knowledge item
         case a : ConstantAssignment =>
            val tlOpt = try {
               Some(content.getDomain(a))
            } catch { 
              case e: GetError =>
                 errorCont(InvalidElement(a,"invalid assignment",e))
                 None
            }
            tlOpt foreach {case (t,l) =>
               val c = content.getConstant(t.path ? a.name)
               a.target foreach {t => checkTermTop(l.to, Context(), t)}
            }
         case a : DefLinkAssignment =>
            val tlOpt = try {
               Some(content.getDomain(a))
            } catch { 
               case e: GetError =>
                  errorCont(InvalidElement(a,"invalid assignment",e))
                  None
            }
            tlOpt foreach {case (t,l) =>
               if (a.name.isAnonymous) {
                  checkMorphism(a.target, OMMOD(a.from), l.to)
               } else {
                  /* temporarily disabled because it fails when a meta-morphism is checked (lookup of the meta-theory as a structure fails)
                  val s = content.getStructure(t.path ? a.name)
                  if (s.fromPath != a.from) errorCont(InvalidElement(a, "import-assignment has bad domain: found " + a.from + " expected " + s.from)) 
                  checkMorphism(a.target, s.from, l.to)
                  */
               }
            }
         case p : Pattern =>
            checkContext(p.home, Context(), p.params)
            checkContext(p.home, p.params, p.body)
         case i : Instance => 
            val ptOpt = try {
               Some(content.getPattern(i.pattern))
            } catch {
               case e: GetError =>
                  errorCont(InvalidElement(i,"invalid pattern: " + i.pattern,e))
                  None
            }
            ptOpt foreach {pt => checkSubstitution(i.home, pt.getSubstitution(i), pt.params, Context())}
            //TODO mihnea's instances already refer to their elaboration in their matches
            
/*        case a : Alias =>
            mem.content.get(a.forpath)
            if (mem.content.imports(a.forpath.parent, a.parent))
               Success(List(IsAlias(a.path), IsAliasFor(a.path, a.forpath)))
            else
               Fail("illegal alias")
         case a : Open =>
            val str = mem.content.getStructure(a.parent)
            val source = try {mem.content.getSymbol(str.from ? a.name)}
                         catch {case _ => return Fail("open of non-existing constant")}
            val name = a.as.map(LocalName(_)).getOrElse(a.name)
            val al = new Alias(str.to, name, str.to ? str.name / source.name)
            Reconstructed(List(a, al), List(IsOpen(a.path)))
*/
         case _ => //succeed for everything else
      }
   }
   
/*   
   def check(cp: CPath) {
      content.getO(cp.parent) match {
        case None => throw Invalid("non-existing item: " + cp.parent)
        case Some(e) => e match {
           case c: Constant =>
              val foundation = extman.getFoundation(c.parent).getOrElse(throw Invalid("no foundation found for " + c.parent))
              cp.component match {
                 case "type" =>
                     try {
                       val trace = foundation.tracedTyping(None, c.tp)
                       trace foreach {t => ont += DependsOn(cp, t)}
                     } catch {
                       case e : Invalid => throw Invalid("Typing failed: invalid type for constant \"" + c.name + "\"")
                     }
                  case "definition" =>
                   try {
                     val trace = foundation.tracedTyping(c.df, c.tp)
                     if (c.df != None) {
                       trace foreach {t => ont += DependsOn(cp, t)}
                     }
                   } catch {
                     case e : Invalid => throw Invalid("Typing failed: invalid definition for constant \"" + c.name + "\"")
                   }
                 case c => throw Invalid("illegal component: " + c)
              }
           case a: ConstantAssignment =>
              val link = content.getLink(a.parent) 
              val foundation = extman.getFoundation(link.to.toMPath).getOrElse(throw Invalid("no foundation found for " + link.to)) //TODO: non-atomic codomain
              cp.component match {
                 case "type" => 
                 case "definition" =>
                     //println(link.from.toNode)
                     //println(link.to.toNode)
                     //println("t" + a.toString)
                    //if (! foundation.typing(Some(a.target), c.tp.map(_ * a.home))(mem.content))
                    //return ContentFail("assignment does not type-check")
                    //val defleq = c.df.isEmpty || a.target == OMHID() || foundation.equality(c.df.get, a.target)(mem.content)
                    //if (! defleq) return ContentFail("assignment violates definedness ordering")
                    try {
                      val ctp = con.getO(link.from.%(a.name)) match {
                        case Some(c : Constant) => c.tp.getOrElse(throw Invalid("Untyped Constant as base for morphism"))
                        case x => throw Invalid("link defined from non-constant")
                      }
                      val expTp = ctp * link.toMorph
                      val trace = foundation.tracedTyping(Some(a.target), Some(expTp))
                      trace foreach {t => ont += DependsOn(cp, t)}
                    }
                 case c => throw Invalid("illegal component: " + c)
              }
        }
      }
   }
*/
  /** checks whether a theory object is well-formed
    *  @param t the theory
    *  @return the reconstructed theory
    */
   def checkTheory(t : Term)(implicit pCont: Path => Unit) : Term = {
     t match {
        case OMMOD(p) =>
          val thy = controller.globalLookup.getTheory(p)
          pCont(p)
          t
        case OMS(mmt.tempty) => t
        case TheoryExp.Empty =>
           t
        case TUnion(ts) =>
           //TODO check same meta-theory?
           val tsR = ts map {t => checkTheory(t)}
           TUnion(tsR)
        case _ =>
           errorCont(InvalidObject(t, "not a valid theory"))
           t
      }
   }
   
   private case object NotInferrable extends java.lang.Throwable
  /** checks whether a morphism object is well-formed and infers its type
    *  @param m the theory
    *  @return the reconstructed morphism, its domain and codomain
    *    domain and codomain are null in case of errors 
    */
   def inferMorphism(m : Term)(implicit pCont: Path => Unit) : (Term, Term, Term) = try {
      inferMorphismAux(m)
   } catch {case NotInferrable => (m, null, null)}
   def inferMorphismAux(m : Term)(implicit pCont: Path => Unit) : (Term, Term, Term) = m match {
     case OMMOD(p) =>
        val l = controller.globalLookup.getLink(p)
        pCont(p)
        (m, l.from, l.to)
     case OMDL(to, name) =>
        checkTheory(to)
        content.get(to % name) match {
           case l: Structure =>
              (m, l.from, to)
           case _ =>
              errorCont(InvalidObject(m,"invalid morphism"))
              throw NotInferrable
        }
        // TODO pCont ??
     case OMIDENT(t) =>
        checkTheory(t)
        (m, t, t)
     case OMCOMP(Nil) =>
        errorCont(InvalidObject(m,"cannot infer type of empty composition"))
        throw NotInferrable
     case OMCOMP(hd::Nil) => inferMorphismAux(hd)
     case OMCOMP(hd :: tl) =>
        val (hdR, r, s1) = inferMorphismAux(hd)
        val (tlR, s2, t) = inferMorphismAux(OMCOMP(tl))
        content.getImplicit(s1,s2) match { 
           case Some(l) =>
              (OMCOMP(hdR, l, tlR), r, t)
           case None =>
              errorCont(InvalidObject(m, "ill-formed morphism: " + hd + " cannot be composed with " + tl))
              throw NotInferrable
        }
     case Morph.Empty =>
        errorCont(InvalidObject(m, "cannot infer type of empty morphism"))
        throw NotInferrable
     case MUnion(ms) =>
        val infs = ms map {m => inferMorphismAux(m)}
        val rms  = infs map {i => i._1}
        val doms = infs map {i => i._2}
        val cods = infs map {i => i._3}
        // TODO l and r agree on all joint domains
        val dom = TUnion(doms)
        val cod = TUnion(cods) //TODO: simplify by removing redundant theories in cods using mem.content.imports(_,_)
        (MUnion(rms), dom ,cod)
   }
   
   /** checks whether a morphism object is well-formed relative to a library, a domain and a codomain
    *  @param m the morphism
    *  @param dom the domain
    *  @param cod the codomain
    *  @return the reconstructed morphism (with necessary implicit morphisms inserted)
    */
   def checkMorphism(m : Term, dom : Term, cod : Term)(implicit pCont: Path => Unit) : Term = {
      val (l,d,c) = inferMorphism(m)
      if (d != null && c != null) {
        (content.getImplicit(dom, d), content.getImplicit(c, cod)) match {
           case (Some(l0), Some(l1)) => OMCOMP(l0, l, l1)
           case _ =>
             errorCont(InvalidObject(m, "ill-formed morphism: expected " + dom + " -> " + cod + ", found " + d + " -> " + c))
             m
        }
      } else 
         m
   }

   /**
    * Checks structural well-formedness of a closed term relative to a home theory.
    * @param home the home theory
    * @param s the term
    * @return the reconstructed term
    */
   def checkTermTop(home: Term, context : Context, s : Term)(implicit pCont: Path => Unit) : (Term, Boolean) = {
      val n = errors.length
      val t = checkTerm(home, context, s)
      if (errors.length == n)
         (t, true) // no new errors
      else
         (t, false)      
   }
   /**
    * Checks structural well-formedness of a term in context relative to a home theory.
    * @param home the home theory
    * @param s the term
    * @return the reconstructed term
    */
   private def checkTerm(home : Term, context : Context, s : Term)(implicit pCont: Path => Unit) : Term = {
      s match {
//         case OMID(GlobalName(h, IncludeStep(from) / ln)) =>
//            if (! content.imports(from, h))
//               throw Invalid(from + " is not imported into " + h + " in " + s)
//            checkTerm(home, context, OMID(from % ln).from(s))
         case OMS(path) =>
            val ceOpt = try {content.getO(path)}
                    catch {case e: GetError =>
                                errorCont(InvalidObject(s, "ill-formed constant reference"))
                    }
            ceOpt match {
               case Some(c : Constant) =>
                  if (! content.hasImplicit(c.home, home))
                     errorCont(InvalidObject(s, "constant " + c.path + " is not imported into home theory " + home))
               case _ => errorCont(InvalidObject(s, "path does not refer to constant"))
            }
            pCont(path)
            s
         case OMV(name) =>
            if (! context.isDeclared(name))
               errorCont(InvalidObject(s, "variable is not declared"))
            s
         case OMA(fun, args) =>
            val funR = checkTerm(home, context, fun)
            val argsR = args map {a => checkTerm(home, context, a)}
            OMA(funR, argsR).from(s)
         case OMBINDC(binder, bound, scopes) =>
            val binderR = checkTerm(home, context, binder)
            val boundR = checkContext(home, context, bound)
            val scopesR = scopes map {c => checkTerm(home, context ++ bound, c)}
            OMBINDC(binderR, boundR, scopesR).from(s)
         case OMATTR(arg, key, value) =>
            val argR = checkTerm(home, context, arg)
            checkTerm(home, context, key)
            val valueR = checkTerm(home, context, value)
            OMATTR(argR, key, valueR).from(s)
         case OMM(arg, morph) =>
            val (morphR, from, to) = inferMorphism(morph)
            if (from != null && to != null && ! content.hasImplicit(to, home)) {
               errorCont(InvalidObject(s, "codomain of morphism is not imported into expected home theory"))
               val argR = checkTerm(from, context, arg) // using the same context because variable attributions are ignored anyway
               OMM(argR, morph).from(s)
            } else s
         case OMHID => s//TODO
         case OME(err, args) =>
            val errR  = checkTerm(home, context, err)
            val argsR = args.map(checkTerm(home, context, _))
            OME(errR, argsR).from(s)
         case OMFOREIGN(node) => s //TODO
         case OMI(i) => s //TODO check if integers are permitted
         case OMSTR(str) => s //TODO check if strings are permitted
         case OMF(d) => s //TODO check if floats are permitted
         case OMSemiFormal(t) => s //TODO
         //case _ => s
      }
   }

   /**
    * Checks structural well-formedness of a context relative to a home theory and a context.
    * @param home the home theory
    * @param context
    * @param con the context
    * @return the reconstructed context
    * if context is valid, then this succeeds iff context ++ con is valid
    */
   def checkContext(home: Term, context: Context, con: Context)(implicit pCont: Path => Unit) : Context = {
      con.mapVarDecls {case (c, vd @ VarDecl(name, tp, df, attrs @_*)) =>
             val currentContext = context ++ c 
    	     val tpR = tp.map(x => checkTerm(home, currentContext, x)) 
    	     val dfR = df.map(x => checkTerm(home, currentContext, x)) 
    	     val vdR = VarDecl(name, tpR, dfR, attrs :_*) //TODO check attribution
    	     vdR.copyFrom(vd)
    	     vdR
      }
   }
   /**
    * Checks structural well-formedness of a substitution relative to a home theory.
    * @param home the home theory
    * @param subs the substitution
    * @param from the context declaring the variables to be substituted
    * @param to the context in which the substituting terms live
    * @return the reconstructed substitution
    */
   def checkSubstitution(home: Term, subs: Substitution, from: Context, to: Context)(implicit pCont: Path => Unit) : Substitution = {
      if (from.length != subs.length) {
         errorCont(InvalidObject(subs, "substitution has wrong number of cases for context " + from))
         subs
      } else {
         (from zip subs) map {       
       	  case (VarDecl(n,tp,df,attrs @ _*), Sub(m,t)) if n == m =>
       	     val tR = checkTerm(home,to,t)
       	     Sub(m,tR)
       	  case (v,s) =>
       	     errorCont(InvalidObject(subs, "illegal case " + s + " for declaration " + v))
       	     s
         }
      }
   }
}

class StructureAndObjectChecker(controller: Controller) extends StructureChecker(controller) {
   val vdt = new Validator(controller)
   override def unitCont(vu: ValidationUnit) {vdt.apply(vu)(errorCont)}
}