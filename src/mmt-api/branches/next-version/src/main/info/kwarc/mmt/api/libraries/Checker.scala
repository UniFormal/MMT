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
 * A Checker that checks only knowledge items whose well-formedness is foundation-independent.
 */
  //Note: Even Library.addUnchecked checks that declarations can only be added to atomic declared theories/views.
  //Therefore, the home modules are not checked here.
  //Body.add checks that no two declarations of the same name exist. Therefore, names are not checked here.
  //Names that are prefixes of other names in the same body are permitted.
  //  This works well for links, for theories it is questionable. Refusing declaration with non-primitive names might be forbidden.
  //  When retrieving, more specific entries overrule the more general ones.
class Checker(controller: Controller) {
   private val extman = controller.extman
   private lazy val content = controller.globalLookup
   private def log(msg: => String) {controller.report("checker", msg)}

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


  private def tryForeach[A](args: Iterable[A])(fun: A => Unit) {
      args foreach {a =>
        try {fun(a)}
        catch {case e @ Invalid(msg) => controller.report(e)}
      }
   }
   /** checks a StructuralElement
    * @param e the element to check
    * @param reCont a continuation called on all RelationalElement's produced while checking objects
    * @param unitCont a continuation called on every validation unit
    */ 
   //TODO: currently the reconstructed element is always e
   def check(e : StructuralElement)(implicit reCont : RelationalElement => Unit, unitCont: ValidationUnit => Unit) {
      val path = e.path
      log("checking " + path)
      implicit val rel = (p: Path) => reCont(RefersTo(path, p))
      implicit val lib = controller.globalLookup
      e match {
         case d: Document => tryForeach(d.getLocalItems) {
            i => check(i.target)
         }
         case t: DeclaredTheory =>
            nrThys += 1

            nrIncls += controller.memory.content.visible(OMMOD(t.path)).size

            //t.components collect {
            //  case s : Structure => nrIncls += 1
            //}

            t.components collect {
              case c : Constant => nrDecls += 1
            }

            t.meta map {mt =>
              checkTheory(OMMOD(mt))
            }
            tryForeach(t.valueListNG) {
               d => check(d)
            }
         case t: DefinedTheory =>
            checkTheory(t.df)
         case v: DeclaredView =>
            nrViews += 1
            checkTheory(v.from)
            checkTheory(v.to)

            tryForeach(v.valueListNG) {
               d => check(d)
            }
         case v: DefinedView =>
            checkTheory(v.from)
            checkTheory(v.to)
            checkMorphism(v.df, v.from, v.to)
         case s: DeclaredStructure =>
            checkTheory(s.from)
            tryForeach(s.valueListNG) {
               d => check(d)
            }
         case s: DefinedStructure =>
            checkTheory(s.from)
            checkMorphism(s.df, s.from, s.home)
         case c : Constant =>
            c.tp map {t => 
               checkTerm(c.home, t)
               OMMOD.unapply(c.home) foreach {p => unitCont(ValidationUnit(c.path $ TypeComponent, Universe(Stack(p), t)))}
            }
            c.df map {d => 
               checkTerm(c.home, d)
               OMMOD.unapply(c.home) foreach {p =>
                  c.tp foreach {tp => unitCont(ValidationUnit(c.path $ DefComponent, Typing(Stack(p), d, tp)))}
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
            val (t,l) = try {
               content.getDomain(a)
            } catch { 
              case e: GetError => throw Invalid("invalid assignment").setCausedBy(e)
            }
            val c = content.getConstant(t.path ? a.name)
            checkTerm(l.to, a.target)
         case a : DefLinkAssignment =>
            val (t,l) = try {
               content.getDomain(a)
            } catch { 
              case e: GetError => throw Invalid("invalid assignment").setCausedBy(e)
            }
            if (a.name.isAnonymous) {
               checkMorphism(a.target, a.from, l.to)
            } else {
               val s = content.getStructure(t.path ? a.name)
               if (s.from != a.from) throw Invalid("import-assignment has bad domain: found " + a.from + " expected " + s.from) 
               checkMorphism(a.target, s.from, l.to)
            }
         case p : Pattern =>
            checkContext(p.home, Context(), p.params)
            checkContext(p.home, p.params, p.body)
         case i : Instance => 
            val pt = try {
               content.getPattern(i.pattern)
            } catch {
               case e: GetError => throw Invalid("invalid pattern: " + i.pattern).setCausedBy(e)
            }
            //TODO mihnea's instances already refer to their elaboration in their matches
            checkSubstitution(i.home, i.matches, pt.params, Context())
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
   
   def check(p: Path)(implicit reCont : RelationalElement => Unit, unitCont: ValidationUnit => Unit) {
      check(controller.get(p))
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
        case _ => throw Invalid("not a valid theory " + t)
      }
   }
   
  /** checks whether a morphism object is well-formed and infers its type
    *  @param m the theory
    *  @return the reconstructed morphism, its domain and codomain
    */
   def inferMorphism(m : Term)(implicit pCont: Path => Unit) : (Term, Term, Term) = m match {
     case OMMOD(p) =>
        val l = controller.globalLookup.getLink(p)
        pCont(p)
        (m, l.from, l.to)
     case OMDL(to, name) =>
        checkTheory(to)
        val from = content.get(to % name) match {
           case l: Structure => l.from
           case _ => throw Invalid("invalid morphism " + m)
        }
        // TODO pCont ??
        (m, from, to)
     case OMIDENT(t) =>
        checkTheory(t)
        (m, t, t)
     case OMCOMP(Nil) => throw Invalid("cannot infer type of empty composition")
     case OMCOMP(hd::Nil) => inferMorphism(hd)
     case OMCOMP(hd :: tl) =>
        val (hdR, r, s1) = inferMorphism(hd)
        val (tlR, s2, t) = inferMorphism(OMCOMP(tl))
        val l = content.getImplicit(s1,s2) match { 
           case Some(l) => l
           case None => throw Invalid("ill-formed morphism: " + hd + " cannot be composed with " + tl)
        }
        (OMCOMP(hdR, l, tlR), r, t)
     case Morph.Empty => throw Invalid("cannot infer type of empty morphism")
     case MUnion(ms) =>
        val infs = ms map {m => inferMorphism(m)}
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
      (content.getImplicit(dom, d), content.getImplicit(c, cod)) match {
         case (Some(l0), Some(l1)) => OMCOMP(l0, l, l1)
         case _ => throw Invalid("ill-formed morphism: expected " + dom + " -> " + cod + ", found " + d + " -> " + c)
      }
   }

   /**
    * Checks structural well-formedness of a closed term relative to a home theory.
    * @param home the home theory
    * @param s the term
    * @return the reconstructed term
    */
   def checkTerm(home: Term, s : Term)(implicit pCont: Path => Unit) : Term = checkTerm(home, Context(), s)
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
//            checkTerm(home, context, OMID(from % ln))
         case OMS(GlobalName(t,n)) =>
            val path = t % n
            val ce = try {content.get(t % n)}
                    catch {case e: GetError =>
                                throw Invalid("ill-formed constant reference").setCausedBy(e)
                    }
            ce match {
               case a : Alias =>
                  if (! content.hasImplicit(a.home, home))
                     throw Invalid("constant " + ce.path + " is not imported into home theory " + home)
               case c : Constant =>
                  if (! content.hasImplicit(c.home, home))
                     throw Invalid("constant " + ce.path + " is not imported into home theory " + home)
               case _ => throw Invalid(path + " does not refer to constant")
            }
            pCont(path)
            s
         case OMV(name) =>
            if (! context.isDeclared(name)) throw Invalid("variable is not declared: " + name)
            s
         case OMA(fun, args) =>
            val funR = checkTerm(home, context, fun)
            val argsR = args map {a => checkTerm(home, context, a)}
            OMA(funR, argsR)
         case OMBINDC(binder, bound, condition, scope) =>
            val binderR = checkTerm(home, context, binder)
            val boundR = checkContext(home, context, bound)
            val conditionR = condition map {c => checkTerm(home, context ++ bound, c)}
            val scopeR = checkTerm(home,  context ++ bound, scope)
            OMBINDC(binderR, boundR, conditionR, scopeR)
         case OMATTR(arg, key, value) =>
            val argR = checkTerm(home, context, arg)
            checkTerm(home, context, key)
            val valueR = checkTerm(home, context, value)
            OMATTR(argR, key, valueR)
         case OMM(arg, morph) =>
            val (morphR, from, to) = inferMorphism(morph)
            if (! content.hasImplicit(to, home))
               throw Invalid("codomain of morphism is not imported into expected home theory")
            val argR = checkTerm(from, context, arg) // using the same context because variable attributions are ignored anyway
            OMM(arg, morph)
         case OMHID => s//TODO
         case OME(err, args) =>
            val errR  = checkTerm(home, context, err)
            val argsR = args.map(checkTerm(home, context, _))
            OME(errR, argsR)
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
   def checkContext(home: Term, context: Context = Context(), con: Context)(implicit pCont: Path => Unit) : Context = {
      con.zipWithIndex map {
    	  case (VarDecl(name, tp, df, attrs @_*), i) => 
    	     val tpR = tp.map(x => checkTerm(home, context ++ con.take(i), x)) 
    	     val dfR = df.map(x => checkTerm(home, context ++ con.take(i), x)) 
    	     VarDecl(name, tpR, dfR, attrs :_*) //TODO check attribution
      }
   }
   /**
    * Checks structural well-formedness of a substitution relative to a home theory.
    * @param home the home theory
    * @param subs the substitution
    * @param from the context declaring the variables to be substituted
    * @param from the context in which the substituting terms live
    * @return the reconstructed substitution
    */
   def checkSubstitution(home: Term, subs: Substitution, from: Context, to: Context)(implicit pCont: Path => Unit) : Substitution = {
      if (from.length != subs.length) throw Invalid("substitution " + subs + " has wrong number of cases for context " + from)
      (from zip subs) map {       
    	  case (VarDecl(n,tp,df,attrs @ _*), Sub(m,t)) if n == m =>
    	     val tR = checkTerm(home,to,t)
    	     Sub(m,tR)
    	  case (v,s) =>
    	     throw Invalid("illegal case " + s + " for declaration " + v)
      }
   }
}