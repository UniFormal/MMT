package info.kwarc.mmt.api.libraries
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.patterns._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.moc._

import frontend._

import info.kwarc.mmt.api.utils.MyList.fromList
import info.kwarc.mmt.api.objects.Conversions._

/** CheckResult is the result type of checking a content element */
sealed abstract class CheckResult
sealed abstract class CheckContentResult extends CheckResult
sealed abstract class CheckChangeResult extends CheckResult
/** the content element is well-formed
 * @param deps the list of dependencies of the object
 */
case class ContentSuccess(deps : List[RelationalElement]) extends CheckContentResult
/** the content element is well-formed and was reconstructed to several new objects (e.g., type inference)
 * @param recon the reconstructed content elements
 */
case class ContentReconstructed(recon : List[ContentElement], deps : List[RelationalElement]) extends CheckContentResult

/** the change is well-formed and was reconstructed to several new objects (e.g., type inference)
 * @param recon the reconstructed content elements
 */
case class ChangeSuccess(recon : List[ContentElement], deps : List[RelationsChange]) extends CheckChangeResult

/** the content element is ill-formed
 * @param msg an error message 
 */
case class ContentFail(msg : String) extends CheckContentResult 
/** the content element is ill-formed
 * @param msg an error message 
 */
case class ChangeFail(msg : String) extends CheckChangeResult 


/** Objects of type Checker can be used by a Library to check added ContentElements.
  *  They may return reconstructed declarations and can infer the relational representation. */
abstract class Checker {
   
   def check(s : ContentElement)(implicit mem: ROMemory) : CheckContentResult
   
   def get(p : Path, component : String, callerPath : Path, callerComponent : String)(implicit mem: ROMemory) : ContentElement = {
     mem.ontology += FineGrainedRelation(DependsOn, callerPath, callerComponent, p, component) 
     mem.content.get(p)
   }
   
}

/**
 * A Checker that always succeeds.
 */
object NullChecker extends Checker {
   def check(s : ContentElement)(implicit mem : ROMemory) = ContentSuccess(Nil)
}

/**
 * A Checker that checks only knowledge items whose well-formedness is foundation-independent.
 */
abstract class ModuleChecker extends Checker {
   def check(s : ContentElement)(implicit mem : ROMemory) : CheckContentResult = try {
      checkModuleLevel(s)
   } catch {
      case e : Error => ContentFail("content element contains ill-formed object\n" + e.msg)
      //case e => Fail("error during checking: " + e.msg)
   }
   def checkModuleLevel(e: ContentElement)(implicit mem : ROMemory) : CheckContentResult = {
      e match {
         case t: DeclaredTheory =>
            checkEmpty(t)
            val d = List(IsTheory(t.path))
            t.meta match {
               case Some(mt) =>
                  checkTheo(OMMOD(mt), _ => null, _ => null)
                  val mincl = PlainInclude(mt, t.path)
                  mincl.setOrigin(MetaInclude)
                  val minclflat = mem.content.importsTo(OMMOD(mt)).toList.map {from =>
                     val i = new Include(OMMOD(t.path), from)
                     i.setOrigin(MetaInclude)
                     i
                  }
                  ContentReconstructed(t :: mincl :: minclflat, HasMeta(t.path, mt) :: d)
               case _ =>
                  ContentSuccess(d)
            }
         case t : DefinedTheory =>
            val deps = checkTheo(t.df, IsAliasFor(t.path, _), DependsOn(t.path, _))
            ContentSuccess(IsTheory(t.path) :: deps)
         case l : DeclaredView =>
            checkEmpty(l)
            ContentSuccess(checkLink(l, e.path))
         case l : DefinedView =>
            val occs = checkMorphism(l.df, l.from, l.to)
            val deps = occs.map(DependsOn(l.path, _))
            ContentSuccess(checkLink(l, e.path) ::: deps)
         case l: Include =>
            val par = checkAtomic(l.home)
            val deps = checkTheo(l.from, Includes(par, _), DependsOn(par, _))
            if (mem.content.imports(l.from, l.home)) {
               // ignoring redundant import
               ContentReconstructed(Nil, Nil)
            } else {
               // flattening (transitive closure) of includes
               val flat = mem.content.importsTo(l.from).toList.mapPartial {t =>
                  if (mem.content.imports(t, l.home)) None
                  else {
                     val i = new Include(l.home, t)
                     i.setOrigin(IncludeClosure)
                     Some(i)
                  }
               }
               ContentReconstructed(l :: flat, deps)
            }
         case s: DeclaredStructure =>
            checkEmpty(s)
            val par = checkAtomic(s.home)
            ContentSuccess(checkLink(s, s.path))
         case s: DefinedStructure =>
            checkAtomic(s.home)
            val ldeps = checkLink(s, s.path)
            val ddeps = checkMorphism(s.df, s.from, s.to).map(DependsOn(s.path, _))
            ContentSuccess(ldeps ::: ddeps)
         case e => checkSymbolLevel(e)
      }
   }
   def checkSymbolLevel(e: ContentElement)(implicit mem: ROMemory) : CheckContentResult
   
   protected def checkLink(l : Link, path : Path)(implicit mem: ROMemory) : List[RelationalElement] = {
	  val todep = l match {
         case _ : View =>
            val ds = checkTheo(l.to, HasCodomain(path, _), DependsOn(path, _))
            IsView(path) :: ds
         case _ : Structure => List(IsStructure(path))
      }
      val fromdep = checkTheo(l.to, HasDomain(path, _), DependsOn(path, _))
      todep ::: fromdep
   }
   protected def checkAtomic(m: ModuleObj) : MPath = m match {
      case OMMOD(p) => p
      case _ => throw Invalid("adding constants to composed theories not allowed")
   }
   protected def checkEmpty[D <: Declaration](b: Body[D]) {
      if (! b.isEmpty) throw Invalid("body not empty")
   }

   /** checks whether a theory object is well-formed relative to a library
    *  @param t the theory
    *  @param atomic return value to construct if the theory is atomic
    *  @param nonatomic return value to construct for all occurrences in a nonatomic theory 
    *  @param mem the memory
    *  @return the list of return values
    */
   def checkTheo[A](t : TheoryObj, atomic: Path => A, nonatomic: Path => A)(implicit mem: ROMemory) : List[A] = t match {
     case OMMOD(p) =>
       checkTheoRef(p)
       List(atomic(p))
     case TEmpty(mt) => mt match {
        case None => Nil
        case Some(mt) =>
           checkTheo(OMMOD(mt), _ => null, _ => null)
           List(atomic(mt))
     }
     case TUnion(l,r) =>
        //TODO check same meta-theory?
        val lr = checkTheo(l, atomic, nonatomic) ::: checkTheo(r, atomic, nonatomic)
        lr.distinct
   }
  /** checks whether a morphism object is well-formed relative to a library and infers its type
    *  @param mem the memory
    *  @param m the theory
    *  @return the list of named objects occurring in m, the domain and codomain of m
    */
   def inferMorphism(m : Morph)(implicit mem: ROMemory) : (List[Path], TheoryObj, TheoryObj) = m match {
     case OMMOD(m : MPath) =>
        val l = checkLinkRef(m)
        (List(m), l.from, l.to)
     case OMDL(to, name) =>
        val occs = checkTheo(to, p => p, p => p)
        val from = mem.content.get(to % name) match {
           case l: DefinitionalLink => l.from
           case _ => throw Invalid("invalid morphism " + m)
        }
        (occs, from, to)
     case OMIDENT(t) =>
        val occs = checkTheo(t, p => p, p => p)
        (occs, t, t)
     case OMCOMP(Nil) => throw Invalid("cannot infer type of empty composition")
     case OMCOMP(hd :: Nil) => inferMorphism(hd)
     case OMCOMP(hd :: tl) =>
        val (l1, r,s1) = inferMorphism(hd)
        val (l3, s2,t) = inferMorphism(OMCOMP(tl))
        val l2 = checkInclude(s1,s2) match { 
           case Some(l) => l
           case None => throw Invalid("ill-formed morphism: " + hd + " cannot be composed with " + tl)
        }
        (l1 ::: l2 ::: l3, r, t)
     case MEmpty(f,t) =>
        val occs = checkTheo(f, p => p, p => p) ::: checkTheo(t, p => p, p => p)
        (occs, f,t)
     case MUnion(l,r) =>
        val (loccs, ld, lc) = inferMorphism(l)
        val (roccs, rd, rc) = inferMorphism(r)
        // TODO l === r on (intersection ld rd)
        val dom = TUnion(ld, rd)
        val cod = if (mem.content.imports(lc, rc)) rc
           else if (mem.content.imports(rc, lc)) lc
           else TUnion(rc, lc)
        (loccs ::: roccs, dom ,cod)
   }
   def checkInclude(from: TheoryObj, to: TheoryObj)(implicit mem: ROMemory) : Option[List[Path]] = {
        if (from == to) Some(Nil)
        else if (mem.content.imports(from,to)) Some(List(to % LocalName(IncludeStep(from))))
        else None
   }
   /** checks whether a morphism object is well-formed relative to a library, a domain and a codomain
    *  @param mem the memory
    *  @param m the morphism
    *  @param dom the domain
    *  @param cod the codomain
    *  @return the list of identifiers occurring in m
    */
   def checkMorphism(m : Morph, dom : TheoryObj, cod : TheoryObj)(implicit mem: ROMemory) : List[Path] = {
      val (l, d,c) = inferMorphism(m)
      (checkInclude(dom, d), checkInclude(c, cod)) match {
         case (Some(l0), Some(l1)) => l0 ::: l ::: l1
         case _ => throw Invalid("ill-formed morphism: expected " + dom + " -> " + cod + ", found " + c + " -> " + d)
      }
   }
   /** called for every reference to a theory */
   def checkTheoRef(p: MPath)(implicit mem: ROMemory) : Theory = mem.content.getTheory(p)
   /** called for every reference to a link */
   def checkLinkRef(p: MPath)(implicit mem: ROMemory) : Link = mem.content.getLink(p)
}

object SimpleChecker extends ModuleChecker {
   def checkSymbolLevel(e: ContentElement)(implicit mem: ROMemory) = ContentSuccess(Nil)
}

/**
 * A Checker that implements MMT-well-formedness relative to a foundation.
 */
class FoundChecker(foundation : Foundation, report: Report) extends ModuleChecker {
     def checkModuleChange(c : ChangeModule)(implicit mem: ROMemory) : CheckChangeResult = c match {
     case AddModule(m) => check(m) match {
       case ContentSuccess(l) => ChangeSuccess(Nil,AddRelations(l) :: Nil)
       case ContentReconstructed(r,l) => ChangeSuccess(r, AddRelations(l) :: Nil)
       case ContentFail(msg) => ChangeFail(msg)
     }
     case RenameModule(pold, pnew) => 
       try {
         mem.content.get(pold)
    	 try {
    	   mem.content.get(pnew)
           ChangeFail("trying to rename module to already existent path " + pnew)
    	 } catch {
    	   case e : Error => ChangeSuccess(Nil, RenameRelations(pold, pnew) :: Nil)
         }
       } catch {
         case e : Error => ChangeFail("trying to rename non-existent module path " + e.msg)
       }
     
     case DeleteModule(m) => 
       try {
         val v = mem.content.get(m.path)
         if (v == m) {
           ChangeSuccess(Nil, DeleteRelations(m.path) :: Nil)
         } else {
           ChangeFail("deleted module does not fit with actual value, change would not be inversable: Change Value:\n " + m.toString + "\n LibraryValue\n" + v.toString)
         }
       } catch {
         case e : Error => ChangeFail("trying to delete non-existent module " + e.msg)
       }
   }
   
   def checkDeclarationChange(ch : ChangeDeclaration)(implicit mem : ROMemory) : CheckChangeResult = ch match {
     case AddDeclaration(d) => check(d) match {
       case ContentSuccess(l) => ChangeSuccess(Nil, AddRelations(l) :: Nil)
       case ContentReconstructed(r,l) => ChangeSuccess(r, AddRelations(l) :: Nil)
       case ContentFail(msg) => ChangeFail(msg)
     }
     case DeleteDeclaration(d) => 
       try {
         val v = mem.content.get(d.path)
         if (v == d) {
           ChangeSuccess(Nil, DeleteRelations(d.path) :: Nil)
         } else {
           ChangeFail("deleted module does not fit with actual value, change would not be inversable: Change Value:\n " + d.toString + "\n LibraryValue\n" + v.toString)
         }
       } catch {
         case e : Error => ChangeFail("trying to delete non-existent module " + e.msg)
       }

     case RenameDeclaration(pold,pnew) =>
       try {
         mem.content.get(pold)
    	 try {
    	   mem.content.get(pold.mod ? pnew)
           ChangeFail("trying to rename declaration to already existent path " + pnew)
    	 } catch {
    	   case e : Error => ChangeSuccess(Nil, RenameRelations(pold, pold.mod ? pnew) :: Nil)
         }
       } catch {
         case e : Error => ChangeFail("trying to rename non-existent declaration path " + e.msg)
       }
   }
   
   def checkComponentChange(home : TheoryObj, path : Path, ch : UpdateComponent)(implicit mem : ROMemory) : CheckChangeResult = ch match {

     case UpdateComponent(name,old, nw, chs) => {
       val dec = mem.content.get(path)
       val compNames = dec.compNames.toMap
       val libco = dec.components(compNames(name.toPath))
       if (libco == old) {
         nw match {
           case t : Term => 
           	 try {
           	   val s = checkTerm(home, Context(), t).map(p => FineGrainedRelation(DependsOn,path, name.toPath, p, "TODO")) //TODO component, either add here or remove in FineGrainedRelation class
           	   ChangeSuccess(Nil, AddRelations(s) :: Nil)  
             } catch {
               case e : Error => ChangeFail(e.msg)
             }
         case _ => throw ImplementationError(nw.toString)
         }
       } else {
         ChangeFail("Trying to delete nonexistent component")
       }
     }
   }

  //Note: Even Library.addUnchecked checks that declarations can only be added to atomic declared theories/views.
  //Therefore, the home modules are not checked here.
  //Body.add checks that no two declarations of the same name exist. Therefore, names are not checked here.
  //Names that are prefixes of other names in the same body are permitted.
  //  This works well for links, for theories it is questionable. Refusing declaration with non-primitive names might be forbidden.
  //  When retrieving, more specific entries overrule the more general ones.

  //TODO: compatibility of multiple assignments to the same knowledge item
  /** checks whether a content element may be added to a library
    * @param mem the memory
    * @param s the content element
    * throws backend.NotFound(p) if possibly well-formed after retrieving p
    */
   def checkSymbolLevel(s : ContentElement)(implicit mem: ROMemory) : CheckContentResult = s match {
         case c : Constant =>
            if (c.parent == foundation.foundTheory) return ContentSuccess(Nil) //TODO this should be more sophisticated; it should be possible to register multiple foundations
            //checkHomeTheory(c)
            val occtp = if (c.tp.isDefined) checkTerm(c.home, c.tp.get) else Nil
            val occdf = if (c.df.isDefined) checkTerm(c.home, c.df.get) else Nil
            if (! foundation.typing(c.df, c.tp)(mem.content)) return ContentFail("constant declaration does not type-check")
            val deps = IsConstant(c.rl).apply(c.path) ::  
              occtp.map(HasOccurrenceOfInType(c.path, _)) ::: occdf.map(HasOccurrenceOfInDefinition(c.path, _))
            ContentSuccess(deps)
         case a : ConstantAssignment =>
            val (l,d) = getSource(a)
            val c = d match {
                case c : Constant => c
                case _ => return ContentFail("constant-assignment to non-constant")
            }
            val occas = checkTerm(l.to, a.target)
            if (! foundation.typing(Some(a.target), c.tp.map(_ * a.home))(mem.content))
               return ContentFail("assignment does not type-check")
            val defleq = c.df.isEmpty || a.target == OMHID() || foundation.equality(c.df.get, a.target)(mem.content)
            if (! defleq) return ContentFail("assignment violates definedness ordering")
            val deps = IsConAss(a.path) :: occas.map(HasOccurrenceOfInTarget(a.path, _))
            ContentSuccess(deps)
         case a : DefLinkAssignment =>
            val (l,d) = getSource(a)
            val s = d match {
                case s : Structure => s
                case _ => return ContentFail("structure-assignment to non-structure")
            }
            val occs = checkMorphism(a.target, s.from, l.to)
            val deps = IsStrAss(a.path) :: occs.map(DependsOn(a.path, _))
            // flattening of includes: this assignment also applies to every theory t included into s.from 
            val flat = mem.content.importsTo(s.from).toList.mapPartial {t =>
               val name = a.name.thenInclude(t)
               if (l.declares(name)) None //here, the compatibility check can be added
               else {
                  val r = new DefLinkAssignment(a.home, name, a.target)
                  r.setOrigin(IncludeClosure)
                  Some(r)
               }
            }
            ContentReconstructed(a :: flat, deps)
         case p : Pattern =>
            val paths = checkContext(p.home, p.params ++ p.body)
            val deps = IsPattern(p.path) :: paths.map(HasOccurrenceOfInDefinition(p.path, _))
            ContentSuccess(deps)
         case i : Instance => 
            val pt : Pattern = mem.content.getPattern(i.pattern)
            val paths : List[Path] = Nil //checkSubstitution(i.home, i.matches, pt.params, Context())
            // Mihnea's instances already refer to the elaborated constants stemming from the same instance
            val deps = IsInstance(i.path) :: IsInstanceOf(i.path, i.pattern) :: paths.map(HasOccurrenceOfInDefinition(i.path, _))
            val elab = Instance.elaborate(i, true)(mem.content, report)
            i.setOrigin(Elaborated)
            ContentReconstructed(i :: elab, deps)
         case _ => ContentSuccess(Nil)
   }
/*       case a : Alias =>
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

   private def getSource(a: Assignment)(implicit mem: ROMemory) : (DeclaredLink, ContentElement) = {
      val p = a.home match {
         case OMMOD(p) => p
         case OMDL(OMMOD(p), name) => OMMOD(p) % name 
         case _ => throw Invalid("adding assignment to non-atomic link")
      }
      val l = mem.content.get(p) match {
         case l: DeclaredLink => l
         case _ => throw Invalid("adding assignment to non-declared link") 
      }
      val s = try {mem.content.get(l.from % a.name)}
              catch {case e : Error => throw Invalid("assignment to undeclared name").setCausedBy(e)}
      (l, s)
   }
   /**
    * Checks structural well-formedness of a closed term relative to a library and a home theory.
    * @param home the home theory
    * @param s the term
    * @param mem  the memory
    * @return the list of identifiers occurring in s (no duplicates, random order)
    */
   def checkTerm(home: TheoryObj, s : Term)(implicit mem: ROMemory) : List[Path] = {
      checkTheo(home, p => p, p => p)
      checkTerm(home, Context(), s).distinct
   }
   //TODO redesign role checking, currently not done
   private def checkTerm(home : TheoryObj, context : Context, s : Term)(implicit mem: ROMemory) : List[Path] = {
      s match {
         case OMID((h: TheoryObj) % (IncludeStep(from) / ln)) =>
            if (! mem.content.imports(from, h))
               throw Invalid(from + " is not imported into " + h + " in " + s)
            checkTerm(home, context, OMID(from % ln))
         case OMID(path) =>
            //val toccs = checkTheo(path.parent) TODO check theory?
            val s = try {mem.content.get(path)}
                    catch {case e: GetError =>
                                throw Invalid("ill-formed constant reference").setCausedBy(e)
                    }
            s match {
               case a : Alias =>
                  if (! mem.content.imports(a.home, home))
                     throw Invalid("constant " + s.path + " is not imported into home theory " + home)
                  List(path)
               case c : Constant =>
                  if (! mem.content.imports(c.home, home))
                     throw Invalid("constant " + s.path + " is not imported into home theory " + home)

                  List(path)
               case _ => throw Invalid(path + " does not refer to constant")
            }
         case OMV(name) =>
            if (! context.isDeclared(name)) throw Invalid("variable is not declared: " + name)
            Nil
         case OMA(fun, args) =>
            val occf = checkTerm(home, context, fun) //TODO level of application cannot be inferred
            val occa = checkSeq(home, context, SeqItemList(args))
            occf ::: occa
         case OMBINDC(binder, bound, condition, scope) =>
            val newcontext = context ++ bound // every variable can occur in every variable declaration
            val occb = checkTerm(home, context, binder)
            val occv = bound.variables.flatMap {
               // not checking the attributions
               case TermVarDecl(_, tp, df, attrs @ _*) => 
                 List(tp,df).filter(_.isDefined).map(_.get).flatMap(checkTerm(home, newcontext, _))                     
               case SeqVarDecl(_, tp, df) => 
                 List(tp,df).filter(_.isDefined).map(_.get).flatMap(checkSeq(home, newcontext, _))
            }
            val occc = if (condition.isDefined) checkTerm(home, newcontext, condition.get)
               else Nil
            val occs = checkTerm(home, newcontext, scope)
            occb ::: occv.toList ::: occc ::: occs
         case OMATTR(arg, key, value) =>
            val occa = checkTerm(home, context, arg)
            val occk = checkTerm(home, context, key)
            val occv = checkTerm(home, context, value)
            occa ::: occk ::: occv
         case OMM(arg, morph) =>
            val (occm, from, to) = inferMorphism(morph)
            if (! mem.content.imports(to, home))
               throw Invalid("codomain of morphism is not imported into expected home theory")
            val occa = checkTerm(from, context, arg) // using the same context because variable attributions are ignored anyway
            occm ::: occa
         case t @ OMSub(arg, via) =>
            val bigcon = context ++ via
            val occvia = via.variables.toList flatMap {
               case TermVarDecl(n, t, d, atts @ _*) =>
                  t.map(checkTerm(home, bigcon, _)).getOrElse(Nil) ::: d.map(checkTerm(home, bigcon, _)).getOrElse(Nil)
               case SeqVarDecl(n, t, d) =>
                  t.map(checkSeq(home, bigcon, _)).getOrElse(Nil) ::: d.map(checkSeq(home, bigcon, _)).getOrElse(Nil)
               }
            val occarg = checkTerm(home, bigcon, arg)
            occvia ::: occarg
         case OMHID => Nil//TODO roles
         case OME(err, args) =>
            val occe = checkTerm(home, context, err)
            val occa = args.flatMap(checkTerm(home, context, _))
            occe ::: occa
         case OMFOREIGN(node) => Nil //TODO
         case OMI(i) => Nil //TODO check if integers are permitted
         case OMSTR(s) => Nil //TODO check if strings are permitted
         case OMF(d) => Nil //TODO check if floats are permitted
         case OMSemiFormal(t) => Nil //TODO
         case Index(seq,ind) => checkSeq(home,context,seq) ::: checkTerm(home,context,ind)
      }
   }
   def checkSeq(home : TheoryObj, context : Context, s : objects.Sequence)(implicit mem: ROMemory) : List[Path] = s match {
   	  case t: Term => checkTerm(home, context, t)
   	  case SeqVar(n) =>
          try {
        	  context(n) match {
   		  	    case SeqVarDecl(_,_,_) => Nil
   		  	    case TermVarDecl(_,_,_,_*) => throw Invalid("sequence variable expected, found term variable: " + n)
   	  	      }
          } catch {
        	  case LookupError(n) => throw Invalid("sequence variable is not declared: " + n) 
          }
   	  //case SeqSubst(ex,n,sq) => checkTerm(home,context ++ TermVarDecl(n,None,None),ex) ::: checkSeq(home,context,sq)
      case SeqSubst(sq1,n,sq2) => checkSeq(home,context ++ TermVarDecl(n,None,None),sq1) ::: checkSeq(home,context,sq2)
   	  case SeqUpTo(t) => checkTerm(home,context,t)
   	  case SeqItemList(items) => items.flatMap(i => checkSeq(home,context,i))   	  
   }
   def checkContext(home: TheoryObj, con: Context)(implicit mem: ROMemory) : List[Path] = {
      con.flatMap {
    	  case TermVarDecl(name, tp, df, attrs @_*) => 
    	   val tpl = tp.map(x => checkTerm(home,con,x)).getOrElse(Nil) 
    	   val dfl = df.map(x => checkTerm(home,con,x)).getOrElse(Nil) 
    	   tpl ::: dfl //TODO not checking attributes
    	  case SeqVarDecl(name, tp, df, attrs @_*) => 
    	   val tpl = tp.map(x => checkSeq(home,con,x)).getOrElse(Nil) 
    	   val dfl = df.map(x => checkSeq(home,con,x)).getOrElse(Nil) 
    	   tpl ::: dfl //TODO not checking attributes
    	  }
   }
   def checkSubstitution(home: TheoryObj, subs: Substitution, from: Context, to: Context)(implicit mem: ROMemory) : List[Path] = {
      if (from.length != subs.length) throw Invalid("substitution " + subs + " has wrong number of cases for context " + from)
      (from zip subs).flatMap {       
    	  case (TermVarDecl(n,tp,df,attrs @ _*), TermSub(m,t)) if n == m => checkTerm(home,to,t) 
    	  case (SeqVarDecl(n,tp,df,attrs @ _*),SeqSub(m,s)) if n == m => checkSeq(home,to,s)
    	  case (v,s) => throw Invalid("illegal case " + s + " for declaration " + v)
      }
   }
}

class StructuralChecker(report: Report) extends FoundChecker(new DefaultFoundation, report)
