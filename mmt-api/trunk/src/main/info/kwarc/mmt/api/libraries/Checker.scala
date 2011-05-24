package info.kwarc.mmt.api.libraries
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.patterns._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.utils.MyList.fromList

/** CheckResult is the result type of checking a content element */
sealed abstract class CheckResult
/** the content element is well-formed
 * @param decls the list of dependencies of the object 
 */
case class Success(deps : List[RelationalElement]) extends CheckResult
/** the content element is well-formed and was reconstructed to several new objects (e.g., type inference)
 * @param recon the reconstructed content elements
 */
case class Reconstructed(recon : List[ContentElement], deps : List[RelationalElement]) extends CheckResult
/** the content element is ill-formed
 * @param msg an error message 
 */
case class Fail(msg : String) extends CheckResult

/** Objects of type Checker can be used by a Library to check added ContentElements.
  *  They may return reconstructed declarations and can infer the relational represtnestion. */
abstract class Checker {
   def check(s : ContentElement)(implicit lib : Lookup) : CheckResult
}

/**
 * A Checker that always succeeds.
 */
object NullChecker extends Checker {
   def check(s : ContentElement)(implicit lib : Lookup) = Success(Nil)
}

/**
 * A Checker that checks only knowledge items whose well-formedness is foundation-independent.
 */
abstract class ModuleChecker extends Checker {
   def check(s : ContentElement)(implicit lib : Lookup) : CheckResult = try {
      checkModuleLevel(s)
   } catch {
      case Invalid(msg) => Fail("content element contains ill-formed object: " + msg)
      case GetError(msg) => Fail("content element contains ill-formed object: get error: " + msg)
      //case e => Fail("error during checking: " + e.msg)
   }
   def checkModuleLevel(e: ContentElement)(implicit lib : Lookup) : CheckResult = 
      e match {
         case t: DeclaredTheory =>
            checkEmpty(t)
            val d = List(IsTheory(t.path))
            t.meta match {
               case Some(mt) =>
                  checkTheo(OMMOD(mt), _ => null, _ => null)
                  val mincl = PlainInclude(mt, t.path)
                  mincl.setOrigin(MetaInclude)
                  Reconstructed(List(t, mincl), HasMeta(t.path, mt) :: d)
               case _ =>
                  Success(d)
            }
         case t : DefinedTheory =>
            val deps = checkTheo(t.df, IsAliasFor(t.path, _), DependsOn(t.path, _))
            Success(IsTheory(t.path) :: deps)
         case l : DeclaredView =>
            checkEmpty(l)
            Success(checkLink(l, e.path))
         case l : DefinedView =>
            val occs = checkMorphism(l.df, l.from, l.to)
            val deps = occs.map(DependsOn(l.path, _))
            Success(checkLink(l, e.path) ::: deps)
         case l: Include =>
            val par = checkAtomic(l.home)
            val deps = checkTheo(l.from, Includes(par, _), DependsOn(par, _))
            // flattening (transitive closure) of includes 
            val flat = lib.importsTo(l.from).toList.mapPartial {t =>
               if (lib.imports(t, l.home)) None
               else {
                  val i = new Include(l.home, t)
                  i.setOrigin(IncludeClosure)
                  Some(i)
               }
            }
            Reconstructed(l:: flat.toList, deps)
         case s: DeclaredStructure =>
            checkEmpty(s)
            val par = checkAtomic(s.home)
            Success(checkLink(s, s.path))
         case s: DefinedStructure =>
            checkAtomic(s.home)
            val ldeps = checkLink(s, s.path)
            val ddeps = checkMorphism(s.df, s.from, s.to).map(DependsOn(s.path, _))
            Success(ldeps ::: ddeps)
         case e => checkSymbolLevel(e)
      }
   def checkSymbolLevel(e: ContentElement)(implicit lib : Lookup) : CheckResult
   
   protected def checkLink(l : Link, path : Path)(implicit lib : Lookup) : List[RelationalElement] = {
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
    *  @parem atomic return value to construct if the theory is atomic
    *  @param nonatomic return value to construct for all occurrences in a nonatomic theory 
    *  @param lib the library
    *  @return the list of return values
    */
   def checkTheo[A](t : TheoryObj, atomic: Path => A, nonatomic: Path => A)(implicit lib : Lookup) : List[A] = t match {
     case OMMOD(p) =>
       checkTheoRef(p)
       List(atomic(p))
   }
  /** checks whether a morphism object is well-formed relative to a library and infers its type
    *  @param lib the library
    *  @param m the theory
    *  @return the list of named objects occurring in m, the domain and codomain of m
    */
   def inferMorphism(m : Morph)(implicit lib : Lookup) : (List[Path], TheoryObj, TheoryObj) = m match {
     case OMMOD(m : MPath) =>
        val l = checkLinkRef(m)
        (List(m), l.from, l.to)
     case OMDL(to, name) =>
        val occs = checkTheo(to, p => p, p => p)
        val from = lib.get(to % name) match {
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
     case OMEMPTY(f,t) =>
        val occs = checkTheo(f, p => p, p => p) ::: checkTheo(t, p => p, p => p)
        (occs, f,t)
   }
   def checkInclude(from: TheoryObj, to: TheoryObj)(implicit lib : Lookup) : Option[List[Path]] = {
        if (from == to) Some(Nil)
        else if (lib.imports(from,to)) Some(List(to % LocalName(IncludeStep(from))))
        else None
   }
   /** checks whether a morphism object is well-formed relative to a library, a domain and a codomain
    *  @param lib the library
    *  @param m the morphism
    *  @param dom the domain
    *  @param cod the codomain
    *  @return the list of identifiers occurring in m
    */
   def checkMorphism(m : Morph, dom : TheoryObj, cod : TheoryObj)(implicit lib : Lookup) : List[Path] = {
      val (l, d,c) = inferMorphism(m)
      (checkInclude(dom, d), checkInclude(c, cod)) match {
         case (Some(l0), Some(l1)) => l0 ::: l ::: l1
         case _ => throw Invalid("ill-formed morphism: expected " + dom + " -> " + cod + ", found " + c + " -> " + d)
      }
   }
   /** called for every reference to a theory */
   def checkTheoRef(p: MPath)(implicit lib : Lookup) : Theory = lib.getTheory(p)
   /** called for every reference to a link */
   def checkLinkRef(p: MPath)(implicit lib : Lookup) : Link = lib.getLink(p)
}

object SimpleChecker extends ModuleChecker {
   def checkSymbolLevel(e: ContentElement)(implicit lib : Lookup) = Success(Nil)
}

/**
 * A Checker that implements MMT-well-formedness relative to a foundation.
 */
class FoundChecker(foundation : Foundation) extends ModuleChecker {
   /** checks whether a content element may be added to a library
    * @param lib the library
    * @param s the content element
    * throws backend.NotFound(p) if possibly well-formed after retrieving p
    */
   //Note: Even Library.addUnchecked checks that declarations can only be added to atomic declared theories/views.
   //Therefore, the home modules are not checked here.
   //Body.add checks that no two declarations of the same name exist. Therefore, names are not checked here.
   //Names that are prefixes of other names in the same body are permitted.
   //  This works well for links, for theories it is questionable. Refusing declaration with non-primitive names might be forbidden.
   //  When retrieving, more specific entries overrule the more general ones.

   //TODO: compatibility of multiple assignments to the same knowledge item
   def checkSymbolLevel(s : ContentElement)(implicit lib : Lookup) : CheckResult = s match {
         case c : Constant =>
            //checkHomeTheory(c)
            val occtp = if (c.tp.isDefined) checkTerm(c.home, c.tp.get, c.uv) else Nil
            val occdf = if (c.df.isDefined) checkTerm(c.home, c.df.get, c.uv) else Nil
            if (! foundation.typing(c.df, c.tp)) return Fail("definition of constant does not type-check")
            val deps = IsConstant(c.path) ::  
              occtp.map(HasOccurrenceOfInType(c.path, _)) ::: occdf.map(HasOccurrenceOfInDefinition(c.path, _))
            Success(deps)
         case a : ConstantAssignment =>
            val (l,d) = getSource(a)
            val c = d match {
                case c : Constant => c
                case _ => return Fail("constant-assignment to non-constant")
            }
            val occas = checkTerm(l.to, a.target, c.uv)
            if (! foundation.typing(Some(a.target), c.tp.map(_ * a.home)))
               return Fail("assignment does not type-check")
            val defleq = c.df.isEmpty || a.target == OMHID() || foundation.equality(c.df.get, a.target)
            if (! defleq) return Fail("assignment violates definedness ordering")
            val deps = IsConAss(a.path) :: occas.map(HasOccurrenceOfInTarget(a.path, _))
            Success(deps)
         case a : DefLinkAssignment =>
            val (l,d) = getSource(a)
            val s = d match {
                case s : Structure => s
                case _ => return Fail("structure-assignment to non-structure")
            }
            val occs = checkMorphism(a.target, s.from, l.to)
            val deps = IsStrAss(a.path) :: occs.map(DependsOn(a.path, _))
            // flattening of includes: this assignment also applies to every theory t included into s.from 
            val flat = lib.importsTo(s.from).toList.mapPartial {t =>
               val name = a.name.thenInclude(t)
               if (l.declares(name)) None //here, the compatibility check can be added
               else {
                  val r = new DefLinkAssignment(a.home, name, a.target)
                  r.setOrigin(IncludeClosure)
                  Some(r)
               }
            }
            Reconstructed(a :: flat, deps)
         case _ => Success(Nil)
   }
/*       case a : Alias =>
            lib.get(a.forpath)
            if (lib.imports(a.forpath.parent, a.parent))
               Success(List(IsAlias(a.path), IsAliasFor(a.path, a.forpath)))
            else
               Fail("illegal alias")
         case a : Open =>
            val str = lib.getStructure(a.parent)
            val source = try {lib.getSymbol(str.from ? a.name)}
                         catch {case _ => return Fail("open of non-existing constant")}
            val name = a.as.map(LocalName(_)).getOrElse(a.name)
            val al = new Alias(str.to, name, str.to ? str.name / source.name)
            Reconstructed(List(a, al), List(IsOpen(a.path)))
         case p : Pattern => Success(Nil) // TODO
         case i : Instance => Reconstructed(List(Pattern.elaborate(i,lib)),Nil)
 */

   private def getSource(a: Assignment)(implicit lib: Lookup) : (DeclaredLink, ContentElement) = {
      val p = a.home match {
         case OMMOD(p) => p
         case OMDL(OMMOD(p), name) => OMMOD(p) % name 
         case _ => throw Invalid("adding assignment to non-declared-view")
      }
      val l = lib.get(p) match {
         case l: DeclaredLink => l
         case _ => throw Invalid("adding assignment to non-declared-view") 
      }
      val s = try {lib.get(l.from % a.name)}
              catch {case _ => throw Invalid("assignment to undeclared name")}
      (l, s)
   }
   /**
    * Checks structural well-formedness of a closed term relative to a library and a home theory.
    * @param lib the library
    * @param home the home theory
    * @param s the term
    * @param univ the universe to check the term against
    * @return the list of identifiers occurring in s (no duplicates, random order)
    */
   def checkTerm(home: TheoryObj, s : Term, univ : Universe)(implicit lib : Lookup) : List[Path] = {
      checkTheo(home, p => p, p => p)
      checkTerm(home, Context(), s, IsEqualTo(univ)).distinct
   }
   //TODO redesign role checking, currently not done
   private def checkTerm(home : TheoryObj, context : Context, s : Term, uvcheck : UnivCheck)(implicit lib : Lookup) : List[Path] = {
      s match {
         case OMID((h: TheoryObj) % (IncludeStep(from) / ln)) =>
            if (! lib.imports(from, h))
               throw Invalid(from + " is not imported into " + h + " in " + s)
            checkTerm(home, context, OMID(from % ln), uvcheck)
         case OMID(path) =>
            //val toccs = checkTheo(path.parent) TODO check theory?
            val s = try {lib.get(path)}
                    catch {case GetError(msg) =>
                                throw Invalid("ill-formed constant reference: " + msg)
                    }
            s match {
               case a : Alias =>
                  if (! lib.imports(a.home, home))
                     throw Invalid("constant " + s + " is not imported into home theory " + home)
                  List(path)
               case c : Constant =>
                  if (! lib.imports(c.home, home))
                     throw Invalid("constant " + s + " is not imported into home theory " + home)
/*                  uvcheck(c.uv) match {
                     case None => ()
                     case Some(msg) => //throw Invalid(msg)
} */
                  List(path)
               case _ => throw Invalid(path + " does not refer to constant")
            }
         //for now: variables check against any role
         case OMV(name) =>
            if (! context.isDeclared(name)) throw Invalid("variable is not declared: " + name)
            Nil
         case OMA(fun, args) =>
            val occf = checkTerm(home, context, fun, IsSemantic) //TODO level of application cannot be inferred
            val occa = args.flatMap(checkTerm(home, context, _, IsSemantic))
            occf ::: occa
         case OMBINDC(binder, bound, condition, scope) =>
            val newcontext = context ++ bound // every variable can occur in every variable declaration
            val occb = checkTerm(home, context, binder, IsEqualTo(Binder))
            val occv = bound.variables.flatMap {
               // not checking the attributions
               case TermVarDecl(_, tp, df, attrs @ _*) => List(tp,df).filter(_.isDefined).map(_.get).flatMap(
                     checkTerm(home, newcontext, _, IsSemantic)
               )
            }
            val occc = if (condition.isDefined) checkTerm(home, newcontext, condition.get, IsSemantic)
               else Nil
            val occs = checkTerm(home, newcontext, scope, IsSemantic)
            occb ::: occv.toList ::: occc ::: occs
         case OMATTR(arg, key, value) =>
            val occa = checkTerm(home, context, arg, uvcheck)
            val occk = checkTerm(home, context, key, IsEqualTo(Key))
            val occv = checkTerm(home, context, value, IsSemantic)
            occa ::: occk ::: occv
         case OMM(arg, morph) =>
            val (occm, from, to) = inferMorphism(morph)
            if (! lib.imports(to, home))
               throw Invalid("codomain of morphism is not imported into expected home theory")
            val occa = checkTerm(from, context, arg, uvcheck) // using the same context because variable attributions are ignored anyway
            occm ::: occa
         case t @ OMSub(arg, via) =>
            val bigcon = context ++ via
            val occvia = via.variables.toList flatMap {
               case TermVarDecl(n, t, d, atts @ _*) =>
                  t.map(checkTerm(home, bigcon, _, uvcheck)).getOrElse(Nil) ::: d.map(checkTerm(home, bigcon, _, uvcheck)).getOrElse(Nil)
               }
            val occarg = checkTerm(home, bigcon, arg, uvcheck)
            occvia ::: occarg
         case OMHID => Nil//TODO roles
         case OME(err, args) =>
            val occe = checkTerm(home, context, err, IsEqualTo(Error))
            val occa = args.flatMap(checkTerm(home, context, _, IsSemantic))
            occe ::: occa
         case OMFOREIGN(node) => Nil //TODO roles, dependencies?
         case OMI(i) => Nil //TODO roles; check import of pseudo-theories, dependencies?
         case OMSTR(s) => Nil //TODO roles; check import of pseudo-theories, dependencies?
         case OMF(d) => Nil //TODO roles; check import of pseudo-theories, dependencies?
         case OMSemiFormal(t) => Nil //TODO
      }
   }
}

object StructuralChecker extends FoundChecker(DefaultFoundation)