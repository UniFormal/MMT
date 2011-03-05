package jomdoc.libraries
import jomdoc._
import jomdoc.modules._
import jomdoc.symbols._
import jomdoc.objects._
import jomdoc.patterns._
import jomdoc.ontology._
import jomdoc.utils._

/** CheckResult is the result type of checking a content element */
sealed abstract class CheckResult
/** the content element is well-formed
 * @param decls the list of dependencies of the object 
 */
case class Success(deps : List[ABoxDecl]) extends CheckResult
/** the content element is well-formed and was reconstructed to several new objects (e.g., type inference)
 * @param recon the reconstructed content elements
 */
case class Reconstructed(recon : List[ContentElement], deps : List[ABoxDecl]) extends CheckResult
/** the content element is ill-formed
 * @param msg an error message 
 */
case class Fail(msg : String) extends CheckResult

abstract class Checker {
   protected def lookupTheory(lib : Lookup, t : MPath) {
      lib.getTheory(t) // can throw NotFound(t)
   }   
   def check(s : ContentElement)(implicit lib : Lookup) : CheckResult
}

/**
 * A Checker that always succeeds.
 */
object NullChecker extends Checker {
   def check(s : ContentElement)(implicit lib : Lookup) = Success(Nil)
}

/**
 * A Checker that makes sure all referenced theories are loaded and infers simple ABox elements,
 * but checks nothing else.
 */
object GraphChecker extends Checker {
   def check(s : ContentElement)(implicit lib : Lookup) = {
      s match {
	      case TheoImport(par, OMT(f)) =>
	         lookupTheory(lib, f)
             Success(List(HasOccurrenceOfInImport(par, f)))
	      case t : Theory =>
	         val mdep = t.meta match {
	            case Some(m) => lookupTheory(lib, m); List(HasMeta(t.path, m))
                case _ => Nil
	         }
             Success(IsTheory(t.path) :: mdep)
	      case l : Link =>
	         lookupTheory(lib, l.from)
	         lookupTheory(lib, l.to)
             val tpdep = l match {
               case _ : View => IsView(l.path)
               case _ : Structure => IsStructure(l.path)
             }
             Success(List(tpdep, HasDomain(l.path,l.from), HasCodomain(l.path,l.to)))
          //TODO: should also load all views
          case _ => Success(Nil)
      }
   }
}

/**
 * A Checker that implements MMT-wellformedness relative to a foundation.
 */
class FoundChecker(foundation : Foundation) extends Checker {
   /** checks whether a content element may be added to a library
    * @param lib the library
    * @param s the content element
    * throws backend.NotFound(p) if possibly well-formed after retrieving p
    */
   def check(s : ContentElement)(implicit lib : Lookup) : CheckResult = try {
      s match {
         case TheoImport(par, of) =>
            lookupTheory(lib, par)
            val occs = checkTheo(lib,of)
            val deps = occs.map(HasOccurrenceOfInImport(par, _))
            Success(deps)
         case LinkImport(par, of) =>
            lib.getLink(par)
            val (occs, _, _) = inferMorphism(lib, of) 
            val deps = occs.map(p => HasOccurrenceOfInImport(par, p))
            Success(deps)
            //TODO: check for overlap between imported morphisms
         case t : Theory =>
            if (! t.isEmpty) return Fail("new theory not empty")
            val mdep = t.meta match {
               case Some(mt) =>
                  lookupTheory(lib, mt)
                  List(HasMeta(t.path, mt))
               case _ => Nil
            }
            Success(IsTheory(t.path) :: mdep)
         case l : DeclaredLink =>
            val tpdep = l match {
               case _ : View =>
                  lookupTheory(lib, l.to)
                  IsView(l.path)
               case _ : Structure => IsStructure(l.path)
            }
            lookupTheory(lib, l.from)
            if (! l.isEmpty) return Fail("new declared link not empty")
            val deps = List(tpdep, HasDomain(l.path, l.from), HasCodomain(l.path, l.to)) 
            Success(deps)
         case l : DefinedLink =>
            val (occs, from, to) = inferMorphism(lib, l.df)
            if (! lib.imports(l.from, from))
               return Fail("definition of defined link does not type-check: " + l.from + " is not imported into " + from)
            if (! lib.imports(to, l.to))
               return Fail("definition of defined link does not type-check: " + to + " is not imported into " + l.to)
            val tpdep = l match {
               case _ : View => IsView(l.path)
               case _ : Structure => IsStructure(l.path)
            }
            val deps = occs.map(HasOccurrenceOfInDefinition(l.path, _))
            Success(tpdep :: deps)
         case c : Constant =>
            val occtp = if (c.tp.isDefined) {
               checkTerm(lib, c.parent, c.tp.get, c.uv)
            } else
               Nil
            val occdf = if (c.df.isDefined) checkTerm(lib, c.parent, c.df.get, c.uv) else Nil
            if (! foundation.typing(c.df, c.tp)) return Fail("definition of constant does not type-check")
            val deps = IsConstant(c.path) ::  
              occtp.map(HasOccurrenceOfInType(c.path, _)) ::: occdf.map(HasOccurrenceOfInDefinition(c.path, _))
            Success(deps)
         case a : Alias =>
            lib.get(a.forpath)
            if (lib.imports(a.forpath.parent, a.parent))
               Success(List(IsAlias(a.path), IsAliasFor(a.path, a.forpath)))
            else
               Fail("illegal alias")
         case a : ConstantAssignment =>
            val link = lib.getLink(a.parent)
            val sourceSym = try {lib.getSymbolNoAlias(link.from ? a.name)}
                         catch {case _ => return Fail("assignment to non-existing constant")}
            val source = sourceSym match {
                case s : Structure => return Fail("assignment to structure")
                case c : Constant => if (c.parent == link.from) c else return Fail("assignment to included constant")
            }
            val defleq = source.df.isEmpty || a.target == OMHID() || foundation.equality(source.df.get, a.target)
            if (! defleq) return Fail("assignment violates definedness ordering")
            val occas = checkTerm(lib, link.to, a.target, source.uv)
            if (! foundation.typing(Some(a.target), source.tp.map(_ * OML(a.parent))))
               return Fail("assignment does not type-check")
            val deps = IsConAss(a.path) :: occas.map(HasOccurrenceOfInTarget(a.path, _))
            Success(deps)
         case a : StructureAssignment =>
            val link = lib.getLink(a.parent)
            val source = try {lib.getStructure(link.from ? a.name)}
                         catch {case _ => return Fail("assignment to non-existing structure")}
            val occs = checkMorphism(lib, a.target, source.from, link.to)
            //TODO: checking of equality
            val deps = IsConAss(a.path) :: occs.map(HasOccurrenceOfInTarget(a.path, _))
            Success(deps)
         case a : Open =>
            val str = lib.getStructure(a.parent)
            val source = try {lib.getSymbol(str.from ? a.name)}
                         catch {case _ => return Fail("open of non-existing constant")}
            val name = a.as.map(new LocalPath(_)).getOrElse(a.name)
            val al = new Alias(str.to, name, str.to ? str.name / source.name)
            Reconstructed(List(a, al), List(IsOpen(a.path)))
         case p : Pattern => Success(Nil) // TODO
         case i : Instance => Reconstructed(List(Pattern.elaborate(i,lib)),Nil)
      }
   } catch {
      case Invalid(msg) => Fail("content element contains ill-formed object: " + msg)
   }
   
   /**
    * Checks structural well-formedness of a closed term relative to a library and a home theory.
    * @param lib the library
    * @param home the home theory
    * @param s the term
    * @param univ the universe to check the term against
    * @return the list of identifiers occurring in s (no duplicates, random order)
    */
   def checkTerm(implicit lib : Lookup, home : MPath, s : Term, univ : Universe) : List[Path] = {
      lookupTheory(lib, home)
      checkTerm(lib, home, Context(), s, IsEqualTo(univ)).distinct
   }
   //TODO redesign role checking, currently not done
   private def checkTerm(lib : Lookup, home : MPath, context : Context, s : Term, uvcheck : UnivCheck) : List[Path] = {
      s match {
         case OMS(path) =>
            val s = try {lib.get(path)}
                    catch {case GetError(msg) =>
                                throw Invalid("ill-formed constant reference: " + msg)
                    }
            s match {
               case a : Alias =>
                  if (! lib.imports(a.parent, home))
                     throw Invalid("constant " + s + " is not imported into home theory " + home)
                  List(path)
               case c : Constant =>
                  if (! lib.imports(c.parent, home))
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
            val occf = checkTerm(lib, home, context, fun, IsSemantic) //TODO level of application cannot be inferred
            val occa = args.flatMap(checkTerm(lib, home, context, _, IsSemantic))
            occf ::: occa
         case OMBINDC(binder, bound, condition, scope) =>
            val newcontext = context ++ bound
            val occb = checkTerm(lib, home, context, binder, IsEqualTo(Binder))
            val occv = bound.variables.flatMap {
               // not checking the attributions
               case TermVarDecl(_, tp, df, attrs @ _*) => List(tp,df).filter(_.isDefined).map(_.get).flatMap(
                     checkTerm(lib, home, newcontext, _, IsSemantic)
               )
            }
            val occc = if (condition.isDefined) checkTerm(lib, home, newcontext, condition.get, IsSemantic)
               else Nil
            val occs = checkTerm(lib, home, newcontext, scope, IsSemantic)
            occb ::: occv.toList ::: occc ::: occs
         case OMATTR(arg, key, value) =>
            val occa = checkTerm(lib, home, context, arg, uvcheck)
            val occk = checkTerm(lib, home, context, key, IsEqualTo(Key))
            val occv = checkTerm(lib, home, context, value, IsSemantic)
            occa ::: occk ::: occv
         case OMM(arg, morph) =>
            val (occm, from, to) = inferMorphism(lib, morph)
            if (! lib.imports(to, home))
               throw Invalid("codomain of morphism is not imported into expected home theory")
            val occa = checkTerm(lib, from, context, arg, uvcheck) // using the same context because variable attributions are ignored anyway
            occm ::: occa
         case OMHID() => Nil//TODO roles
         case OME(err, args) =>
            val occe = checkTerm(lib, home, context, err, IsEqualTo(Error))
            val occa = args.flatMap(checkTerm(lib, home, context, _, IsSemantic))
            occe ::: occa
         case OMFOREIGN(node) => Nil //TODO roles, dependencies?
         case OMI(i) => Nil //TODO roles; check import of pseudo-theories, dependencies?
         case OMSTR(s) => Nil //TODO roles; check import of pseudo-theories, dependencies?
         case OMF(d) => Nil //TODO roles; check import of pseudo-theories, dependencies?
      }
   }
   
   /** checks whether a theory object is well-formed relative to a library
    *  @param lib the library
    *  @param t the theory
    *  @return the list of identifiers occurring in t
    */
   def checkTheo(lib : Lookup, t : TheoryObj) : List[Path] = t match {
     case OMT(p) =>
       lookupTheory(lib, p)
       List(p)
   }
   
   /** checks whether a morphism object is well-formed relative to a library and infers its type
    *  @param lib the library
    *  @param m the theory
    *  @return the list of named objects occurring in m, the domain and codomain of m
    */
   def inferMorphism(lib : Lookup, m : Morph) : (List[Path], MPath, MPath) = m match {
     case OMIDENT(OMT(t)) => (List(t), t,t)
     case OML(m : MPath) =>
        val l = lib.getLink(m)
        (List(m), l.from, l.to)
     case OMCOMP(hd) => inferMorphism(lib, hd)
     case OMCOMP(hd, tl @ _*) =>
        val (l1, r,s1) = inferMorphism(lib, hd)
        val (l2, s2,t) = inferMorphism(lib, OMCOMP(tl.head, tl.tail : _*))
        if (lib.imports(s1,s2))
           (l1 ::: l2, r, t)
        else
           throw Invalid("ill-formed morphism: " + hd + " cannot be composed with " + tl)
   }
   /** checks whether a morphism object is well-formed relative to a library, a domain and a codomain
    *  @param lib the library
    *  @param m the morphism
    *  @param dom the domain
    *  @param cod the codomain
    *  @return the list of identifiers occurring in m
    */
   def checkMorphism(lib : Lookup, m : Morph, dom : MPath, cod : MPath) : List[Path] = {
      val (l, d,c) = inferMorphism(lib, m)
      if (! lib.imports(dom, d) || ! lib.imports(c, cod))
         throw Invalid("ill-formed morphism: expected " + dom + " -> " + cod + ", found " + c + " -> " + d)
      l
   }
}
