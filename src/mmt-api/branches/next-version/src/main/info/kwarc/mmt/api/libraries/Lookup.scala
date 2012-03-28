package info.kwarc.mmt.api.libraries
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.patterns._
import info.kwarc.mmt.api.utils._

/** A read-only abstraction of a library. A Library is a Lookup with write methods */
abstract class Lookup(val report : frontend.Report) {
   def apply(path : Path) = get(path)

   def get(path : Path) : ContentElement
   /** Same as get, but returns an option
    * @return Some(content) if get succeeds, None if get throws an error
    */
   def getO(path: Path) : Option[ContentElement] = try {Some(get(path))} catch {case GetError(_) => None}
   //typed access methods
   private def defmsg(path : Path) : String = "no element of required type found at " + path
   def getModule(path : ContentPath, msg : Path => String = defmsg) : Module =
     get(path) match {case m: Module => m case _ => throw GetError(msg(path))}
   def getTheory(path : ContentPath, msg : Path => String = defmsg) : Theory =
     get(path) match {case t: Theory => t case _ => throw GetError(msg(path))}
   def getLink(path : ContentPath, msg : Path => String = defmsg) : Link =
     get(path) match {case e : Link => e case _ => throw GetError(msg(path))}
   def getSymbol(path : GlobalName, msg : Path => String = defmsg) : Symbol =
     get(path) match {case e : Symbol => e case _ => throw GetError(msg(path))} 
   def getConstant(path : GlobalName, msg : Path => String = defmsg) : Constant =
     get(path) match {case e : Constant => e case _ => throw GetError(msg(path))} 
   def getStructure(path : GlobalName, msg : Path => String = defmsg) : Structure =
     get(path) match {case e : Structure => e case _ => throw GetError(msg(path))} 
   def getConstantAssignment(path : GlobalName, msg : Path => String = defmsg) : ConstantAssignment =
     get(path) match {case e : ConstantAssignment => e case _ => throw GetError(msg(path))} 
   def getDefLinkAssignment(path : GlobalName, msg : Path => String = defmsg) : DefLinkAssignment =
     get(path) match {case e : DefLinkAssignment => e case _ => throw GetError(msg(path))} 
   def getPattern(path : GlobalName, msg: Path => String = defmsg) : Pattern = 
     get(path) match {case e : Pattern => e case _ => throw GetError(msg(path))}
   /* The above methods should be polymorphic in the return type like this:
      def get[A <: ContentElement](p : Path) : A = {
         get(p) match {
            case a : A => a
            case _ => throw GetError("bad role")
         }
      }
   *  But we cannot case-split over an abstract type parameter due to Scala's compilation-time type erasure.
   */
   /*
   def localDomain(th : ModuleObj) : Iterator[LocalPath]

   def globalDomain(th : ModuleObj) : Iterator[LocalPath]
   
   def localImports(th : TheoryObj) : List[TheoryObj]
   
   def localImports(m : Morph) : List[Morph]
   
   def globalImports(th : TheoryObj) : Iterator[TheoryObj]
   
   def globalImports(m : Morph) : Iterator[Morph]
    */
   
   def imports(from: Term, to: Term) : Boolean
   def importsTo(to: Term) : Iterator[Term]
   //def getSymbolNoAlias(path : Path) : Symbol = resolveAlias(getSymbol(path)) 
   //def structureModToSym(p : MPath) : SPath
   //def resolveAlias(s : Symbol) : Symbol
   
 /*  def imports(from : MPath, to : MPath) : Boolean
   //def importsFrom(from : MPath) : scala.collection.mutable.Set[MPath]
   def importsTo(to : MPath) : List[objects.ModuleObj] */
   /** if p is imported by a structure, returns the preimage of the symbol under the outermost structure */
   def preImage(p : GlobalName) : Option[GlobalName]
   
   
   /**
    * A Traverser that recursively expands definitions of Constants.
    * It carries along a test function that is used to determine when a constant should be expanded. 
    */
   object ExpandDefinitions extends Traverser[ContentPath => Boolean] {
      def doTerm(t: Term)(implicit con: Context, expand: ContentPath => Boolean) = t match {
         case OMID(p: GlobalName) if expand(p) => getConstant(p).df match {
            case Some(t) => doTerm(t)
            case None => OMID(p)
         }
         case t => Traverser.doTerm(this, t)(con, expand)
      }
   }
   
   /**
    * A Traverser that recursively eliminates all explicit morphism applications.
    * apply(t,m) can be used to apply a morphism to a term.
    */                                     // TODO term
   object ApplyMorphs extends Traverser[Term] {
      def doTerm(t: Term)(implicit con: Context, morph: Term) = t match {
         case OMM(arg, via) => doTerm(arg)(con, morph * via)
         case OMID(theo % ln) =>
           val t = getConstantAssignment(morph % ln).target
           doTerm(t)
         case t => Traverser.doTerm(this,t)(con, morph)
      }
   }
}