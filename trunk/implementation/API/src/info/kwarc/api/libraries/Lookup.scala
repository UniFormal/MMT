package jomdoc.libraries
import jomdoc._
import jomdoc.modules._
import jomdoc.symbols._
import jomdoc.patterns._
import jomdoc.utils._

abstract class Lookup(val report : frontend.Report) {
   def apply(path : Path) = get(path)

   
   def get(path : Path) : ContentElement
   //typed access methods -- note that these cannot be polymorphic in the return type (see below)
   private def defmsg(path : Path) : String = "no element of required type found at " + path
   def getTheory(path : Path, msg : Path => String = defmsg) : Theory =
     get(path) match {case e : Theory => e case _ => throw GetError(msg(path))}
   def getLink(path : Path, msg : Path => String = defmsg) : Link =
     get(path) match {case e : Link => e case _ => throw GetError(msg(path))}
   def getSymbol(path : Path, msg : Path => String = defmsg) : Symbol =
     get(path) match {case e : Symbol => e case _ => throw GetError(msg(path))} 
   def getConstant(path : Path, msg : Path => String = defmsg) : Constant =
     get(path) match {case e : Constant => e case _ => throw GetError(msg(path))} 
   def getStructure(path : Path, msg : Path => String = defmsg) : Structure =
     get(path) match {case e : Structure => e case _ => throw GetError(msg(path))} 
   def getConstantAssignment(path : Path, msg : Path => String = defmsg) : ConstantAssignment =
     get(path) match {case e : ConstantAssignment => e case _ => throw GetError(msg(path))} 
   def getStructureAssignment(path : Path, msg : Path => String = defmsg) : StructureAssignment =
     get(path) match {case e : StructureAssignment => e case _ => throw GetError(msg(path))} 
   def getPattern(path : Path, msg: Path => String = defmsg) : Pattern = 
     get(path) match {case e : Pattern => e case _ => throw GetError(msg(path))}

   def getSymbolNoAlias(path : Path) : Symbol = resolveAlias(getSymbol(path)) 
   /** It should be like this.
    *  But we cannot case-split over an abstract type parameter due to Scala's compilation-time type erasure.
   def get[A <: ContentElement](p : Path) : A = {
      get(p) match {
         case a : A => a
         case _ => throw GetError("bad role")
      }
   }
   */
   def structureModToSym(p : MPath) : SPath
   def resolveAlias(s : Symbol) : Symbol
   
   def imports(from : MPath, to : MPath) : Boolean
   //def importsFrom(from : MPath) : scala.collection.mutable.Set[MPath]
   def importsTo(to : MPath) : List[objects.ModuleObj]
   /** if p is imported by a structure, returns the preimage of the symbol under the outermost structure */
   def preImage(p : SPath) : Option[SPath]
}