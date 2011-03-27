package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.patterns._
import info.kwarc.mmt.api.presentation._

/**
 * A Structure represents an MMT structure.<p>
 * 
 * Structures be declared (given by a list of assignments) or defined (given by an existing morphism).
 * These cases are distinguished by which subtrait of Link is mixed in.
 * 
 * @param parent the {@link info.kwarc.mmt.api.names.Path} of the parent theory (also the codomain of the link)
 * @param name the name of the view
 * @param from the domain theory
 */
abstract class Structure extends Symbol with Link {
   val to = OMT(parent)
   def toMorph = OML(parent / name)
   /**
    * computes induced symbols, compare the corresponding method in {@link info.kwarc.mmt.api.symbols.StructureAssignment}
    * @param sym a symbol of the domain theory of the structure
    * @return the induced symbol
    */
   def applyTo(sym : Symbol) : Symbol = sym match {
      case c : Constant => new Constant(to, name / c.name, c.tp.map(_ * toMorph), c.df.map(_ * toMorph), c.uv, c.genFrom.map(applyTo))
      case s : Structure => new DefinedStructure(to, name / s.name, s.from, s.toMorph * toMorph)
      case p : Pattern => null //TODO translate pattern
      case i : Instance => null //TODO translate instance
   }
   protected def outerComponents = List(StringLiteral(name.flat), from)
   protected def outerString = path + " : " + from.toString
   def toNode = from.asPath match {
	   case Some(p) =>
         <structure name={name.flat} from={p.toPath}>
           {innerNodes}
         </structure>
	   case _ =>
         <structure name={name.flat}>
           <from>{from.toOBJNode}</from>
           {innerNodes}
         </structure>
   }
}

/**
 * A DeclaredStructure represents an MMT structure given by a list of assignments.<p>
 * 
 * @param parent the {@link info.kwarc.mmt.api.names.Path} of the parent theory
 * @param name the name of the structure
 * @param from the domain theory
 * @param meta the optional meta-morphism
 */
class DeclaredStructure(val parent : MPath, val name : LocalName, val from : TheoryObj)
      extends Structure with DeclaredLink {
   def role = info.kwarc.mmt.api.Role_Structure
}

 /**
  * A DefinedStructure represents an MMT structure given by an existing morphism.<p>
  * 
  * @param parent the {@link info.kwarc.mmt.api.names.Path} of the parent theory
  * @param name the name of the structure
  * @param from the domain theory
  * @param df the definiens
  */
class DefinedStructure (val parent : MPath, val name : LocalName, val from : TheoryObj, val df : Morph)
      extends Structure with DefinedLink {
   def role = info.kwarc.mmt.api.Role_DefinedStructure
}
 