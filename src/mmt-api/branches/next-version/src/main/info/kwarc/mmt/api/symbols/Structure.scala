package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.presentation._

/**
 * An Import: either Structure or Include
 */
trait DefinitionalLink extends Symbol with Link {
   val to = home
   def toMorph = OMDL(home, name)
   def applyTo(sym : Symbol) : Symbol = sym match {
      case c : Constant => new Constant(to, name / c.name, c.tp.map(_ * toMorph), c.df.map(_ * toMorph), c.rl, c.not)
      case s : Structure => new DefinedStructure(to, name / s.name, s.from, s.toMorph * toMorph)
      //case p : Pattern => null //TODO translate pattern
      //case i : Instance => null //TODO translate instance
   }
}

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
abstract class Structure extends DefinitionalLink {
   /**
    * computes induced symbols, compare the corresponding method in {@link info.kwarc.mmt.api.symbols.StructureAssignment}
    * @param sym a symbol of the domain theory of the structure
    * @return the induced symbol
    */
   protected def outerComponents = List(StringLiteral(name.flat), from)
   protected def outerString = path + " : " + from.toString
   def toNode = /*from.asPath match {
	    case Some(p) =>
         <structure name={name.flat} from={p}>
           {getMetaDataNode}
           {innerNodes}
         </structure>
	    case _ =>
         <structure name={name.flat}>
           {getMetaDataNode}
           <from>{from.toOBJNode}</from>
           {innerNodes}
         </structure>
     }   */ {
        if (from.toMPath.isGeneric)
          <structure name={name.flat} from={from.toMPath.toPath}>
            {getMetaDataNode}
            {innerNodes}
          </structure>
        else
          <structure name={name.flat}>
            {getMetaDataNode}
            <from>{from.toOBJNode}</from>
            {innerNodes}
          </structure>
   }
}

/**
 * A DeclaredStructure represents an MMT structure given by a list of assignments.<p>
 * 
 * @param home the {@link info.kwarc.mmt.api.objects.Term} representing the parent theory
 * @param name the name of the structure
 * @param from the domain theory
 * @param meta the optional meta-morphism
 */
class DeclaredStructure(val home : Term, val name : LocalName, val from : Term)
      extends Structure with DeclaredLink {
   def role = info.kwarc.mmt.api.Role_Structure
}

 /**
  * A DefinedStructure represents an MMT structure given by an existing morphism.<p>
  * 
  * @param home the {@link info.kwarc.mmt.api.objects.Term} representing the parent theory
  * @param name the name of the structure
  * @param from the domain theory
  * @param df the definiens
  */
class DefinedStructure(val home : Term, val name : LocalName, val from : Term, val df : Term)
      extends Structure with DefinedLink {
   def role = info.kwarc.mmt.api.Role_DefinedStructure
}

/**
 * An Include represents an MMT inclusion into a theory.<p>
 *
 * @param home the codomain of the inclusion
 * @param from the included theory
 */                                // TODO term
case class Include(val home: Term, val from: Term) extends DefinitionalLink with IncludeLink {
   val name = LocalName(IncludeStep(from))
   val role = Role_Include
   protected def outerComponents : List[Content] = from match {
      case OMMOD(p) => List(from, StringLiteral(p.toPath))
      case _ => List(from)
   }
   protected def outerString : String = "include " + from  
   def toNode = from match {
      case OMMOD(p) => 
        <include from={p.toPath}>
            {getMetaDataNode}
        </include>
      case _ => 
        <include>
            {getMetaDataNode}
            {from.toOBJNode}
        </include>
   }
}

/**
 * A PlainInclude represents an MMT inclusion between theories.<p>
 *
 * @param from the domain of the inclusion
 * @param to the codomain of the inclusion
 */                        // TODO change MPath to Term?
object PlainInclude {
   def apply(from : MPath, to : MPath) = Include(OMMOD(to), OMMOD(from))
   def unapply(t: Include) : Option[(MPath,MPath)] = t match {
      case Include(OMMOD(to), OMMOD(from)) => Some((from, to))
      case _ => None
   }
}