package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
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
   val fromPath : MPath
   val from = OMMOD(fromPath)
   val to = home
   def toTerm = if (name.isAnonymous) OMIDENT(to) else OMDL(home, name)
   protected def outerComponents = List(
         StringLiteral(if (name.isAnonymous) "include" else name.toString),
         StringLiteral(fromPath.toPath))
   protected def outerString = if (name.isAnonymous) "include " + from.toString else path + " : " + from.toString
   def toNode =
         <import name={if (name.isAnonymous) null else name.toPath} from={fromPath.toPath} implicit={if (! name.isAnonymous && isImplicit) "true" else null}>
            {getMetaDataNode}
            {innerNodes}
          </import>
}

/**
 * A DeclaredStructure represents an MMT structure given by a list of assignments.<p>
 * 
 * @param home the [[info.kwarc.mmt.api.objects.Term]] representing the parent theory
 * @param name the name of the structure
 * @param from the domain theory
 * @param isImplicit true iff the link is implicit
 */
class DeclaredStructure(val home : Term, val name : LocalName, val fromPath : MPath, val isImplicit : Boolean)
      extends Structure with DeclaredLink {
   def role = info.kwarc.mmt.api.Role_Structure
   /** override in order to permit implicit structures (identified by their domain) */
   override def implicitKey = Some(fromPath)
}

 /**
  * A DefinedStructure represents an MMT structure given by an existing morphism.<p>
  * 
  * @param home the [[info.kwarc.mmt.api.objects.Term]] representing the parent theory
  * @param name the name of the structure
  * @param from the domain theory
  * @param df the definiens
  * @param isImplicit true iff the link is implicit
  */
class DefinedStructure(val home : Term, val name : LocalName, val fromPath : MPath, val df : Term, val isImplicit : Boolean)
      extends Structure with DefinedLink {
   def role = info.kwarc.mmt.api.Role_DefinedStructure
}

object Include {
   def apply(home: Term, from: MPath) = new DeclaredStructure(home, LocalName.Anon, from, true)
   def unapply(t: ContentElement) : Option[(Term,MPath)] = t match {
      case d: DeclaredStructure if d.name.isAnonymous => Some((d.to, d.fromPath)) //TODO can there be assignments?
      case _ => None
   }
}

/**
 * A PlainInclude represents an MMT inclusion between theories.<p>
 *
 * @param from the domain of the inclusion
 * @param to the codomain of the inclusion
 */
object PlainInclude {
   def apply(from : MPath, to : MPath) = Include(OMMOD(to), from)
   def unapply(t: ContentElement) : Option[(MPath,MPath)] = t match {
      case Include(OMMOD(to), from) => Some((from, to))
      case _ => None
   }
}