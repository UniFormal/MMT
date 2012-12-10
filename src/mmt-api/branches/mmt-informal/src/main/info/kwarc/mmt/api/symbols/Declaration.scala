package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.libraries._

/**
 * Declaration unifies MMT symbols and MMT assignments. These are the named statements.
 * 
 * @param parent the {@link info.kwarc.mmt.api.names.Path} of the parent theory or link, respectively
 * @param name the name of the symbol that is declared or instantiated, respectively
 */
sealed abstract class Declaration extends ContentElement {
   val parent = home.toMPath
   val home : Term
   val name : LocalName
   def path = GlobalName(home, name)
   /** the component used to identify anonymous declarations, e.g., the from of an import, None by default but may be overridden */ 
   def implicitKey : Option[Term] = None
}

/**
 * A Symbol represents an MMT symbol.<p>
 * 
 * @param home the {@link info.kwarc.mmt.api.objects.Term} representing the parent theory
 */
abstract class Symbol extends Declaration {
   val home : Term
}

/**
 * An Assignment represents an MMT assignment.<p>
 * 
 * @param home the {@link info.kwarc.mmt.api.objects.Term} representing the parent link
 */
abstract class Assignment extends Declaration {
   val home : Term
   //val target : Obj
}

class SFDeclaration(val home : Term, val tokens : List[SFDeclElem]) extends Declaration {
  val name = LocalName.Anon
  lazy val mpath = home.toMPath
  def components = home :: tokens.flatMap(_.components)
  def role = Role_SFDecl
  def toNode = <SFDeclaration home={mpath.toPath}>{tokens.map(_.toNode)}</SFDeclaration>
}