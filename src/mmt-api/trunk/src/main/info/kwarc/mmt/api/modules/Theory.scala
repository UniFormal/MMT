package info.kwarc.mmt.api.modules
import info.kwarc.mmt.api._
import symbols._
import libraries._
import objects._
import utils._
import presentation.{StringLiteral,Omitted}

abstract class Theory(doc : DPath, name : LocalPath) extends Module(doc, name)
/**
 * A Theory represents an MMT theory.<p>
 * 
 * Theories are constructed empty. {@link info.kwarc.mmt.api.modules.StatementSet[Symbol]} is derived to hold a set of named symbols.
 * 
 * @param doc the {@link info.kwarc.mmt.api.names.Path} of the parent document
 * @param name the name of the theory
 * @param meta the path of the optional meta-theory
 */
class DeclaredTheory(doc : DPath, name : LocalPath, var meta : Option[MPath])
      extends Theory(doc, name) with DeclaredModule {
   def role = Role_DeclaredTheory
   def components = OMID(path) :: meta.map(objects.OMMOD(_)).getOrElse(Omitted) :: innerComponents
   /** convenience method to obtain all constants */
   def getConstants:List[Constant] = getDeclarations.flatMap {
      case c: Constant => List(c)
      case _ => Nil
   }
   /** convenience method to obtain all included theories (including a possible meta-theory) */
   def getIncludes:List[MPath] = meta.toList ::: getIncludesWithoutMeta 
   /** convenience method to obtain all included theories (without a possible meta-theory) */
   def getIncludesWithoutMeta:List[MPath] = {
    getDeclarations.flatMap {
        case PlainInclude(from, _) => List(from)
        case _ => Nil
     }
   }   
   /** convenience method to obtain all named structures */
   def getNamedStructures:List[Structure] = getDeclarations.flatMap {
      case s: Structure if ! s.isAnonymous => List(s)
      case _ => Nil
   }   
   /** convenience method to obtain all patterns */
   def getPatterns:List[patterns.Pattern] = getDeclarations.flatMap {
      case p: patterns.Pattern => List(p)
      case _ => Nil
   }
   /** convenience method to obtain all pattern instances */
   def getInstances:List[patterns.Instance] = getDeclarations.flatMap {
      case p: patterns.Instance => List(p)
      case _ => Nil
   }
   override def compNames = List(("name", 0), ("meta",1))
   override def toString = "theory " + path + meta.map(" : " + _.toPath).getOrElse("") + innerString
   def toNode =
      <theory name={name.flat} base={doc.toPath} meta={if (meta.isDefined) meta.get.toPath else null}>
        {getMetaDataNode}
        {innerNodes}
      </theory>
   def toNodeElab = 
    <theory name={name.flat} base={doc.toPath}>
        {getMetaDataNode}
        {innerNodesElab}
    </theory>
}

class DefinedTheory(doc : DPath, name : LocalPath, val dfC : TermContainer) extends Theory(doc, name) with DefinedModule {
   def role = Role_DefinedTheory
   def components = StringLiteral(name.flat) :: innerComponents
   override def toString = path + innerString
   def toNode = 
    <theory name={name.flat} base={doc.toPath}>
        {getMetaDataNode}
        {innerNodes}
    </theory>
}

object DefinedTheory {
   def apply(doc : DPath, name : LocalPath, df : Term) =
      new DefinedTheory(doc, name, TermContainer(df))
}