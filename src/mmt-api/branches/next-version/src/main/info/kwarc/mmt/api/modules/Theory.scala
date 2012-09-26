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
      extends Theory(doc, name) with DeclaredModule[Symbol] {
   def role = Role_DeclaredTheory
   def components = OMID(path) :: meta.map(objects.OMMOD(_)).getOrElse(Omitted) :: innerComponents
   override def compNames = List(("name", 0), ("meta",1))
   override def toString = path + meta.map(" : " + _.toPath).getOrElse("") + innerString
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

class DefinedTheory(doc : DPath, name : LocalPath, val df : Term) extends Theory(doc, name) with DefinedModule {
   def role = Role_DefinedTheory
   def components = StringLiteral(name.flat) :: innerComponents
   override def toString = path + innerString
   def toNode = 
    <theory name={name.flat} base={doc.toPath}>
        {getMetaDataNode}
        {innerNodes}
    </theory>
}