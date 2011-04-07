package info.kwarc.mmt.api.modules
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.presentation.{StringLiteral,Omitted}

abstract class Theory(doc : DPath, name : LocalPath) extends Module(doc, name) {
}
/**
 * A Theory represents an MMT theory.<p>
 * 
 * Theories are constructed empty. {@link info.kwarc.mmt.api.modules.StatementSet[Symbol]} is derived to hold a set of named symbols.
 * 
 * @param doc the {@link info.kwarc.mmt.api.names.Path} of the parent document
 * @param name the name of the theory
 * @param meta the path of the optional meta-theory
 */
class DeclaredTheory(doc : DPath, name : LocalPath, val meta : Option[MPath])
      extends Theory(doc, name) with DeclaredModule[Symbol] {
   def role = Role_DeclaredTheory
   def components = StringLiteral(name.flat) :: meta.map(objects.OMMOD(_)).getOrElse(Omitted) :: innerComponents
   override def toString = path + meta.map(" : " + _.toPath).getOrElse("") + innerString
   def toNode =
      <theory name={name.flat} cdbase={doc.toPath} meta={if (meta.isDefined) meta.get.toPath else null}>
         {innerNodes}
      </theory>
}

class DefinedTheory(doc : DPath, name : LocalPath, val df : TheoryObj) extends Theory(doc, name) with DefinedModule[TheoryObj] {
   def role = Role_DefinedTheory
   def components = StringLiteral(name.flat) :: innerComponents
   override def toString = path + innerString
   def toNode = <theory name={name.flat} cdbase={doc.toPath}>{innerNodes}</theory>
}