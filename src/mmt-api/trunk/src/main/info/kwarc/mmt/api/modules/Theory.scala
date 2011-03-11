package info.kwarc.mmt.api.modules
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.presentation.{StringLiteral,Omitted}

/**
 * A Theory represents an MMT theory.<p>
 * 
 * Theories are constructed empty. {@link info.kwarc.mmt.api.modules.StatementSet[Symbol]} is derived to hold a set of named symbols.
 * 
 * @param doc the {@link info.kwarc.mmt.api.names.Path} of the parent document
 * @param name the name of the theory
 * @param meta the path of the optional meta-theory
 */
class Theory(doc : DPath, name : LocalPath, val meta : Option[MPath])
      extends Module(doc, name) with StatementSet[Symbol, TheoImport] {
   def components = StringLiteral(name.flat) :: meta.map(objects.OMT(_)).getOrElse(Omitted) :: valueList
   def role = info.kwarc.mmt.api.Role_Theory
   def toNode =
      <theory name={name.flat} cdbase={doc.toPath} meta={if (meta.isDefined) meta.get.toPath else null}>
         {valueList.map(_.toNode)}
      </theory>
   override def toString = path + " = " + valueList.map("\t" + _.toString).mkString("{\n","\n","\n}")
}