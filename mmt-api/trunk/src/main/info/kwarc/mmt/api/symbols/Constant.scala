package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.patterns._
import info.kwarc.mmt.api.presentation.{StringLiteral,Omitted}

/**
 * A Constant represents an MMT constant.<p>
 * 
 * @param parent the {@link info.kwarc.mmt.api.names.Path} of the parent theory
 * @param name the name of the constant
 * @param tp the optional type
 * @param df the optional definiens
 * @param role the role of the constant
 */
class Constant(val home : TheoryObj, val name : LocalName,
               val tp : Option[Term], val df : Option[Term], val uv : Universe) extends Symbol {
  def toTerm = OMID(path)

  def role = info.kwarc.mmt.api.Role_Constant(uv)
  def components = List(OMID(path), tp.getOrElse(Omitted), df.getOrElse(Omitted))
  
  def toNode =
     <constant name={name.flat}>
       {getMetaDataNode}
       {if (tp.isDefined) <type>{tp.get.toOBJNode}</type> else Nil}
       {if (df.isDefined) <definition>{df.get.toOBJNode}</definition> else Nil}
     </constant>
  override def toString = name + tp.map(" : " + _).getOrElse("") + df.map(" = " + _).getOrElse("")
}