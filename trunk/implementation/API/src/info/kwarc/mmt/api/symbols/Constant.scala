package info.kwarc.mmt.api.symbols
import jomdoc._
import jomdoc.objects._
import jomdoc.modules._
import jomdoc.patterns._
import jomdoc.presentation.{StringLiteral,Omitted}

/**
 * A Constant represents an MMT constant.<p>
 * 
 * @param parent the {@link jomdoc.names.Path} of the parent theory
 * @param name the name of the constant
 * @param tp the optional type
 * @param df the optional definiens
 * @param role the role of the constant
 */
class Constant(parent : MPath, name : LocalPath,
               val tp : Option[Term], val df : Option[Term], val uv : Universe, val genFrom : Option[Symbol]) extends Symbol(parent, name) {
  def toTerm = OMS(path)

  def role = jomdoc.Role_Constant(uv)
  def components = List(OMS(path), tp.getOrElse(Omitted), df.getOrElse(Omitted))
  
  def toNode =
     <constant name={name.flat}>
       {if (tp.isDefined) <type>{tp.get.toOBJNode}</type> else Nil}
       {if (df.isDefined) <definition>{df.get.toOBJNode}</definition> else Nil}
     </constant>
  override def toString = name + tp.map(" : " + _).getOrElse("") + df.map(" = " + _).getOrElse("")
}