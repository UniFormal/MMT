package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.patterns._
import info.kwarc.mmt.api.presentation.{StringLiteral,Omitted}
import info.kwarc.mmt.api.moc._

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
               val tp : Option[Term], val df : Option[Term], val rl : Option[String]) extends Symbol {
  def toTerm = OMID(path)

  def role = Role_Constant(rl)
  override def compNames = List(("name", 0), ("type",1), ("definition", 2))
  def components = List(OMID(path), tp.getOrElse(Omitted), df.getOrElse(Omitted),
                                    rl.map(StringLiteral(_)).getOrElse(Omitted))
  
  def toNode =
     <constant name={name.flat} role={rl.getOrElse(null)}>
       {getMetaDataNode}
       {if (tp.isDefined) <type>{tp.get.toOBJNode}</type> else Nil}
       {if (df.isDefined) <definition>{df.get.toOBJNode}</definition> else Nil}
     </constant>
  override def toString = name + tp.map(" : " + _).getOrElse("") + df.map(" = " + _).getOrElse("")
  
}