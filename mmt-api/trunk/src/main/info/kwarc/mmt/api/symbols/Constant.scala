package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import objects._
import modules._
import moc._
import parser.TextNotation
import presentation.{NotationProperties, StringLiteral, Omitted}

/**
 * A Constant represents an MMT constant.<p>
 * 
 * @param home the {@link info.kwarc.mmt.api.objects.Term} representing the parent theory
 * @param name the name of the constant
 * @param tp the optional type
 * @param df the optional definiens
 * @param rl the role of the constant
 */
class Constant(val home : Term, val name : LocalName,
               val tp : Option[Term], val df : Option[Term], val rl : Option[String], val not: Option[TextNotation]) extends Symbol {
  def toTerm = OMID(path)

  def role = Role_Constant(rl)
  override def compNames = List(("name", 0), ("type",1), ("definition", 2))
  def components = List(OMID(path), tp.getOrElse(Omitted), df.getOrElse(Omitted),
                                    rl.map(StringLiteral(_)).getOrElse(Omitted))
  
  def toNode =
     <constant name={name.toPath} role={rl.getOrElse(null)}>
       {getMetaDataNode}
       {if (tp.isDefined) <type>{tp.get.toOBJNode}</type> else Nil}
       {if (df.isDefined) <definition>{df.get.toOBJNode}</definition> else Nil}
       {if (not.isDefined) <notation>{not.get.toNode}</notation> else Nil}
     </constant>
  override def toString = name.toString + tp.map(" : " + _).getOrElse("") + df.map(" = " + _).getOrElse("")
  
}