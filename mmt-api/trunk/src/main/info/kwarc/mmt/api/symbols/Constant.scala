package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import objects._
import notations._
import modules._
import moc._

/**
 * the abstract interface to MMT constants with a few basic methods 
 */
abstract class Constant extends Declaration with HasNotation {
   def alias: Option[LocalName]
   def tpC: TermContainer
   def dfC: TermContainer
   def rl : Option[String]

  override def alternativeName = alias

  def tp = tpC.get
  def df = dfC.get
  
  def getComponents = List(TypeComponent(tpC), DefComponent(dfC)) ::: notC.getComponents
  def getDeclarations = Nil
  
  def toNode =
     <constant name={name.toPath} alias={alias.map(_.toPath).getOrElse(null)} role={rl.getOrElse(null)}>
       {getMetaDataNode}
       {if (tp.isDefined) <type>{tp.get.toOBJNode}</type> else Nil}
       {if (df.isDefined) <definition>{df.get.toOBJNode}</definition> else Nil}
       {notC.toNode}
     </constant>
  override def toString = name.toString + alias.map(" @ " + _).getOrElse("") +
     tp.map(" : " + _).getOrElse("") + df.map(" = " + _).getOrElse("") + notC.toString
}

/**
 * the main class for a concrete MMT constant
 * 
 * @param home the parent theory
 * @param name the name of the constant
 * @param alias an alternative (usually shorter) name
 * @param tp the optional type
 * @param df the optional definiens
 * @param rl the role of the constant
 */
class FinalConstant(val home : Term, val name : LocalName, val alias: Option[LocalName],
               val tpC : TermContainer, val dfC : TermContainer, val rl : Option[String], val notC: NotationContainer) extends Constant {
}

/** helper object */
object Constant {
   /** factory that hides the TermContainer's
    * 
    * all arguments are as in the primary constructor, except the terms, which are wrapped in the 
    * TermContainer factory
    */
   def apply(home : Term, name : LocalName, alias: Option[LocalName], tp: Option[Term], df: Option[Term],
             rl : Option[String], not: NotationContainer = NotationContainer()) =
      new FinalConstant(home, name, alias, TermContainer(tp), TermContainer(df), rl, not)
   def apply(home : Term, name : LocalName, alias: Option[LocalName],
             tpC : TermContainer, dfC : TermContainer, rl : Option[String], notC: NotationContainer) =
      new FinalConstant(home, name, alias, tpC, dfC, rl, notC)
}