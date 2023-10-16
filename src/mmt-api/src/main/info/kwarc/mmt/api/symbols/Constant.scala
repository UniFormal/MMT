package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import objects._
import notations._
import modules._
import moc._

/**
 * the abstract interface to MMT constants with a few basic methods
 */
abstract class Constant extends Declaration with HasType with HasDefiniens with HasNotation {
  val feature = "constant"
  def alias: List[LocalName]
  def tpC: TermContainer
  def dfC: TermContainer
  def rl : Option[String]
   
  def vs: Visibility

  override def alternativeNames = alias

  def getComponents = List(TypeComponent(tpC), DefComponent(dfC)) ::: notC.getComponents
  def getDeclarations = Nil

  def toNode =
     <constant name={name.toPath} alias={if (alias.isEmpty) null else alias.map(_.toPath).mkString(" ")} role={rl.getOrElse(null)}>
       {getMetaDataNode}
       {tpNode}
       {dfNode}
       {notNode}
     </constant>
  override def toString = name.toString + alias.map(" @ " + _).mkString(" ") +
     tp.map(" : " + _).getOrElse("") + df.map(" = " + _).getOrElse("") + notC.toString

  type ThisType = Constant

  // finalizes the Constant if it is not final
  def translate(newHome: Term, prefix: LocalName, except: LocalName, translator: Translator, context : Context): FinalConstant = {
     Constant(
         newHome, prefix.appendExcept(except, name), alias.map(prefix / _),
         translateTp(translator, context),
         translateDf(translator, context),
         rl, notC
     )
  }
  // may finalize the Constant if it is not final
  def merge(that: Declaration): Constant = that match {
    case that: Constant =>
      val aliasM = that.alias:::this.alias
      val tpM = that.tpC merge this.tpC
      val dfM = that.dfC merge this.dfC
      val notM = that.notC merge this.notC
      val rlM = that.rl orElse this.rl
      val vsM = that.vs merge this.vs
      new FinalConstant(this.home, this.name, aliasM, tpM, dfM, rlM, notM, vsM)
    case _ => mergeError(that)
  }
}

/** visibility information for a [[Constant]]
 *  @param tp type is visible
 *  @param _df definiens is visible (must be invisible if type is)
 */
case class Visibility(tp: Boolean, private val _df: Boolean) {
  val df = tp && _df
  /** lower visibility prevails */
  def merge(that: Visibility) = Visibility(tp && that.tp, df && that.df)
}

object Visibility {
  val public = Visibility(true, true)
}

/**
 * the main class for a concrete MMT constant
 *
 * @param home the parent theory
 * @param name the name of the constant
 * @param alias an alternative (usually shorter) name
 * @param tpC the type container optionally containing a type
 * @param dfC the definiens container optionally containing a definiens
 * @param rl the role of the constant
 */
class FinalConstant(val home : Term, val name : LocalName, val alias: List[LocalName],
               val tpC : TermContainer, val dfC : TermContainer, val rl : Option[String], val notC: NotationContainer, val vs: Visibility) extends Constant {
}

/** helper object */
object Constant {
   /** factory that hides the TermContainer's
    *
    * all arguments are as in the primary constructor, except the terms, which are wrapped in the
    * TermContainer factory
    */
   def apply(home : Term, name : LocalName, alias: List[LocalName], tp: Option[Term], df: Option[Term],
             rl : Option[String], not: NotationContainer = NotationContainer.empty()) =
      new FinalConstant(home, name, alias, TermContainer(tp), TermContainer(df), rl, not, Visibility.public)
   def apply(home : Term, name : LocalName, alias: List[LocalName],
             tpC : TermContainer, dfC : TermContainer, rl : Option[String], notC: NotationContainer) =
      new FinalConstant(home, name, alias, tpC, dfC, rl, notC, Visibility.public)
}
