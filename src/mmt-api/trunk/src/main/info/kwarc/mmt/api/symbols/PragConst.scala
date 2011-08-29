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
class PragConst(val home : TheoryObj, val name : LocalName,
               val by : List[Constant], val means : List[Constant], val uv : Universe) extends Symbol {
  def toTerm = OMID(path)

  def role = info.kwarc.mmt.api.Role_Constant(uv)
  def components = List(OMID(path))//
 
  def toNode =
     <pragConst name={name.flat}>
       <by>{by.map(x => x.toNode)}</by>
       <means>{means.map(x => x.toNode)}</means>
     </pragConst>
  //override def toString = name + tp.map(" : " + _).getOrElse("") + df.map(" = " + _).getOrElse("")
}