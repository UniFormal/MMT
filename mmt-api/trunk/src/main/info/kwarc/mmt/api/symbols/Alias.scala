package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.presentation.{StringLiteral}

/**
 * An Alias represents an MMT alias.
 * @param parent the module in which the alias is declared
 * @param name the new name
 * @param forpath the path the new name abbreviates
 */
class Alias(parent : MPath, name : LocalPath, val forpath : SPath) extends Symbol(parent, name) {
  def role = info.kwarc.mmt.api.Role_Alias
  def components = List(StringLiteral(name.flat), OMS(forpath))
  
  def toNode =
     <alias name={name.flat} for={forpath.toPath}/>
  override def toString = name + " => " + forpath.toPath
}
