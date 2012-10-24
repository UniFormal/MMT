package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.presentation.{StringLiteral}

/**
 * An Alias represents an MMT alias.
 * @param home the module in which the alias is declared
 * @param name the new name
 * @param forpath the path the new name abbreviates
 */
class Alias(val home: Term, val name : LocalName, val forpath : GlobalName) extends Symbol {
  def role = info.kwarc.mmt.api.Role_Alias
  def components = List(StringLiteral(name.toPath), OMID(forpath))
  
  def toNode =
    <alias name={name.toPath} for={forpath.toPath}>
        {getMetaDataNode}
    </alias> 
  override def toString = name + " => " + forpath.toPath
}
