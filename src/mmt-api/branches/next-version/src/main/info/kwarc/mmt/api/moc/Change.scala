package info.kwarc.mmt.api.moc

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._

import scala.xml.Node



abstract class Change {
  def toNode : Node
}

class Diff(val changes : List[ContentElementChange]) {
   def toNode = <omdoc-diff> {changes.map(_.toNode)} </omdoc-diff>
   
   def toNodeFlat = <omdoc-diff> {changes.flatMap(_.toNodeFlat)} </omdoc-diff>
   
   def ++(d : Diff) = new Diff(changes ++ d.changes)
}

abstract class Add extends Change
abstract class Update extends Change
abstract class Delete extends Change
abstract class Rename extends Change


trait ContentElementChange extends Change {
    def toNodeFlat : List[Node]
}

/**
 * Module Level
 */

trait ChangeModule extends ContentElementChange {
  def toNode : Node
  def toNodeFlat : List[Node]
}

case class AddModule(m : Module) extends Add with ChangeModule {
  def toNode =
    <module change="add">
      {m.toNode}
    </module>

  def toNodeFlat = <change type="add" path={m.path.toPath}> {m.toNode} </change> :: Nil

}

case class DeleteModule(m : Module) extends Delete with ChangeModule {
  def toNode = 
    <module change="delete" path={m.path.toPath}/>
    
  def toNodeFlat = 
    <change type="delete" path={m.path.toPath}/> :: Nil
}

case class RenameModule(oldpath : MPath, nwpath : MPath) extends Rename with ChangeModule {
  def toNode = 
    <module change="rename" oldpath={oldpath.toPath} newpath={nwpath.toString}/>
  
  def toNodeFlat = 
    <change type="rename" oldpath={oldpath.toPath} newpath={nwpath.toString}/> :: Nil

}

/**
 * Declaration Level
 */

trait ChangeDeclaration extends ContentElementChange {
  def toNode : Node
  def toNodeFlat : List[Node]
}


case class AddDeclaration(d : Declaration) extends Add with ChangeDeclaration {
  def toNode = 
    <declaration change="add">
  		{d.toNode}
	</declaration>  
  
  def toNodeFlat = <change type="add" path={d.path.toPath}> {d.toNode} </change> :: Nil
}

case class DeleteDeclaration(d : Declaration) extends Delete with ChangeDeclaration {
  def toNode =
    <declaration change="delete" path={d.path.toPath}/>
    
  def toNodeFlat = 
    <change type="delete" path={d.path.toPath}/> :: Nil
}

case class RenameDeclaration(path : GlobalName, name : LocalName) extends Rename with ChangeDeclaration {
  def toNode =
    <declaration change="rename" path={path.toPath} name={name.toString}/>
  def toNodeFlat = 
    <change type="rename" path={path.toPath} name={name.toString}/> :: Nil
}


/**
 * Object Level
 */


case class Component(c : Option[Obj])

case class UpdateComponent(path : Path, name : String, old : Option[Obj], nw : Option[Obj]) extends Update with ContentElementChange {
  def toNode =
  	<component name={name} change="update">
  	</component>
//  	  {o.toNode(changes)}
  
  def toNodeFlat =
    <change type="update" path={path.toString} component={name}> //TODO </change> :: Nil
}



/* Declaration 
case class TermComponent(c : Term) extends Component

case class MorphComponent(c : Morph) extends Component

case class TypeComponent(c : Option[Term]) extends Component

case class ThyObjComponent(c : TheoryObj) extends Component

case class MetaComponent(c : Option[Path]) extends Component

case class ContextComponent(c : Context) extends Component

case class GlobalNameComponent(c : GlobalName) extends Component

case class SubstitutionComponent(c : Substitution) extends Component
*/
/*
case class Component(o : Option[Obj]) {

  def toNode = <OMOBJ xmlns:om="om">{o.map(_.toNode)}</OMOBJ>
  def toNode(changes : List[(Position, Obj)]) = {
    val nodeChanges : List[Node] = changes.map(c => <change pos={c._1.toString}>{c._2.toNode}</change>)
    <OMOBJ xmlns:om="om">{o.map(_.toNode)}<changes>{nodeChanges}</changes></OMOBJ>
  }
  
  def tp : String = o match {
    case t : Term => "Term"
    case o : OMMOD => "OMMOD"       
    case o : TheoryObj => "TheoryObj"
    case _ => throw ImplementationError("object type not supported " + o.toString)
  }
}
*/










