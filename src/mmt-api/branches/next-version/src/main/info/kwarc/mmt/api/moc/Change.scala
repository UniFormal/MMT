package info.kwarc.mmt.api.moc

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._

import scala.xml.Node
import collection.immutable.HashSet


abstract class Change {
  def toNode : Node
}


class Diff(val changes : List[ContentChange]) {
   def toNode = <omdoc-diff> {changes.map(_.toNode)} </omdoc-diff>
   
   def toNodeFlat = <omdoc-diff> {changes.flatMap(_.toNodeFlat)} </omdoc-diff>
   
   def ++(d : Diff) = new Diff(changes ++ d.changes)
}


class StrictDiff(override val changes : List[StrictChange]) extends Diff(changes) {
  def ++(d : StrictDiff) = new StrictDiff(changes ++ d.changes)
}


sealed trait ContentChange extends Change {
    def toNodeFlat : List[Node]
}

sealed abstract class StrictChange extends ContentChange {
  def getReferencedURI : Path
}

abstract class Add extends StrictChange
abstract class Update extends StrictChange
abstract class Delete extends StrictChange


class PragmaticChangeType(val name : String,
                          val matches : Diff => Boolean,
                          val termProp : Diff => Term => Term,
                          val modProp : Diff => Module => StrictDiff) {

  def make(diff : Diff) : Option[PragmaticChange] = matches(diff) match {
    case true => Some(new PragmaticChange(name, diff, termProp(diff), modProp(diff)))
    case false => None
  }
}

case class PragmaticChange(val name : String, val diff : Diff, val termProp : Term => Term, val modProp : Module => StrictDiff) extends ContentChange {
  def toNode =
    <PragmaticChange name={name}>
      {diff.toNode}
    </PragmaticChange>

  def toNodeFlat = diff.changes.flatMap(_.toNodeFlat)

  def toStrict : Diff = diff
}


object SamplePragmaticChanges {
  private val name = "ConstantRename"
  private def matches(diff : Diff) : Boolean = diff.changes match {
    case DeleteDeclaration(o : Constant) :: AddDeclaration(n : Constant) :: Nil =>
      o.name != n.name && o.tp == n.tp && o.df == n.df
    case _ => false
  }

  //TODO
  private def termProp(diff : Diff)(tm : Term) : Term = {
    tm
  }

  //TODO
  private def modProp(diff : Diff)(m : Module) : StrictDiff = {
    new StrictDiff(Nil)
  }

  val renameConstant = new PragmaticChangeType(name, matches, termProp, modProp)

  //val sampleRename = renameConstant.make(new Diff(Nil))

}


/**
 * Module Level
 */
trait ChangeModule extends ContentChange {
  def getReferencedURI : MPath
  def toNode : Node
  def toNodeFlat : List[Node]
}

case class AddModule(m : Module) extends Add with ChangeModule {
  def getReferencedURI = m.path
  def getAffectedCPaths = new HashSet[CPath]()
  def getAffectedPaths = new HashSet[ContentPath]()

  def toNode =
    <module change="add">
      {m.toNode}
    </module>

  def toNodeFlat = <change type="add" path={m.path.toPath}> {m.toNode} </change> :: Nil

}

case class DeleteModule(m : Module) extends Delete with ChangeModule {
  def getReferencedURI = m.path

  def toNode = 
    <module change="delete" path={m.path.toPath}/>
    
  def toNodeFlat = 
    <change type="delete" path={m.path.toPath}/> :: Nil
}


/**
 * Declaration Level
 */

trait ChangeDeclaration extends ContentChange {
  def getReferencedURI : GlobalName
  def toNode : Node
  def toNodeFlat : List[Node]
}


case class AddDeclaration(d : Declaration) extends Add with ChangeDeclaration {
  def getReferencedURI = d.path

  def toNode = 
    <declaration change="add">
  		{d.toNode}
	</declaration>  
  
  def toNodeFlat = <change type="add" path={d.path.toPath}> {d.toNode} </change> :: Nil
}

case class DeleteDeclaration(d : Declaration) extends Delete with ChangeDeclaration {
  def getReferencedURI = d.path

  def toNode =
    <declaration change="delete" path={d.path.toPath}/>
    
  def toNodeFlat = 
    <change type="delete" path={d.path.toPath}/> :: Nil
}


/**
 * Object Level
 */
case class Component(c : Option[Obj])

case class UpdateComponent(path : ContentPath, name : String, old : Option[Obj], nw : Option[Obj]) extends Update with ContentChange {

  def getReferencedURI : CPath = CPath(path,name)

  def toNode =
    <component path={path.toPath} name={name} change="update">
      {nw.map(_.toNode)}
    </component>
//  	  {o.toNode(changes)}
  
  def toNodeFlat =
    <change type="update" path={path.toString} component={name}>  {nw.map(_.toNode)}  </change> :: Nil
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










