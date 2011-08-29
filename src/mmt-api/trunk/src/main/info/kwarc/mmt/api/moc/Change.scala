package info.kwarc.mmt.api.moc

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._

import scala.collection.mutable.HashSet
import scala.xml.Node



abstract class Change {
  def toNode : Node
  //override def toString : String
}

class Diff(val cold : Controller, val cnew : Controller, val old : DPath, val nw : DPath) {
   var changes: List[Change] = Nil
   def toNode = 
     <omdoc-diff old={old.toPath} new={nw.toPath}>
	   {changes.map(_.toNode)}
	 </omdoc-diff>
}

trait Add extends Change
trait Update extends Change
trait Delete extends Change
trait Rename extends Change
trait Identical extends Change

/**
 * Document Level
 */
trait ChangeDocument extends Change {
  def toNode : Node
}

case class AddDocument(doc : Document) extends Add with ChangeDocument {
  def toNode = 
    <doc change="add">
	  {doc.toNode}
  	</doc>
}
case class UpdateDocument(val path : DPath, val changes : List[Change]) extends Update with ChangeDocument {
 def toNode = 
   <doc change="update" path={path.toPath}>
 	<changes>
 	  {changes.map(_.toNode)}
 	</changes>
   </doc>

}

case class DeleteDocument(path : DPath) extends Delete with ChangeDocument {
  def toNode = 
    <doc change="delete" path={path.toPath}/>
}
case class RenameDocument(path : DPath, name : String) extends Rename with ChangeDocument {
  def toNode = 
    <doc change="rename" path={path.toPath} name={name}/>
}
case class IdenticalDocument(path : DPath) extends Identical with ChangeDocument {
  def toNode = 
    <doc change="identical" path={path.toPath}/>
}

/**
 * Module Level
 */

trait ChangeModule extends Change {
  def toNode : Node
}

case class AddModule(m : Module, tp : String) extends Add with ChangeModule {
  def toNode = 
    <module type={tp} change="add">
	  {m.toNode}
	</module>
}


case class UpdateModule(path : MPath, tp : String, componentChanges : List[ChangeDeclaration], fieldChanges : List[ChangeComponent]) extends Update with ChangeModule { 
  def toNode = 
    <module type={tp} change="update" path={path.toPath}>

  	  {fieldChanges.map(s => s.toNode)}
  	  <bodyChanges>
  		{componentChanges.map(_.toNode)}
  	  </bodyChanges>
	</module>
}

case class DeleteModule(path : MPath, tp : String) extends Delete with ChangeModule {
  def toNode = 
    <module type={tp} change="delete" path={path.toPath}/>
}
case class RenameModule(path : MPath, tp : String, name : LocalPath) extends Rename with ChangeModule {
  def toNode = 
    <module type={tp}  change="rename" path={path.toPath} name={name.toString}/>
}
case class IdenticalModule(path : MPath, tp : String) extends Identical with ChangeModule {
  def toNode =
	<module path={path.toPath} type={tp} change="identical"/>
}

/**
 * Declaration Level
 */

trait ChangeDeclaration extends Change {
  def toNode : Node
}


case class AddDeclaration(d : Declaration, tp : String) extends Add with ChangeDeclaration {
  def toNode = 
    <declaration type={tp} change="add">
  		{d.toNode}
	</declaration>  
}
case class UpdateDeclaration(val path : GlobalName, tp : String, val changes : List[ChangeComponent]) extends Update with ChangeDeclaration {
  def toNode = 
	<declaration type={tp} change="update" path={path.toPath}>
  	  {changes.map(s => s.toNode)}
	</declaration>
}

case class DeleteDeclaration(path : GlobalName, tp : String) extends Delete with ChangeDeclaration {
  def toNode =
    <declaration type={tp} change="delete" path={path.toPath}/>
}
case class RenameDeclaration(path : GlobalName, tp : String, name : LocalName) extends Rename with ChangeDeclaration {
  def toNode =
    <declaration type={tp} change="rename" path={path.toPath} name={name.toString}/>
}
case class IdenticalDeclaration(path : GlobalName, tp : String) extends Identical with ChangeDeclaration {
  def toNode =
    <declaration type={tp} change="identical" path={path.toPath}/>
}

/**
 * Object Level
 */

sealed abstract class Component {
  def toNode : Node
  def tp : String
}

case class Obj2Component(o : Obj) extends Component {
  def toNode = <OMOBJ xmlns:om="om">{o.toNode}</OMOBJ>
  def tp : String = o match {
    case t : Term => "Term"
    case o : OMCOMP => "OMCOMP"
    case o : OMDL => "OMDL"
    case o : OMIDENT => "OMIDENT"
    case o : OMMOD => "OMMOD"
        
        
  }
}
 
case class Path2Component(p : Path) extends Component {
  def toNode = <path> {p.toPath} </path>
  def tp = p match {
    case d : DPath => "DPath"
    case m : MPath => "MPath"
  }
}

case class LN2Component(ln : LocalName) extends Component {
  def toNode = <ln>{ln.toString}</ln>
  def tp = "LocalName"
}

case class LP2Component(lp : LocalPath) extends Component {
  def toNode = <lp>{lp.toString}</lp>
  def tp = "LocalPath"
}

case class TheoryObj2Component(to : TheoryObj) extends Component {
  def toNode = <to>{to.asPath} </to>
  def tp = "TheoryObj"
}


trait ChangeComponent {
  def toNode : Node
}

case class AddComponent(name : String, o : Component) extends Add with ChangeComponent {
  def toNode = 
    <component type={o.tp} name={name} change="add">
      {o.toNode}
	 </component>
}
case class UpdateComponent(name : String, o : Component) extends Add with ChangeComponent {
  def toNode = 
  	<component type={o.tp} name={name} change="update">
  	  {o.toNode}
  	</component>
}

case class DeleteComponent(tp : String, name : String) extends Delete with ChangeComponent {
  def toNode =
    <component type={tp} name={name} change="delete"/>
}

case class IdenticalComponent(tp : String, name : String) extends Identical with ChangeComponent {
  def toNode =
   <component type={tp} name={name} change="identical"/>
}



/*

case class Add(e: StructuralElement) extends Change {
  def toNode : Node = {
    <add>
		{e.toNode}
	</add>
  }
  
  override def toString : String = {
    println(e.asInstanceOf[AnyRef].getClass.getDeclaredFields.toList)

    "ADD [ " + e.toString + " ]"
  }
  
}

case class Update(e : StructuralElement) extends Change {
  def toNode : Node = {
    <update path={e.path.toPath}>
		{e.toNode}
	</update>
  }
  
  override def toString : String = {
    "UPDATE [ " + e.toString + " ]"
  }
}
case class Delete(p: Path) extends Change {
  def toNode : Node = {
    <delete path={p.toString}/>

  }
  
  override def toString : String = {
    "DELETE [ " + p.toString + " ]"
  }
}

case class Rename(p: Path, name: LocalName) extends Change {
  def toNode : Node = {
    <rename path={p.toString} to={name.toString}/>
  }
  
  override def toString : String = {
    "RENAME [ " + p.toString + " ]\n\t TO [ " + name.toString + " ]"
  }
}


class Diff(val old : Controller, val nw : Controller,val path : DPath) {
   var changes: List[Change] = Nil
   def toNode : Node = {
     <omdoc-diff old={old.toString} new={nw.toString}>
		<!-- This is an automatically generated omdoc diff file -->
		<changes>
		{changes.map(_.toNode)}
		</changes>
	 </omdoc-diff>
   }
   
   override def toString : String = {
     changes.foldLeft("DIFF {")((s,x) => s + "\n" + x.toString + "\n") + "}"
   }
}

object Diff {
  def apply(old : Controller, nw : Controller, p : DPath, c : Change*) = {
    
    val d = new Diff(old, nw, p)
    d.changes = c.toList
    d
  }
}

abstract class Status {
  def toChange() : List[Change]
}

case class Identical(orig : Path) extends Status {
  def toChange = {
    Nil
  }
}
case class Modified(c : List[Change], orig : Path) extends Status {
  def toChange() = {
    c
  }
}
case class Distinct(c : StructuralElement) extends Status {
  def toChange() = {
    List(Add(c)) 
  }
}
*/

