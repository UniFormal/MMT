package info.kwarc.mmt.api.moc

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._

import scala.xml.Node

import scala.collection._
import scala.collection.immutable.HashSet


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
    def getReferencedURIs : List[Path]
}

sealed abstract class StrictChange extends ContentChange {
  def getReferencedURI : Path
  
  def getReferencedURIs : List[Path] = List(getReferencedURI)
}

abstract class Add extends StrictChange
abstract class Update extends StrictChange
abstract class Delete extends StrictChange


abstract class PragmaticChangeType {
  val name : String
  def matches(diff : Diff) : Boolean
  
  def matches(chSet : Set[ContentChange]) : Boolean = {
    matches(new Diff(chSet.toList))
  }
  
  def termProp(diff : Diff)(tm : Term) : Term
  
  def modProp(diff : Diff)(mod : Module) : StrictDiff

  def make(diff : StrictDiff) : Option[PragmaticChange] = matches(diff) match {
    case true => Some(new PragmaticChange(name, diff, termProp(diff), modProp(diff)))
    case false => None
  }
}

case class PragmaticChange(val name : String, val diff : StrictDiff, val termProp : Term => Term, val modProp : Module => StrictDiff) extends ContentChange {
  def toNode =
    <PragmaticChange name={name}>
      {diff.toNode}
    </PragmaticChange>

  def toNodeFlat = diff.changes.flatMap(_.toNodeFlat)

  def getReferencedURIs : List[Path] = diff.changes.map(_.getReferencedURI)
    
  def toStrict : Diff = diff
}

object pragmaticRename extends PragmaticChangeType {
  val name = "ConstantRename"
  
  def matches(diff : Diff) : Boolean = diff.changes match {
    case DeleteDeclaration(o : Constant) :: AddDeclaration(n : Constant) :: Nil =>
      o.name != n.name && o.tp == n.tp && o.df == n.df
    case AddDeclaration(n : Constant) :: DeleteDeclaration(o : Constant) :: Nil =>
      o.name != n.name && o.tp == n.tp && o.df == n.df
    case _ => false
  }
 
  def termProp(diff : Diff)(tm : Term) : Term = {
    val (old,nw) = diff.changes match {
      case DeleteDeclaration(o : Constant) :: AddDeclaration(n : Constant) :: Nil =>
        (o.path, n.path)
      case AddDeclaration(n : Constant) :: DeleteDeclaration(o : Constant) :: Nil =>
        (o.path, n.path)
      case _ => 
        throw ImplementationError("unexpected error: invalid diff used to propagate in pragmatic change: " + name)
    }
    
    def prop(t : Term) : Term = t match {
      case OMID(p) => if (p == old) OMID(nw) else t
      case OMA(f, args) => OMA(prop(f), args.map(prop))
      case OMBIND(binder, con, body) =>
        val newCon = Context(con.components.map(v =>
          VarDecl(v.name, v.tp.map(prop), v.df.map(prop), v.ats :_ *)
        ) :_ *)
        
        OMBIND(prop(binder), newCon, prop(body))
      case _ => t
    }
    
    prop(tm)
  }

  def modProp(diff : Diff)(m : Module) : StrictDiff = {
    new StrictDiff(Nil)
  }
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

case class UpdateComponent(path : ContentPath, name : DeclarationComponent, old : Option[Obj], nw : Option[Obj]) extends Update with ContentChange {

  def getReferencedURI : CPath = CPath(path, MetaDataComponent)

  def toNode =
    <component path={path.toPath} name={name.toString} change="update">
      {nw.map(x => x.toNode).toSeq}
    </component>
//  	  {o.toNode(changes)}
  
  def toNodeFlat =
    <change type="update" path={path.toString} component={name.toString}>  {nw.map(_.toNode).toSeq}  </change> :: Nil
}

case class UpdateMetadata(path : ContentPath, old : metadata.MetaData, nw : metadata.MetaData) extends Update with ContentChange {
  def getReferencedURI :  CPath = CPath(path, MetaDataComponent) 
  def toNode = 
     <update path={path.toPath} name="metadata" change="update">
      {nw.toNode}
    </update>
      
  def toNodeFlat = 
    <change type="update" path={path.toString} component="metadata"> nw.toNode </change> :: Nil
  
}


