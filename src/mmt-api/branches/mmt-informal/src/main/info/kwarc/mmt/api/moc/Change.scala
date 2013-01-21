package info.kwarc.mmt.api.moc

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._

import scala.xml.Node

import scala.collection._
import scala.collection.immutable.HashSet


sealed abstract class Change {
  def toNode : Node
}

abstract class Add(val s : StructuralElement) extends Change
abstract class Delete(val s : StructuralElement) extends Change

object Add {
  def apply(s : StructuralElement) : Add = s match {
    case p : PresentationElement => AddPresentation(p)
    case d : Declaration => AddDeclaration(d)
    case m : Module => AddModule(m)
  }
}

object Delete {
  def apply(s : StructuralElement) : Delete = s match {
    case p : PresentationElement => DeletePresentation(p)
    case d : Declaration => DeleteDeclaration(d)
    case m : Module => DeleteModule(m)
  }
}

class Diff(val changes : List[Change]) {
   lazy val contentChanges = changes collect {
     case c : ContentChange => c 
   }
   
   def toNode = <omdoc-diff> {changes.map(_.toNode)} </omdoc-diff>
   
   def toNodeFlat = <omdoc-diff> {contentChanges.flatMap(_.toNodeFlat)} </omdoc-diff>
   
   def ++(d : Diff) = new Diff(changes ++ d.changes)
}


class StrictDiff(override val changes : List[StrictChange]) extends Diff(changes) {
  def ++(d : StrictDiff) = new StrictDiff(changes ++ d.changes)
}


sealed trait ContentChange extends Change {
    def toNodeFlat : List[Node]
    def getReferencedURIs : List[Path]
}

sealed trait StrictChange extends ContentChange {
  def getReferencedURI : Path
  
  def getReferencedURIs : List[Path] = List(getReferencedURI)
}

abstract class AddContent(s : ContentElement) extends Add(s : StructuralElement) with StrictChange
abstract class UpdateContent extends StrictChange
abstract class DeleteContent(s : ContentElement) extends Delete(s : StructuralElement) with StrictChange


abstract class PragmaticChangeType {
  val name : String
  def matches(diff : Diff) : Boolean
  
  def matches(chSet : Set[ContentChange]) : Boolean = {
    matches(new Diff(chSet.toList))
  }
  
  def termProp(diff : Diff)(tm : Term) : Term
  
  def modProp(diff : Diff)(mod : Module) : StrictDiff

  def description(diff : Diff) : String
  
  def make(diff : StrictDiff) : Option[PragmaticChange] = matches(diff) match {
    case true => Some(new PragmaticChange(name, diff, termProp(diff), modProp(diff), description(diff)))
    case false => None
  }
}

case class PragmaticChange(
			 val name : String, 
			 val diff : StrictDiff, 
			 val termProp : Term => Term, 
			 val modProp : Module => StrictDiff,
			 val description : String
		   ) extends ContentChange {
  def toNode =
    <PragmaticChange name={name}>
      {diff.toNode}
    </PragmaticChange>

  def toNodeFlat = diff.changes.flatMap(_.toNodeFlat)
  

  def getReferencedURIs : List[Path] = diff.changes.map(_.getReferencedURI)
    
  def toStrict : Diff = diff
}

object pragmaticAlphaRename extends PragmaticChangeType {
  val name = "AlphaRename"
  
  private def isAlpha(old : Option[Term], nw : Option[Term])
    (implicit context : immutable.HashMap[String, String]) : Boolean = (old,nw) match {
    case (Some(o), Some(n)) => isAlpha(o,n)
    case (None, None) => true
    case _ => false
  }  
    
  private def isAlpha(old : Term, nw : Term)(implicit context : immutable.HashMap[String, String]) : Boolean = {
    (old,nw) match {
      case (OMID(p1), OMID(p2)) => p1 == p2
      case (OMA(f, args), OMA(f2, args2)) => 
        isAlpha(f,f2) && args.length == args2.length && args.zip(args2).forall(p => isAlpha(p._1, p._2)) 
      case (OMV(n), OMV(n2)) => context(n.toPath) == n2.toPath
      case (OMBIND(b, con, body), OMBIND(b2,con2, body2)) if (isAlpha(b,b2)) => 
        if (con.components.length == con2.components.length) {
          var newcon = context
          var holds = con.components.zip(con2.components) forall {
            case (VarDecl(n, tp, df, _*), VarDecl(n2, tp2, df2, _*)) => 
              newcon += (n.toPath -> n2.toPath)
              isAlpha(tp, tp2) && isAlpha(df, df2)              
          }
          holds && isAlpha(body, body2)(newcon)
        } else {
          false
        }
      case _ => false
    }
  }
  
  def matches(diff : Diff) : Boolean = diff.changes match {
    case UpdateComponent(p,c, Some(t : Term), Some(t2 : Term)) :: Nil => isAlpha(t,t2)(new immutable.HashMap[String,String])
    case _ => false
  }
  
  def termProp(diff : Diff)(tm : Term) : Term = tm
  
  def modProp(diff : Diff)(m : Module) : StrictDiff = {
    new StrictDiff(Nil)
  }
  
  def description(diff : Diff) : String = {
    val (p, c) = diff.changes match {
      case UpdateComponent(p,c, Some(t : Term), Some(t2 : Term)) :: Nil => p -> c
      case _ => throw ImplementationError("Invalid diff " + diff.toString + " for pragmatic change" + name)
    }
    "Alpha Rename detected in " + c + " of constant " + p.toPath
  }
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
  
  def description(diff : Diff) : String = {
    val (mod,old,nw) = diff.changes match {
      case DeleteDeclaration(o : Constant) :: AddDeclaration(n : Constant) :: Nil =>
        (o.path.module.toMPath, o.path.name, n.path.name)
      case AddDeclaration(n : Constant) :: DeleteDeclaration(o : Constant) :: Nil =>
        (o.path.module.toMPath, o.path.name, n.path.name)
      case _ => 
        throw ImplementationError("unexpected error: invalid diff used to propagate in pragmatic change: " + name)
    }
    
    "Rename in module " + mod.toString + " from " + old.toString + " to " + nw.toString()
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

case class AddModule(m : Module) extends AddContent(m) with ChangeModule {
  def getReferencedURI = m.path
  def getAffectedCPaths = new HashSet[CPath]()
  def getAffectedPaths = new HashSet[ContentPath]()

  def toNode =
    <module change="add">
      {m.toNode}
    </module>

  def toNodeFlat = <change type="add" path={m.path.toPath}> {m.toNode} </change> :: Nil

}

case class DeleteModule(m : Module) extends DeleteContent(m) with ChangeModule {
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


case class AddDeclaration(d : Declaration) extends AddContent(d) with ChangeDeclaration {
  def getReferencedURI = d.path

  def toNode = 
    <declaration change="add">
  		{d.toNode}
	</declaration>  
  
  def toNodeFlat = <change type="add" path={d.path.toPath}> {d.toNode} </change> :: Nil
}

case class DeleteDeclaration(d : Declaration) extends DeleteContent(d) with ChangeDeclaration {
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

case class UpdateComponent(path : ContentPath, name : DeclarationComponent, old : Option[Obj], nw : Option[Obj]) extends UpdateContent with ContentChange {

  def getReferencedURI : CPath = CPath(path, MetaDataComponent)

  def toNode =
    <component path={path.toPath} name={name.toString} change="update">
      {nw.map(x => x.toNode).toSeq}
    </component>
//  	  {o.toNode(changes)}
  
  def toNodeFlat =
    <change type="update" path={path.toPath} component={name.toString}>  {nw.map(_.toNode).toSeq}  </change> :: Nil
}

case class UpdateMetadata(path : ContentPath, key : GlobalName, old: List[Obj], nw : List[Obj]) extends UpdateContent with ContentChange {
  def getReferencedURI :  CPath = CPath(path, MetaDataComponent) 
  def toNode =
    <metadata path={path.toPath} key={key.toPath} change="update">
      {nw.map(x => x.toNode).toSeq}
    </metadata>
//  	  {o.toNode(changes)}
  
  def toNodeFlat =
    <change type="update" path={path.toPath} key={key.toPath}>  {nw.map(_.toNode).toSeq}  </change> :: Nil
  
}

/**
 * Presentation
 */

trait PresentationChange

case class AddPresentation(p : PresentationElement) extends Add(p) with PresentationChange {
  def toNode = <add level="pres">{p.toNode}</add>
}

case class DeletePresentation(p : PresentationElement) extends Delete(p) with PresentationChange {
  def toNode = <delete level="pres">{p.toNode}</delete>
}