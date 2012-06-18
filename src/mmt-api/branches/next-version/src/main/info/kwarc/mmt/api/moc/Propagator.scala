package info.kwarc.mmt.api.moc

import info.kwarc.mmt.api.frontend.ROMemory
import info.kwarc.mmt.api.ontology.{DependsOn, ToSubject}
import info.kwarc.mmt.api.{CPath, Path, ContentPath}
import info.kwarc.mmt.api.symbols.{Assignment, Constant, Declaration}
import collection.immutable.HashSet
import info.kwarc.mmt.api.modules.{DeclaredView, DeclaredTheory, Module}
import info.kwarc.mmt.api.objects.{OMID, OME, Term}

object Propagator {
  //TODO improve
  def box(t : Term, change : ContentChange) : Term = {
    OME(t, t :: Nil) //TODO
  }

  def box(t : Term, changes : List[ContentChange]) : Term = changes match {
    case Nil => t
    case hd :: tl => box(box(t,hd), tl)
  }

}

abstract class Propagator(memory : ROMemory) {
  def propagate(diff : Diff) : Diff
}

class FoundationalPropagator(memory : ROMemory) extends Propagator(memory) {
  def propagate(diff : Diff)  : Diff = {
    new Diff(propagate(diff.changes))
  }

  private def propagate(changes : List[ContentChange]) : List[ContentChange] = {
    val impactedBy = getImpacts(changes)

    val impacts = impactedBy collect {
      case (c : CPath, l) =>
        memory.content.get(c) match {
          //TODO refine .box to change-specific function
          case t : Term => UpdateComponent(c.parent, c.component, Some(t), Some(Propagator.box(t, l)))
        }
    }
    impacts.toList
  }

  private def getImpacts(changes : List[ContentChange]) : collection.mutable.HashMap[Path,List[ContentChange]]=  {

    var impactedBy = new collection.mutable.HashMap[Path,List[ContentChange]]()

    changes foreach {
      addImpacts(_, impactedBy)
    }

    impactedBy
  }


  private def addImpacts(change : ContentChange, impactedBy : collection.mutable.HashMap[Path,List[ContentChange]]) : collection.mutable.HashMap[Path,List[ContentChange]]=  {

    def addPath(x : Path) : Unit = {
      if (impactedBy.isDefinedAt(x)) {
        impactedBy += (x -> (change :: impactedBy(x)))
      } else {
        impactedBy += (x -> (change :: Nil))
      }
    }

    getAffectedCPaths(change) foreach {cpath =>
      memory.ontology.query(cpath, ToSubject(DependsOn))(addPath)
    }

    impactedBy
  }



  private def getAffectedCPaths(change : ContentChange) : HashSet[CPath]= change match {
    case AddDeclaration(d) => new HashSet[CPath]()
    case AddModule(m) => new HashSet[CPath]()
    case DeleteModule(m) =>  getAffectedCPaths(m)
    case DeleteDeclaration(d) => getAffectedCPaths(d)
    case UpdateComponent(path, name, old, nw) => new HashSet[CPath]() + CPath(path,name)
    case PragmaticChange(name, diff, tp, mp) => new HashSet[CPath]() ++ diff.changes.flatMap(getAffectedCPaths(_))

  }


  private def getAffectedCPaths(mod : Module) : HashSet[CPath] = {
    var cpaths = new HashSet[CPath]()
    mod match {
      case t : DeclaredTheory => cpaths += CPath(mod.path, "meta")
      case v : DeclaredView => cpaths = cpaths + CPath(mod.path, "from") + CPath(mod.path, "to")
    }

    mod.components collect {
      case dec : Declaration => cpaths ++= getAffectedCPaths(dec)
    }

    cpaths
  }

  private def getAffectedCPaths(dec : Declaration) : HashSet[CPath] = dec match {
    case  c : Constant => new HashSet[CPath]() + CPath(c.path, "type") + CPath(c.path, "definition")
    case  a : Assignment => new HashSet[CPath]() + CPath(a.path, "definition")
    case _ => new HashSet[CPath]()
  }
}