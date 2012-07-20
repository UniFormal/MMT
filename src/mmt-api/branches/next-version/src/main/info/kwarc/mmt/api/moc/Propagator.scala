package info.kwarc.mmt.api.moc

import scala.collection._
import scala.collection.immutable.{HashMap,HashSet,List}

import info.kwarc.mmt.api.frontend.ROMemory
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.{CPath, Path, ContentPath}
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.patterns._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.utils.mmt
import info.kwarc.mmt.api.objects.{OMID, OME, Term}


/**
 * The generic propagator class that simply acts as a diff enricher
 * @param mem the memory containing the current theory graph
 */
abstract class Propagator(mem : ROMemory) {
  
  /**
   * The main propagation function
   * @param diff the diff
   * @return the generated propagation diff
   */
  def propagate(diff : Diff) : Diff
  
  /**
   * default application of propagators is to propagate
   * (i.e. apply directly calls propagate)
   * @param diff the diff
   * @return the generated propagation diff
   */
  def apply(diff : Diff) : Diff = propagate(diff)
}


/**
 * An example of a (trivial and useless) propagator
 */
class NullPropagator(mem : ROMemory) extends Propagator(mem) {
  def propagate(diff : Diff) : Diff = new Diff(Nil)
}

/**
 * An impact propagator is a special propagator that generates changes based on an 
 * abstract dependency relation (given by the method ''dependsOn'') and on a
 * propagation function (''propFunc'') to be applied to impacted content items.
 * @param mem the memory
 */
abstract class ImpactPropagator(mem : ROMemory) extends Propagator(mem) {
  
  /**
   * Implements the dependency relation, returns all paths that depend on a certain path (i.e. the impacts)
   * @param path the path 
   * @return the set of impacted paths
   */
  def dependsOn(path : Path) : Set[Path]
  
  /**
   * The propagation function for individual paths
   * It implements how/which impacted items are affected by the propagation
   * '''It must hold that the returned change affects exactly the impacted path'''
   * @param path the impacted path
   * @param changes the set of changes that impact path
   * @return optionally the generated change
   */
  def propFunc(path : Path, changes : Set[ContentChange]) : Option[StrictChange]
  
  /**
   * The main diff propagation function
   * For Impact propagators, implemented using the ''dependsOn'' and ''propFunc'' functions
   * @param diff the diff
   * @return the generated propagation diff
   */
  def propagate(diff : Diff) : Diff = { 
    var impacts = new mutable.HashMap[Path, mutable.HashSet[ContentChange]]()
    
    //gathers impacted content items (and for each the changes that impact them)
    diff.changes map { c => 
      affectedPaths(c).flatMap(p => dependsOn(p)) map { p => 
        if (! impacts.isDefinedAt(p)) {
          impacts(p) = new mutable.HashSet[ContentChange]()
        } 
        impacts(p) += c
      }
    }
    
    //applies the propagation function to the impacted content items
    val propagatedChanges = impacts flatMap {p => 
      propFunc(p._1, p._2) 
    }
    
    new Diff(propagatedChanges.toList)
    
  }
  
  /**
   * Computes the MMT paths affected by a change 
   * @param change the change
   */
  private def affectedPaths(change : ContentChange) : HashSet[Path]= change match {
    case AddDeclaration(d) => new HashSet[Path]()
    case AddModule(m) => new HashSet[Path]()
    case DeleteModule(m) =>  containedPaths(m)
    case DeleteDeclaration(d) => containedPaths(d)
    case UpdateComponent(path, name, old, nw) => new HashSet[Path]() + CPath(path,name)
    case PragmaticChange(name, diff, tp, mp) => new HashSet[Path]() ++ diff.changes.flatMap(affectedPaths(_))

  }

  /**
   * Computes the MMT paths contained in a module: the path of the module itself,
   * the declaration paths and the component paths
   * @param mod the module
   */
  private def containedPaths(mod : Module) : HashSet[Path] = {
    var cpaths = new HashSet[Path]()
    
    // module path
    cpaths += mod.path
    
    //declaration paths
    mod.components collect {
      case dec : Declaration => cpaths ++= containedPaths(dec)
    }
    
    //component paths
    mod match {
      case t : DeclaredTheory => cpaths += CPath(mod.path, "meta")
      case t : DefinedTheory => cpaths += CPath(mod.path, "df")
      case v : DeclaredView => cpaths = cpaths + CPath(mod.path, "from") + CPath(mod.path, "to")
      case v : DefinedView => cpaths = cpaths + CPath(mod.path, "from") + CPath(mod.path, "to") + CPath(mod.path, "df") 
    }
    
    cpaths
  }
  
  /**
   * Computes the paths contained in a declaration: the path of the declaration itself and
   * and its component paths
   * @param dec : the declaration
   */
  private def containedPaths(dec : Declaration) : HashSet[Path] = {
    dec match {
      case c : Constant => new HashSet[Path]() + dec.path + CPath(c.path, "type") + CPath(c.path, "definition")
      case s : Structure => new HashSet[Path]() + dec.path + CPath(s.path, "from")
      case p : Pattern => new HashSet[Path]() + dec.path + CPath(p.path, "params") + CPath(p.path, "body")
      case i : Instance => new HashSet[Path]() + dec.path + CPath(i.path, "pattern") + CPath(i.path, "matches")
      case a : ConstantAssignment => new HashSet[Path]() + dec.path + CPath(a.path, "target")
      case d : DefLinkAssignment => new HashSet[Path]() + dec.path + CPath(d.path, "target")
      case a : Alias => new HashSet[Path]() + dec.path + CPath(a.path, "forpath")
    } 
  } 
}

/**
 * The foundational impact propagator is an impact propagator 
 * that marks impacted items by surrounding them with error terms so that 
 * after the error terms are replaced with valid ones the validity of the entire 
 * theory graph is ensured 
 */
class FoundationalImpactPropagator(mem : ROMemory) extends ImpactPropagator(mem) {
  /**
   * Implements the dependency relation, returns all paths that depend on a certain path (i.e. the impacts)
   * For the foundational impact propagator uses the default ''dependsOn'' relation given by the ontology (and gathered by the checker)
   * @param path the path 
   */
  def dependsOn(path : Path) : Set[Path] = {   
    val impacts = new mutable.HashSet[Path]()
    mem.ontology.query(path,ToSubject(DependsOn))(p => impacts += p)
    impacts
  }
  
  /**
   * The propagation function for individual paths
   * It implements how impacted items are affected by the propagation
   * For the foundational impact propagator this applies box terms (OpenMath error terms) to 
   * surround (potentially) invalid terms for strict changes and applies the default
   * propagation function for pragmatic changes (if applicable). 
   * @param path the impacted path
   * @param changes the set of changes that impact path
   */
  def propFunc(path : Path, changes : Set[ContentChange]) : Option[StrictChange] = path match {
    case cp : CPath => 
      def makeChange(otm : Option[Term]) : Option[StrictChange] = otm match {
        case Some(tm) => Some(UpdateComponent(cp.parent, cp.component, Some(tm), Some(box(tm, changes))))
        case None => None
      }
      
      (mem.content.get(cp.parent), cp.component) match {
      /* Theories */
      case (t : DeclaredTheory, "meta") => None
      case (t : DefinedTheory, "meta") => None
      case (t : DefinedTheory, "df") => makeChange(Some(t.df))

      /* Views */
      case (v : View, "to") => makeChange(Some(v.to))
      case (v : View, "from") => makeChange(Some(v.from))
      case (v : DefinedView,  "df") => makeChange(Some(v.df))

      /* Constants */
      case (c : Constant, "type") => makeChange(c.tp)
      case (c : Constant, "def") => makeChange(c.df)

      /* Patterns */
      case (p : Pattern, "params") => None //TODO makeChange(Some(p.params))
      case (p : Pattern, "body") => None //TODO makeChange(Some(p.body))

      /* Instance */
      case (i : Instance, "pattern") => None
      case (i : Instance, "matches") => None //TODO makeChange(Some(i.matches))

      /* ConstantAssignments */
      case (c : ConstantAssignment, "target") => makeChange(Some(c.target))

      /* DefLinkAssignment */
      case (d : DefLinkAssignment, "target") => makeChange(Some(d.target))

      /* Aliases */
      case (a : Alias, "forpath") => None
    }
    case _ => None  
  }
  
  /**
   * Function that marks the errors by surrounding with box terms
   * TODO: refine by adding better error information
   * @param tm the impacted term
   * @param changes the changes that impact tm
   * @return the boxed term
   */
  private def box(tm : Term, changes : Set[ContentChange]) : Term = {
    
    OME(OMID(mmt.mmtsymbol("impacted term")), tm :: changes.flatMap(_.getReferencedURIs).map(OMID(_)).toList) 
  }
  
}

