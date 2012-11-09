package info.kwarc.mmt.api.moc

import scala.collection._
import scala.collection.immutable.{HashMap,HashSet,List}

import info.kwarc.mmt.api._
import ontology._
import symbols._
import patterns._
import modules._
import utils.mmt
import frontend.ROMemory
import objects._


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
  
  
  /**
   * paths made invalid by the propagation 
   */
  var boxedPaths : immutable.Set[CPath] = Nil.toSet
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
  protected def dependsOn(path : Path) : Set[Path]
  
  /**
   * The propagation function for individual paths
   * It implements how/which impacted items are affected by the propagation
   * '''It must hold that the returned change affects exactly or a sub-path of the impacted path'''
   * i.e. semantically {{{path isAncestorOf propFunc(path,_).getReferencedURI}}} must hold
   * @param path the impacted path
   * @param changes the set of changes that impact path
   * @return optionally the generated change
   */
  protected def propFunc(path : Path, changes : Set[ContentChange]) : List[StrictChange]
  
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
    
    val pdiff = new Diff(propagatedChanges.toList)
    updateBoxedPaths(diff, pdiff)
    pdiff
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
    case UpdateMetadata(path, key, old, nw) => new HashSet[Path]() + path
    case PragmaticChange(name, diff, tp, mp, desc) => new HashSet[Path]() ++ diff.changes.flatMap(affectedPaths(_))

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
      case t : DeclaredTheory => cpaths += CPath(mod.path, DomComponent)
      case t : DefinedTheory => cpaths += CPath(mod.path, DefComponent)
      case v : DeclaredView => cpaths = cpaths + CPath(mod.path, DomComponent) + CPath(mod.path, CodComponent)
      case v : DefinedView => cpaths = cpaths + CPath(mod.path, DomComponent) + CPath(mod.path, CodComponent) + CPath(mod.path, DefComponent) 
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
      case c : Constant => new HashSet[Path]() + dec.path + CPath(c.path, TypeComponent) + CPath(c.path, DefComponent)
      case s : Structure => new HashSet[Path]() + dec.path + CPath(s.path, DomComponent)
      case p : Pattern => new HashSet[Path]() + dec.path + CPath(p.path, ParamsComponent) + CPath(p.path, PatternBodyComponent)
      case i : Instance => new HashSet[Path]() + dec.path + CPath(i.path, PatternComponent) + CPath(i.path, MatchesComponent)
      case a : ConstantAssignment => new HashSet[Path]() + dec.path + CPath(a.path, DefComponent)
      case d : DefLinkAssignment => new HashSet[Path]() + dec.path + CPath(d.path, DefComponent)
      case a : Alias => new HashSet[Path]() + dec.path + CPath(a.path, ForPathComponent)
    } 
  }
  
  private def updateBoxedPaths(diff : Diff, pdiff : Diff) = {
    def isBoxed(tm : Option[Obj]) : Boolean = tm match {
      case None => false
      case Some(t : OME) => true
      case _ => false
    }
    
    //removing unboxed paths
    diff.changes foreach {
      case UpdateComponent(p,c,t1,t2) => boxedPaths -= CPath(p,c)
      case DeleteDeclaration(d) => 
        val tmp = boxedPaths 
        tmp.foreach(p => if (p.parent == d.path) boxedPaths -= p)
      case DeleteModule(m) => 
        val tmp = boxedPaths 
        tmp.foreach(p => if (p.parent == m.path || p.parent.^! == m.path) boxedPaths -= p)
      case _ => None
    }
    
    //adding new boxed paths
    pdiff.changes foreach {
      case UpdateComponent(p,c,t1,t2) if isBoxed(t2) => boxedPaths += CPath(p,c)
      case _ => None
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
  protected def dependsOn(path : Path) : Set[Path] = {   
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
  protected def propFunc(path : Path, changes : Set[ContentChange]) : List[StrictChange] = path match {
    case cp : CPath => 
      def makeChange(otm : Option[Term]) : Option[StrictChange] = otm match {
        case Some(tm) => Some(UpdateComponent(cp.parent, cp.component, Some(tm), Some(box(tm, changes))))
        case None => None
      }
      
      val chOpt = (mem.content.get(cp.parent), cp.component) match {
      /* Theories */
      case (t : DeclaredTheory, DomComponent) => None
      case (t : DefinedTheory, DomComponent) => None
      case (t : DefinedTheory, DefComponent) => makeChange(Some(t.df))

      /* Views */
      case (v : View, CodComponent) => makeChange(Some(v.to))
      case (v : View, DomComponent) => makeChange(Some(v.from))
      case (v : DefinedView,  DefComponent) => makeChange(Some(v.df))

      /* Constants */
      case (c : Constant, TypeComponent) => makeChange(c.tp)
      case (c : Constant, DefComponent) => makeChange(c.df)

      /* Patterns */
      case (p : Pattern, _) => None //TODO makeChange(Some(p.params))

      /* Instance */
      case (i : Instance, _) => None

      /* ConstantAssignments */
      case (c : ConstantAssignment, DefComponent) => makeChange(Some(c.target))

      /* DefLinkAssignment */
      case (d : DefLinkAssignment, DefComponent) => makeChange(Some(d.target))

      /* Aliases */
      case (a : Alias, _) => None
    }
    chOpt.toList
    case _ => Nil  
  }
  
  /**
   * Function that marks the errors by surrounding with box terms
   * TODO: refine by adding better error information
   * @param tm the impacted term
   * @param changes the changes that impact tm
   * @return the boxed term
   */
  private def box(tm : Term, changes : Set[ContentChange]) : Term = {
    
    def makeTerm(path : Path) : Term = path match {
      case p : ContentPath => OMID(p)
      case cp : CPath => OMID(cp.parent)
      case _ => throw ImplementationError("Expected ContentPath or CPath found: " + path.toPath)
      
    }
    
    OME(OMID(mmt.mmtsymbol("fullbox")), tm :: changes.flatMap(_.getReferencedURIs).map(makeTerm(_)).toList) 
  }
  
}

/**
 * The occurs-in impact propagator is an impact propagator based on the occurs-in (refers-to) relation
 * that marks impacted items by surrounding them with error terms so that 
 * after the error terms are replaced with valid ones the validity of the entire 
 * theory graph is ensured 
 */
class OccursInImpactPropagator(mem : ROMemory) extends ImpactPropagator(mem) {
  /**
   * Implements the dependency relation, returns all paths that refer to a certain path (i.e. the impacts)
   * For the foundational impact propagator uses the default ''refersTo'' relation given by the ontology (and gathered by the checker)
   * @param path the path 
   */
  protected def dependsOn(path : Path) : Set[Path] = {
    val impacts = new mutable.HashSet[Path]()
    
    affectedPaths(path) foreach {p =>
      mem.ontology.query(p,ToSubject(RefersTo)) {p => 
        try {
          mem.content.get(p) match {
            case c : Constant => 
              impacts += CPath(c.path, TypeComponent) 
              impacts += CPath(c.path, DefComponent)
            case _ => //TODO
          }
        } catch {
          case _ => //TODO
        }
      }
        
    }
    impacts 
  }
  
  private def affectedPaths(path : Path) : List[Path] = path match {
    case c : CPath => c :: affectedPaths(c.parent)
    case g : GlobalName => g :: affectedPaths(g.module.toMPath)
    case m : MPath => m :: Nil
    case _ => Nil //doc's don't count
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
  protected def propFunc(path : Path, changes : Set[ContentChange]) : List[StrictChange] = path match {
    case cp : CPath => 
      def makeChange(otm : Option[Term]) : Option[StrictChange] = otm match {
        case Some(tm) => Some(UpdateComponent(cp.parent, cp.component, Some(tm), Some(box(tm, changes))))
        case None => None
      }
      
      val chOpt = (mem.content.get(cp.parent), cp.component) match {
      /* Theories */
      case (t : DeclaredTheory, DomComponent) => None
      case (t : DefinedTheory, DomComponent) => None
      case (t : DefinedTheory, DefComponent) => makeChange(Some(t.df))

      /* Views */
      case (v : View, CodComponent) => makeChange(Some(v.to))
      case (v : View, DomComponent) => makeChange(Some(v.from))
      case (v : DefinedView, DefComponent) => makeChange(Some(v.df))

      /* Constants */
      case (c : Constant, TypeComponent) => makeChange(c.tp)
      case (c : Constant, DefComponent) => makeChange(c.df)

      /* Patterns */
      case (p : Pattern, _) => None //TODO makeChange(Some(p.params))

      /* Instance */
      case (i : Instance, _) => None

      /* ConstantAssignments */
      case (c : ConstantAssignment, DefComponent) => makeChange(Some(c.target))

      /* DefLinkAssignment */
      case (d : DefLinkAssignment, DefComponent) => makeChange(Some(d.target))

      /* Aliases */
      case (a : Alias, _) => None
    }
    chOpt.toList
    case _ => Nil  
  }
  
  /**
   * Function that marks the errors by surrounding with box terms
   * TODO: refine by adding better error information
   * @param tm the impacted term
   * @param changes the changes that impact tm
   * @return the boxed term
   */
  private def box(tm : Term, changes : Set[ContentChange]) : Term = changes.toSeq match {
    case Seq(p : PragmaticChange) => p.termProp(tm)
    case _ => 
      def makeTerm(path : Path) : Term = path match {
        case p : ContentPath => OMID(p)
        case cp : CPath => OMID(cp.parent)
        case _ => throw ImplementationError("Expected ContentPath or CPath found: " + path.toPath)
      }
      OME(OMID(mmt.mmtsymbol("fullbox")), tm :: changes.flatMap(_.getReferencedURIs).map(makeTerm(_)).toList) 
  }
  
}

/**
 * The structural impact propagator is an impact propagator 
 * that ensures the totality of views
 */
class StructuralImpactPropagator(mem : ROMemory) extends ImpactPropagator(mem) {
  
  /**
   * Implements the dependency relation, returns all paths that depend on a certain path (i.e. the impacts)
   * For the structural impact propagator it is based on the totality of views constraint
   * Specifically, T?c/def -> v?c iff v : T -> S
   * @param path the path 
   * @return the set of impacted paths
   */
  def dependsOn(path : Path) : Set[Path] = {
    var impacts = new mutable.HashSet[Path]()
    
    path match {
      case CPath(GlobalName(mod, lname), DefComponent) =>
        mem.ontology.query(mod.toMPath, ToSubject(HasDomain)) {
          case viewPath : MPath => 
            impacts += viewPath ? lname
          case _ => 
        }
      case _ =>
    }
    
    impacts
  }
  
  /**
   * The propagation function for individual paths
   * It implements how impacted items are affected by the propagation
   * For the structural impact propagator this deletes assignments that are 
   * no longer necessary and adds stubs for newly required assignments 
   * @param path the impacted path
   * @param changes the set of changes that impact path
   * @return optionally the generated change
   */
  def propFunc(path : Path, changes : Set[ContentChange]) : List[StrictChange] = path match {
    case GlobalName(mod, lname) => 
      
      val emptyBox = OME(OMID(mmt.mmtsymbol("emptybox")), Nil)

      changes.size match {
        case 1 => 
          changes.head match {
            //definition is deleted -> one more undefined constant -> assignment needed for it
            case UpdateComponent(cPath, DefComponent, Some(s), None) => 
              val ca = new ConstantAssignment(mod, lname, emptyBox)
              List(AddDeclaration(ca))                
           
            //definition is added -> one less undefined constant -> assignment for it no longer needed
            case UpdateComponent(cPath, DefComponent, None, Some(s)) => 
              val ca = mem.content.getConstantAssignment(mod.toMPath ? lname)
              List(DeleteDeclaration(ca))
            
            case _ => Nil

          }

        case 0 => throw ImplementationError("Cannot have path impacted by no changes:" + path.toPath)
        
        case _ => throw ImplementationError("Cannot have path impacted structurally by more than" +
                                            " one change, (a view has exactly one domain)")
      }
    case _ => throw ImplementationError("Cannot have structural validity impact non-declarations " +
    		                                "i.e. T?c/def -> v?c for v : T -> S ")
  }
  
}

