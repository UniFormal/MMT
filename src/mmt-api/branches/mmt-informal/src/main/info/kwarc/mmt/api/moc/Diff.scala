package info.kwarc.mmt.api.moc

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.patterns._
import info.kwarc.mmt.api.libraries._

/**
 * the differ object contains methods for detecting the changes between two MMT content items (modules/declarations/objects)
 */
object Differ {
  
    def diff(c : Controller, p : MPath, rev : Int) : StrictDiff = {
      
      //println(p)
      val c2 = new Controller
      c2.backend.addStore(c.backend.copyStorages(rev) :_ *)
      
      val mold = c.get(p) match {
        case m : Module => m
        case _ => throw NotFound(p)
      }
      
      val mnew = c2.get(p) match {
        case m : Module => m
        case _ => throw NotFound(p)
      }
      compareModules(mold,mnew)
    }
    
  def diff(cold : Controller, cnew : Controller, pold : MPath, pnew : MPath) : StrictDiff = {

    val old = cold.memory.content.getModule(pold)

    val nw = cnew.memory.content.getModule(pnew)

    compareModules(old, nw)
  }  
    
	def diff(cold : Controller, cnew : Controller, pold : DPath, pnew : DPath) : StrictDiff = {

	  val old = cold.getDocument(pold)

	  val nw = cnew.getDocument(pnew)

	  //TODO
	  val mold = old.getModulesResolved(cold.library)(0)
	  val mnw = nw.getModulesResolved(cnew.library)(0)
	  compareModules(mold, mnw)
	}
	
	
	def diff(old : StructuralElement, nw : StructuralElement) : StrictDiff = (old,nw) match {
	  case (o : Module, n : Module) => compareModules(o, n)
	  case (o : Declaration, n : Declaration) => compareDeclarations(o, n)
	  case _ => throw ImplementationError("Cannot diff between " + old.toString + " and " + nw.toString)	  
	}
	
		
	/**
	 * checks if two optional objects are equal
	 * @param old the first optional object
	 * @param nw  the second optional objects
	 * @return true if old and nw are equal, false otherwise
	 */
  private def areEqual(old : Option[Obj], nw : Option[Obj]) : Boolean = (old,nw) match {
    case (None,None) => true
    case (Some(o), None) => false
    case (None, Some(n)) => false
    case (Some(o), Some(n)) => o == n
  }
 
  // recursing inside objects not useful now, but may be in the future
  /*   
   private def _fullType(o : Obj) : List[String] = o.toNode.label :: o.toNode.attributes.toString :: Nil
	
	private def compareObjects(o : Obj, n : Obj, pos : Position = Position(Nil)) : List[Position] = {
	  if (o == n) Nil
	  else if (_fullType(o) == _fullType(n) && o.components.length == n.components.length) {
	    o.components.zip(n.components).zipWithIndex.flatMap(p => p._1 match {
	      case (o1 : Obj, n1 : Obj) => compareObjects(o1, n1, pos + p._2)
	      case _ => if (p._1._1 == p._1._2) Nil else pos :: Nil
	    })
	  } else {
	    pos :: Nil
	  }
	}
	*/
	
  /**
   * compares two constants 
   * @param old the first constant
   * @param nw the second constant
   * @return the (strict) diff representing the difference between old and nw
   */
	private def compareConstants(old : Constant, nw : Constant) : StrictDiff = {
    var changes : List[StrictChange] = Nil

    if(!areEqual(old.tp, nw.tp)) {
       changes = UpdateComponent(old.path, TypeComponent, old.tp, nw.tp) :: changes
		}
		
		if (!areEqual(old.df, nw.df))  {
		  changes = UpdateComponent(old.path, DefComponent, old.df, nw.df) :: changes
		}
		
		new StrictDiff(changes)
	}
	
	/**
   * compares two structures 
   * @param old the first structure
   * @param nw the second structure
   * @return the (strict) diff representing the difference between old and nw 
   */
  private def compareStructures(old : Structure, nw : Structure) : StrictDiff = {
	  var changes : List[StrictChange] = Nil

    if (old.from != nw.from) {
	    changes = UpdateComponent(old.path, DomComponent, Some(old.from), Some(nw.from)) :: changes
	  }
    //TODO changes to body of DeclaredStructure, definiens of DefinedStructure
    new StrictDiff(changes)
	}
	  
  /**
   * compares two patterns 
   * @param old the first pattern
   * @param nw the second pattern
   * @return the (strict) diff representing the difference between old and nw 
   */
	private def comparePatterns(old : Pattern, nw : Pattern) : StrictDiff = {
    var changes : List[StrictChange] = Nil

    if (old.params != nw.params){
      changes = UpdateComponent(old.path, ParamsComponent, Some(old.params), Some(nw.params)) :: changes
    }

    if (old.body != nw.body) {
      changes = UpdateComponent(old.path, PatternBodyComponent, Some(old.body), Some(nw.body)) :: changes
    }

	  new StrictDiff(changes)
	}
	
	/**
   * compares two instances 
   * @param old the first instance
   * @param nw the second instance
   * @return the (strict) diff representing the difference between old and nw 
   */
	private def compareInstances(old : Instance, nw : Instance) : StrictDiff = {
     var changes : List[StrictChange] = Nil
     if (old.tp != nw.tp) {
	    changes = UpdateComponent(old.path, TypeComponent, Some(old.tp), Some(nw.tp)) :: changes
	   }
     new StrictDiff(changes)
	}
	
	/**
   * compares two constant assignments 
   * @param old the first assignment
   * @param nw the second assignment
   * @return the (strict) diff representing the difference between old and nw 
   */
	private def compareConstantAssignments(old : ConstantAssignment, nw : ConstantAssignment) : StrictDiff = {
    var changes : List[StrictChange] = Nil

    if (old.target != nw.target) {
      changes = UpdateComponent(old.path, DefComponent, old.target, nw.target) :: changes
    }
	  
	  new StrictDiff(changes)
	}
	
  /**
   * compares two definitional link assignments 
   * @param old the first assignment
   * @param nw the second assignment
   * @return the (strict) diff representing the difference between old and nw 
   */
	private def compareDefLinkAssignments(old : DefLinkAssignment, nw : DefLinkAssignment) : StrictDiff = {
      var changes : List[StrictChange] = Nil
      if (old.target != nw.target) {
        changes = UpdateComponent(old.path, DefComponent, Some(old.target), Some(nw.target)) :: changes
      }
	  new StrictDiff(changes)
	}
	
  /**
   * compares two semi-formal declarations 
   * @param old the semi-formal declaration
   * @param nw the semi-formal declaration
   * @return the (strict) diff representing the difference between old and nw 
   */
	private def compareSFDeclarations(old : SFDeclaration, nw : SFDeclaration) : StrictDiff = {
      var changes : List[StrictChange] = Nil
      
      if (old.home != nw.home || old.tokens != nw.tokens) {
        changes = DeleteDeclaration(old) :: AddDeclaration(nw) :: changes 
      }
	  new StrictDiff(changes)
	}	
	
  /**
   * compares two declarations 
   * @param old the first declaration
   * @param nw the second declaration
   * @return the (strict) diff representing the difference between old and nw 
   */
  private def compareDeclarations(old : Declaration, nw : Declaration) : StrictDiff = {
    (old,nw) match {
      case (o : Constant, n : Constant) =>
        compareConstants(o,n)
      case (o : Structure, n : Structure) =>
        compareStructures(o,n)
      case (o : Pattern, n : Pattern) =>
        comparePatterns(o,n)
      case (o : Instance, n : Instance) =>
        compareInstances(o,n)
      case (o : ConstantAssignment, n : ConstantAssignment) =>
        compareConstantAssignments(o,n)
      case (o : DefLinkAssignment, n : DefLinkAssignment) =>
        compareDefLinkAssignments(o,n)
      case (o : SFDeclaration, n : SFDeclaration) => 
        compareSFDeclarations(o,n)
    }
  }
  
  /**
   * Gets the declarations in a module
   * @param m the module
   * @return the list of declarations in m
   */
  private def _declarations(m : Module) : List[Declaration] = {
    m.components.flatMap(x => x match {
      case d : Declaration => List(d)
      case FormalDeclaration(d) => List(d) //for semi formal modules (SFModule)
      case _ => Nil
    })
  }
    

	/**
	 * compares two modules
	 * @param old the first module
	 * @param nw the second module
	 * @return the (strict) diff representing the difference between old and nw
	 */
	private def compareModules(old : Module, nw : Module) : StrictDiff = {
      //getting all declarations stored in each library

	  val od = _declarations(old)
	  val nd = _declarations(nw)
	  
	  // checking for declarations pairs having the same name aka same declaration with two versions
	  // due to name uniqueness max size of each filtered list is 0 or 1
	  // making the final result contain all declaration names that exist in both library versions
	  val matched = nd.flatMap(n => od.filter(o => n.name.toString == o.name.toString).map((_,n)))
	  
	  //filtering away matched paths
	  val unmatchedold = od.filterNot(x => matched.exists(y => x.name.toString == y._1.name.toString))
	  val unmatchednew = nd.filterNot(x => matched.exists(y => x.name.toString == y._1.name.toString))

    //generating adds & deletes
    val oldch : List[StrictChange] = unmatchedold.map(x => DeleteDeclaration(x))
    val newch : List[StrictChange] = unmatchednew.map(x => AddDeclaration(x))

	//comparing declaration pairs to see how (if at all) they were updated over the two versions
	val updates : List[StrictChange] = matched.flatMap(x => compareDeclarations(x._1,x._2).changes)
	val innerChanges = new StrictDiff(updates ++ oldch ++ newch)
	  
	(old,nw) match {
	  case (o : DeclaredTheory, n : DeclaredTheory) =>
        var changes : List[StrictChange] = Nil
        (o.meta, n.meta) match {
          case (None,None) => None
          case (None,Some(p)) => changes = UpdateComponent(o.path, DomComponent, None, Some(OMMOD(p))) :: changes
          case (Some(p),None) => changes = UpdateComponent(o.path, DomComponent, Some(OMMOD(p)), None) :: changes
          case (Some(op),Some(np)) =>
            if (op != np) {
	    	      changes = UpdateComponent(o.path, DomComponent, Some(OMMOD(op)), Some(OMMOD(np))) :: changes
            }
        }
        new StrictDiff(changes) ++ innerChanges
	  case (o : DefinedTheory, n : DefinedTheory) =>
        var changes : List[StrictChange] = Nil
	    if (o.df != n.df) {
	        changes = UpdateComponent(o.path, DefComponent, Some(o.df), Some(n.df)) :: changes
        }
	    new StrictDiff(changes) ++ innerChanges 
	  case (o : DeclaredView, n : DeclaredView) =>
        var changes : List[StrictChange] = Nil
        if (o.from != n.from) {
          changes = UpdateComponent(o.path, DomComponent, Some(o.from), Some(n.from)) :: changes
        }
        if (o.to != n.to) {
          changes = UpdateComponent(o.path, CodComponent, Some(o.to), Some(n.to)) :: changes
        }
        new StrictDiff(changes) ++ innerChanges
	  case (o : DefinedView, n : DefinedView) =>
        var changes : List[StrictChange] = Nil
        if (o.from != n.from) {
          changes = UpdateComponent(o.path, DomComponent, Some(o.from), Some(n.from)) :: changes
        }
        if (o.to != n.to) {
          changes = UpdateComponent(o.path, CodComponent, Some(o.to), Some(n.to)) :: changes
        }
        if (o.df != n.df) {
          changes = UpdateComponent(o.path, DefComponent, Some(o.df), Some(n.df)) :: changes
        }
        new StrictDiff(changes) ++ innerChanges
	  case (o : SFModule, n : SFModule) => 
	    var changes : List[StrictChange] = Nil
	    if (o.tokens != n.tokens || o.path != n.path) {
	      changes = DeleteModule(o) :: AddModule(n) :: changes
	    }
	    new StrictDiff(changes)
	  }
	}
	
	/**
	 * compares two libraries
	 * @param old the first library
	 * @param nw the second library
	 * @return the (strict) diff representing the difference between old and nw
	 */
	def compareFlatLibraries(old : Library, nw : Library) : StrictDiff = {
	  
	  //getting all module URI's (paths) stored in each library
	  val ops = old.getAllPaths
	  val nps = nw.getAllPaths
	  
	  // checking for module pairs having the same URI aka same file with two versions
	  // due to path uniqueness max size of each filtered list is 0 or 1
	  // making the final result contain all paths that exist in both library versions
	  val matched = nps.flatMap(n => ops.filter(o => n.toPath == o.toPath))
	  
	  //filtering away matched paths
	  val unmatchedold = ops.filterNot(x => matched.exists(y => x.toPath == y.toPath))
	  val unmatchednew = nps.filterNot(x => matched.exists(y => x.toPath == y.toPath))
	  
	  //old module declarations become deletes
	  val oldch = unmatchedold.map(x => DeleteModule(old.getModule(x)))
	  
	  //new module declarations become adds
	  val newch = unmatchednew.map(y => AddModule(nw.getModule(y)))
	  
	  //comparing module pairs to see how (if at all) they were updated over the two versions
	  //since the match criterion was identical paths, we can reuse the path for both library versions
	  val updates = matched.map(x => compareModules(old.getModule(x), nw.getModule(x)))
	  updates.foldLeft(new StrictDiff(Nil))((r,x) => r ++ x) ++ new StrictDiff(oldch.toList) ++ new StrictDiff(newch.toList)
	}
	
}