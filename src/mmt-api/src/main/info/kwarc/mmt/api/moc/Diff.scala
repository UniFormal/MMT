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
      c2.backend.addStore(c.backend.getStores :_ *) //TODO this used to change the revision when creating c2

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
      if(old.tp != nw.tp) {
        changes = UpdateComponent(old.path, TypeComponent, old.tp, nw.tp) :: changes
     }
    if (old.df != nw.df)  {
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
    //TODO changes to body of Structure, definiens of DefinedStructure
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
    }
  }

  /**
   * compares two modules
   * @param old the first module
   * @param nw the second module
   * @return the (strict) diff representing the difference between old and nw
   */
  private def compareModules(old : Module, nw : Module) : StrictDiff = {
      //getting all declarations stored in each library

    def getInnerChanges(old: Module, nw: Module) = {
       val od = old.getDeclarations
       val nd = nw.getDeclarations

       // checking for declarations pairs having the same name aka same declaration with two versions
       // due to name uniqueness max size of each filtered list is 0 or 1
       // making the final result contain all declaration names that exist in both library versions
       val matched = nd.flatMap(n => od.filter(o => n.name == o.name).map((_,n)))

       //filtering away matched paths
       val unmatchedold = od.filterNot(x => matched.exists(y => x.name == y._1.name))
       val unmatchednew = nd.filterNot(x => matched.exists(y => x.name == y._1.name))

        //generating adds & deletes
        val oldch : List[StrictChange] = unmatchedold.map(x => DeleteDeclaration(x))
        val newch : List[StrictChange] = unmatchednew.map(x => AddDeclaration(x))

       //comparing declaration pairs to see how (if at all) they were updated over the two versions
       val updates : List[StrictChange] = matched.flatMap(x => compareDeclarations(x._1,x._2).changes)
       new StrictDiff(updates ++ oldch ++ newch)
    }

    (old,nw) match {
      case (o : Theory, n : Theory) =>
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
        if (o.df != n.df) {
          changes = UpdateComponent(o.path, DefComponent, o.df, n.df) :: changes
        }
       new StrictDiff(changes) ++ getInnerChanges(o, n)

      case (o : View, n : View) =>
        var changes : List[StrictChange] = Nil
        if (o.fromC.get != n.fromC.get) {
          changes = UpdateComponent(o.path, DomComponent, o.fromC.get, n.fromC.get) :: changes
        }
        if (o.toC.get != n.toC.get) {
          changes = UpdateComponent(o.path, CodComponent, o.toC.get, n.toC.get) :: changes
        }
        if (o.df != n.df) {
          changes = UpdateComponent(o.path, DefComponent, o.df, n.df) :: changes
        }
        new StrictDiff(changes)  ++ getInnerChanges(o, n)
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
