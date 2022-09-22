package info.kwarc.mmt.api.refactoring

/**
  * Theory intersections intersect two theories along a partial invertible view.
  * They can be a means of refactoring a theory graph.
  *
  * todo: This whole file needs to be reworked to improve coding style and to become idiomatic
  *       MMT and Scala code.
  *       E.g. [[Intersecter]] should arguably *not* be an `Extension`.
  */

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives.{Archive, BuildTarget, Build}
import info.kwarc.mmt.api.frontend.{Extension, Controller}
import info.kwarc.mmt.api.modules.{Theory, ModuleOrLink, View}
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.Renamer
import info.kwarc.mmt.api.objects.TraversingTranslator
import info.kwarc.mmt.api.objects.{Context, Traverser, Term, OMID}
import info.kwarc.mmt.api.presentation.{FileWriter, MMTSyntaxPresenter}
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils.FilePath

import scala.collection.mutable
import scala.util.{Try, Success}

abstract class Intersecter extends Extension {
  val DEBUG = true
  var optimizer = new GraphOptimizationTool()


  override def start(args : List[String]): Unit = {
    super.start(args)
    controller.extman.addExtension(optimizer)
  }

  private def debugOut(arg: Any): Unit = if (DEBUG) println(arg)

  /**
    *
    * @param view view over which the Intersecter intersects
    * @return
    */
  def apply(view: View): (List[Theory], List[(Theory, Theory)], List[Theory], List[(View, View)]) = {
    intersectGraph(view)
  }

  /** Intersect two theories over view
    *
    * Creates intersection of domain and codomain of view over said view.
    * Lower level theories will be intersected recursively.
    *
    * @param view View over which theories are intersected
    * @return
    */
  def intersectGraph(view: View): (List[Theory], List[(Theory, Theory)], List[Theory], List[(View,View)]) = {
    implicit val intersections: mutable.HashMap[(MPath, MPath), (Theory, Theory)] = collection.mutable.HashMap[(MPath, MPath), (Theory, Theory)]()
    val th1 = controller.getTheory(view.from.toMPath)
    val th2 = controller.getTheory(view.to.toMPath)
    implicit val renamings : mutable.HashMap[GlobalName, GlobalName] = mutable.HashMap[GlobalName, GlobalName]()
    implicit val viewMap : mutable.HashMap[MPath, View] = mutable.HashMap[MPath, View]()
    val views = recIntersect(th1, th2, view)
    val rem1 = remainder1(th1, th2, intersections, renamings)
    val rem2 = remainder2(th1, th2, intersections, renamings)
    (rem1, intersections.values.toSet.toList, rem2, views)
  }

  /** Recursive method to generate intersections
    *
    * Recursively generates intersections for dependencies of th1 and th2
    *
    * @param th1 left side theory to intersect
    * @param th2 right side theory to intersect
    * @param view view to intersect over
    * @param intersections map containing already computed theory intersections
    * @param renamings map of renamings of GlobalNames
    * @return
    */
  protected def recIntersect(th1: Theory, th2: Theory, view : View)
                            (implicit intersections: mutable.HashMap[(MPath, MPath), (Theory, Theory)], renamings: mutable.HashMap[GlobalName, GlobalName], viewMap : mutable.HashMap[MPath, View])
  : List[(View,View)] = {
    var views = List[(View,View)]()
    if (intersections.contains((th1.path, th2.path))) { //TODO optimize multiple calls for empty theory
      return views
    }
    val includes1 = collect_structures(th1)
    val includes2 = collect_structures(th2)

    //Recurse through dependent Theories of th1 and th2
    //th1:
    includes1.foreach {
      case PlainInclude(from, to) =>
        views ++= recIntersect(controller.getTheory(from), th2, view)
      case default =>
    }
    //th2:
    includes2.foreach {
      case PlainInclude(from, to) =>
        views ++= recIntersect(th1, controller.getTheory(from), view)
      case default =>
    }
    views ++ createIntersection(th1, th2, view, includes1, includes2)
  }

  protected def createIntersection(th1: Theory, th2: Theory, partialView : View, includes1 : List[Declaration], includes2 : List[Declaration])
                                  (implicit intersections: mutable.HashMap[(MPath, MPath), (Theory, Theory)], renamings: mutable.HashMap[GlobalName, GlobalName], viewMap : mutable.HashMap[MPath, View])
  : Option[(View,View)]

  protected def addNonTrivialIntersection(th1 : Theory, th2 : Theory, int1 : Theory, int2 : Theory, view : Option[(View,View)])
                                         (implicit intersections : mutable.HashMap[(MPath, MPath), (Theory, Theory)], viewMap : mutable.HashMap[MPath, View])
  : Option[(View,View)] = {

    var retview = view
    //add to controller
    controller.add(int1)
    controller.add(int2)

    //Check for redundancy
    val redundant1 = optimizer.findRedundantIncludes(int1.path).toSet
    val redundant2 = optimizer.findRedundantIncludes(int2.path).toSet

    for (decl <- int1.getDeclarations) {
      decl match {
        case PlainInclude(from, to) if redundant1.contains(from) => int1.delete(PlainInclude(from, to).name)
        case default =>
      }
    }
    for (decl <- int2.getDeclarations) {
      decl match {
        case PlainInclude(from, to) if redundant2.contains(from) => int2.delete(PlainInclude(from, to).name)
        case default =>
      }
    }
    var res1 = int1
    var res2 = int2
    //check that intersection is not trivial
    if (int1.getDeclarations.nonEmpty) {
      if (int1.getDeclarations.length==1) int1.getDeclarations.head match {
        case PlainInclude(from, to) =>
          res1 = controller.getTheory(from)
          controller.delete(int1.path)
          retview = None
        case default =>
      }
      if (int2.getDeclarations.length==1) {
        int2.getDeclarations.head match {
          case PlainInclude(from, to) =>
            res2 = controller.getTheory(from)
            controller.delete(int2.path)
            retview = None
          case default =>
        }
      }
      intersections.put((th1.path, th2.path), (res1, res2))
      retview match {
        case Some((v1,v2)) =>
          controller.add(v1)
          controller.add(v2)
          viewMap.put(int1.path, v1)
          viewMap.put(int2.path, v2)
        case None =>
      }
      retview
    } else {
      controller.delete(int1.path)
      controller.delete(int2.path)
      None
    }
  }

  protected def intersectionDeclarations(th1 : Theory, th2 : Theory, view_map: collection.immutable.Map[FinalConstant, FinalConstant]): List[Declaration] = {
    //debugOut(th1 + "\n" + th2)
    val res = getSubDeclarations(th1).filter {
      _ match {
        case c: FinalConstant =>
          view_map.contains(c) && (view_map(c).parent == th2.path || th2.getDeclarations.collect{case subModule: ModuleOrLink => subModule}.map(m => m.modulePath).contains(view_map(c).parent))
        case PlainInclude(from, to) => false
      }
    }
    res
  }

  protected def intersectionDefinedDeclarations(th : Theory, int : Theory, renamings : mutable.HashMap[GlobalName, GlobalName]) : List[Declaration] = {
    getSubDeclarations(th).filter {
      _ match {
        case c: FinalConstant => isDefinedIn(c, isDefinedInTraverserState(int,renamings, mutable.HashSet[MPath]()))
        case PlainInclude(from, to) => false
      }
    }
  }

  /** returns Declarations of theory including submodules
    *
    * returns List of all direct Declarations in a given Theory, including those that are declared in direct submodules
    *
    * @param th searched theory
    * @return list of all Declarations
    */
  protected def getSubDeclarations(th : Theory): List[Declaration] = {
    th.getDeclarations.flatMap {
      _ match {
        case c: FinalConstant => Some(c)
        case PlainInclude(from, to) => Some(PlainInclude(from, to))
        case s: Structure => s.getDeclarations
        //all cases?
        case default => None
      }
    }
  }

  protected def getRecIncludes(theory: Theory) : List[Theory] = {
    val includes = theory.getIncludesWithoutMeta.map(controller.getTheory(_))
    includes ++ includes.flatMap(getRecIncludes)
  }

  protected def getFlatDeclarations(theory : Theory) : List[Declaration] = {
    getRecIncludes(theory).flatMap(_.getDeclarations)++theory.getDeclarations
  }

  /** Fill remainder of intersection with constants
    *
    * @param rem remainder theory to be filled
    * @param th1 original theory from which to fill remainder
    * @param th2 other theory
    * @param intersections map of theory intersections
    * @param renamings map of renamings of GlobalNames
    * @return
    */
  protected def fillConstantsRemainder1(rem : Theory, th1 : Theory, th2 : Theory, intersections : mutable.HashMap[(MPath, MPath), (Theory, Theory)], renamings : mutable.HashMap[GlobalName, GlobalName]) : Theory  = {
    if (intersections.contains((th1.path, th2.path))) rem.add(PlainInclude(intersections.get(th1.path, th2.path).get._1.path, rem.path))
    val intersected = intersections.get(th1.path, th2.path) match {
      case Some((t1, t2)) => getFlatDeclarations(t1).map(d => d.name).toSet
      case None => mutable.HashSet[LocalName]()
    }
    getSubDeclarations(th1).filter(_ match {
      case c : FinalConstant => !(renamings.contains(c.path) && intersected.contains(renamings(c.path).name))
      case PlainInclude(from, to) => false //includes are handled separately
      case s : Structure => true //structures are incompatible with deep intersections
      case default => ???
    }).foreach(addDeclaration(_, rem, renamings))

    rem
  }

  /** Fill remainder of intersection with constants
    *
    * @param rem remainder theory to be filled
    * @param th1 other theory
    * @param th2 original theory from which to fill remainder
    * @param intersections map of theory intersections
    * @param renamings map of renamings of GlobalNames
    * @return
    */
  protected def fillConstantsRemainder2(rem : Theory, th1 : Theory, th2 : Theory, intersections : mutable.HashMap[(MPath, MPath), (Theory, Theory)], renamings : mutable.HashMap[GlobalName, GlobalName]) : Theory  = {
    if (intersections.contains((th1.path, th2.path))) rem.add(PlainInclude(intersections.get(th1.path, th2.path).get._2.path, rem.path))
    val intersected = intersections.get(th1.path, th2.path) match {
      case Some((t1, t2)) => getFlatDeclarations(t2).map(d => d.name).toSet
      case None => mutable.HashSet[LocalName]()
    }
    getSubDeclarations(th2).filter(_ match {
      case c : FinalConstant => !(renamings.contains(c.path) && intersected.contains(renamings(c.path).name))
      case PlainInclude(from, to) => false //includes are handled separately
      case s : Structure => true //structures are incompatible with deep intersections
      case default => ???
    }).foreach(addDeclaration(_, rem, renamings))

    rem
  }

  /** Recursively create remainders of the right side theories of the intersection
    *
    * @param th1 left side theory
    * @param th2 right side theory
    * @param intersections map containing already computed theory intersections
    * @param renamings map of renamings of GlobalNames
    */
  protected def remainder1(th1: Theory, th2: Theory, intersections: mutable.HashMap[(MPath, MPath), (Theory, Theory)], renamings: mutable.HashMap[GlobalName, GlobalName]): List[Theory] = {
    val includes1 = collect_structures(th1)
    val rem = Theory.empty(th1.parent, LocalName(th1.name.toString+"R"), th1.meta) // T'

    //Recurse through dependent Theories of th1
    val rem_list = includes1.flatMap(_ match {
      case Include(IncludeData(home, from, args, df, total)) =>
        val rem_pre = remainder1(controller.getTheory(from), th2, intersections, renamings)
        addDeclaration(Include(rem.toTerm, rem_pre.last.path, args), rem, renamings)
        rem_pre
      case SimpleDeclaredStructure(home, name, from, isImplicit, istotal) =>
        addDeclaration(SimpleDeclaredStructure(rem.toTerm, name, from, isImplicit, istotal), rem, renamings)
        List()
      case default => ???
    })
    //Fill remainder with remaining constants
    fillConstantsRemainder1(rem, th1, th2, intersections, renamings)
    controller.add(rem)
    rem_list :+ rem
  }

  /** Recursively create remainders of the right side theories of the intersection
    *
    * @param th1 left side theory
    * @param th2 right side theroy
    * @param intersections map containing already computed theory intersections
    * @param renamings map of renamings of GlobalNames
    */
  protected def remainder2(th1: Theory, th2: Theory, intersections: mutable.HashMap[(MPath, MPath), (Theory, Theory)], renamings : mutable.HashMap[GlobalName, GlobalName]): List[Theory] = {
    val includes2 = collect_structures(th2)
    val rem = Theory.empty(th2.parent, LocalName(th2.name.toString+"R"), th2.meta) // T'

    //Recurse through dependent Theories of th2
    val rem_list = includes2.flatMap(_ match {
      case Include(IncludeData(home, from, args, df, total)) =>
        val rem_pre = remainder2(th1, controller.getTheory(from), intersections, renamings)
        addDeclaration(Include(rem.toTerm, rem_pre.head.path, args), rem, renamings)
        rem_pre
      case SimpleDeclaredStructure(home, name, from, isImplicit, istotal) =>
        addDeclaration(SimpleDeclaredStructure(rem.toTerm, name, from, isImplicit, istotal), rem, renamings)
        List()
      case default => ???
    })
    //Fill remainder with remaining constants
    fillConstantsRemainder2(rem, th1, th2, intersections, renamings)
    controller.add(rem)
    rem_list :+ rem
  }

  /** Move declaration to another theory
    *
    * Moves a declaration to a different theory, substituting all global names with the renaming
    *
    * @param dec declaration to be moved
    * @param th new home theory
    * @param renamings map of renamings of GlobalNames
    * @return
    */
  protected def addDeclaration(dec : Declaration, th : Theory, renamings : mutable.HashMap[GlobalName, GlobalName]): dec.ThisType = {
    val renamer = Renamer(name => renamings.get(name))
    val translator = TraversingTranslator(renamer)

    val renamed = dec.translate(th.toTerm, LocalName.empty, translator, Context())
    try th.add(renamed) catch {
      case _ : AddError => /* ignore */
    }
    renamings.put(dec.path, renamed.path)
    renamed
  }

  /** As above, but for collections
    *
    * @param decs collection of declarations
    * @param th target theory
    * @param renamings map of renamings of GlobalNames
    * @return
    */
  protected def addDeclaration(decs : Iterable[Declaration], th : Theory, renamings : mutable.HashMap[GlobalName, GlobalName]) : List[Declaration] = {
    decs.map(addDeclaration(_, th, renamings)).toList
  }

  /** collect all Includes in theory
    *
    * @param theory theory in which structures are to be collected
    * @return
    */
  protected def collect_structures(theory : Theory): List[Declaration] = theory.getDeclarations.filter(_ match {
    case PlainInclude(from, to) => true
    case default => false
  })

  case class isDefinedInTraverserState(theory: Theory, renamings : mutable.HashMap[GlobalName, GlobalName], used : mutable.HashSet[MPath])

  /** Traverser finding used theories
    *
    * This object is a traverser and searches a theory for all theories that are used
    */
  object isDefinedIn extends Traverser[isDefinedInTraverserState] {
    /** Traverses terms
      *
      * Traverses over terms, finding any used theories
      * @param t This is the current subterm
      * @param con This is the current context
      * @param state This is the traverser's state
      * @return
      */
    def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
      // look for URIs
      case OMID(path) =>
        // cut path to module path
        state match {
          case isDefinedInTraverserState(_,renamings,used) =>
            path match {
              case gname: GlobalName => used.add(renamings.getOrElse(gname, path).module)
              case default => ??? //TODO?
            }
          case default =>
        }
        OMID(path)

      // in all other cases, traverse
      case t =>
        Traverser(this, t)
    }

    /** Applies to Declaration
      *
      * Searches a Declaration for its used theories, adds them to state
      * @param decl This is the Declaration to be searched
      * @param state This is the traverser's state
      */
    def apply(decl: Declaration, state: State): Boolean = {
      decl match {
        case c: Constant =>
          c.df match {
            case Some(t) =>
              traverse(t) (Context(), state)
              state match {
                case isDefinedInTraverserState(theory,_,used) => used.subsetOf(getRecIncludes(theory).map(th => th.path).toSet+theory.path)
                case default => false
              }
            case _ => false
          }
        case _ => false
      }
    }
  }
}

class BinaryIntersecter extends Intersecter {
  /**
    * Creates new (potentially partial) view by restricting it to the domain (and codomain) of the given theories
    *
    * renamings are used to translate the partial view into the correct domain.
    *
    * @param vm map of partial view that is to be restricted
    * @param th1 domain theory
    * @param th2 codomain theory
    * @param renamings map of renamings of GlobalNames
    * @return restricted view
    */
  def restrictVM(vm: scala.collection.immutable.Map[FinalConstant,FinalConstant], parent : DPath, th1: Theory, th2: Theory)
                (implicit renamings: mutable.HashMap[GlobalName, GlobalName], viewMap : mutable.HashMap[MPath, View])
  : View = {
    val res = new View(parent, LocalName(th1.name.toString+"to"+th2.name.toString), TermContainer(th1.toTerm), TermContainer(th2.toTerm), new TermContainer, false)
    th1.getDeclarations.flatMap(_ match {case PlainInclude(from, to) => Some(PlainInclude(from, to)) case default => None}).foreach(inc => {
      res.add(Include(inc.home, inc.from.toMPath, List(), Some(viewMap(inc.from.toMPath).toTerm)))
    })
    for (c1 <- vm.keys) res.add(Constant(res.toTerm, ComplexStep(c1.parent)/c1.name, List(), TermContainer.empty(), TermContainer(vm(c1).toTerm), None, NotationContainer.empty()))
    res
  }

  override def createIntersection(th1: Theory, th2: Theory, partialView : View, includes1 : List[Declaration], includes2 : List[Declaration])
                                 (implicit intersections: mutable.HashMap[(MPath, MPath), (Theory, Theory)], renamings: mutable.HashMap[GlobalName, GlobalName], viewMap : mutable.HashMap[MPath, View])
  : Option[(View,View)] = {
    //Generate intersection of th1 and th2 over view and add them to intersections map
    var int1 = Theory.empty(th1.parent, LocalName(th1.name.toString+"Intersects"+th2.name.toString), th1.meta)
    var int2 = Theory.empty(th2.parent, LocalName(th2.name.toString+"Intersects"+th1.name.toString), th2.meta)

    //Generate inclusions of dependent intersections
    includes1.foreach {
      case PlainInclude(from, to) =>
        intersections.get((from, th2.path)).foreach { case (pre_int1, pre_int2) =>
          addDeclaration(PlainInclude(pre_int1.path, int1.path), int1, renamings)
          addDeclaration(PlainInclude(pre_int2.path, int2.path), int2, renamings)
        }
      case s: Structure => ??? //Ignored due inconsistent semantics
      case default => ???
    }
    includes2.foreach {
      case PlainInclude(from, to) =>
        intersections.get((th1.path, from)).foreach { case (pre_int1, pre_int2) =>
          addDeclaration(PlainInclude(pre_int1.path, int1.path), int1, renamings)
          addDeclaration(PlainInclude(pre_int2.path, int2.path), int2, renamings)
        }
      case s: Structure => ??? //Ignored due inconsistent semantics
      case default => ???
    }
    val view_map = ViewSplitter.getPairs(partialView, th1, th2)(controller).toMap
    val view_map_inverse = ViewSplitter.getPairs(partialView, th1, th2)(controller).map(_.swap).toMap
    fillConstantsIntersection(th1, th2, int1, int2, view_map, view_map_inverse, renamings)
    val view = restrictVM(view_map, partialView.parent, int1, int2)
    val view_inverse = restrictVM(view_map_inverse, partialView.parent, int2, int1)
    //add intersections to map
    addNonTrivialIntersection(th1, th2, int1, int2, Some((view,view_inverse)))
  }

  /** Creates intersection
    *
    * creates intersection of th1 and th2 over view
    *
    * @param th1 left side theory to intersect
    * @param th2 right side theory to intersect
    * @param view_map mapping of view to intersect over
    * @param view_map_inverse reverse mapping of view to intersect over
    * @param renamings map of renamings of GlobalNames
    * @return
    */
  protected def fillConstantsIntersection(th1: Theory, th2: Theory, int1 : Theory, int2 : Theory, view_map: collection.immutable.Map[FinalConstant, FinalConstant], view_map_inverse: collection.immutable.Map[FinalConstant, FinalConstant], renamings: mutable.HashMap[GlobalName, GlobalName]): (Theory, Theory) = {
    //Add constants from th1
    intersectionDeclarations(th1, th2, view_map).map(addDeclaration(_, int1, renamings))
    intersectionDefinedDeclarations(th1, int1, renamings).map(addDeclaration(_, int1, renamings))

    //Add constants from th2
    intersectionDeclarations(th2, th1, view_map_inverse).map(addDeclaration(_, int2, renamings))
    intersectionDefinedDeclarations(th2, int2, renamings).map(addDeclaration(_, int2, renamings))

    (int1, int2)
  }
}

class UnaryIntersecter extends Intersecter {

  override def createIntersection(th1: Theory, th2: Theory, partialView : View, includes1 : List[Declaration], includes2 : List[Declaration])
                                 (implicit intersections: mutable.HashMap[(MPath, MPath), (Theory, Theory)], renamings: mutable.HashMap[GlobalName, GlobalName], viewMap : mutable.HashMap[MPath, View])
  : Option[(View,View)] = {
    //Generate intersection of th1 and th2 over view and add them to intersections map
    var int = Theory.empty(th1.parent, LocalName(th1.name.toString+"Intersects"+th2.name.toString), th1.meta)

    //Generate inclusions of dependent intersections
    includes1.foreach {
      case PlainInclude(from, to) =>
        intersections.get((from, th2.path)).foreach { case (pre_int1, _) =>
          addDeclaration(PlainInclude(pre_int1.path, int.path), int, renamings)
        }
      case s: Structure => ??? //Ignored due inconsistent semantics
      case default => ???
    }
    includes2.foreach {
      case PlainInclude(from, to) =>
        intersections.get((th1.path, from)).foreach { case (pre_int1, _) =>
          addDeclaration(PlainInclude(pre_int1.path, int.path), int, renamings)
        }
      case s: Structure => ??? //Ignored due inconsistent semantics
      case default => ???
    }
    val view_map = ViewSplitter.getPairs(partialView, th1, th2)(controller).toMap
    val view_map_inverse = ViewSplitter.getPairs(partialView, th1, th2)(controller).map(_.swap).toMap
    fillConstantsIntersection(th1, th2, int, view_map, view_map_inverse, renamings)
    //add intersections to map
    addNonTrivialIntersection(th1, th2, int, int, None)
  }

  /** Creates intersection
    *
    * creates intersection of th1 and th2 over view
    *
    * @param th1 left side theory to intersect
    * @param th2 right side theory to intersect
    * @param view_map mapping of view to intersect over
    * @param view_map_inverse reverse mapping of view to intersect over
    * @param renamings map of renamings of GlobalNames
    * @return
    */
  protected def fillConstantsIntersection(th1: Theory, th2: Theory, int : Theory, view_map: collection.immutable.Map[FinalConstant, FinalConstant], view_map_inverse: collection.immutable.Map[FinalConstant, FinalConstant], renamings: mutable.HashMap[GlobalName, GlobalName]): (Theory, Theory) = {
    //Add constants from th1
    intersectionDeclarations(th1, th2, view_map).map {
      case c: FinalConstant =>
        addDeclaration(c, int, renamings)
        renamings.put(view_map(c).path, renamings(c.path))
    }
    //No defined constants in unary intersection

    (int, int)
  }
}

/** ViewSplitter splits views into lists of pairs
  * Original code by Dennis Mueller
  * with some modifications
  */
object ViewSplitter {

  def apply(v : View) ( implicit controller : Controller) : List[(FinalConstant,FinalConstant)] = {
    getPairs(v, controller.getTheory(v.from.toMPath), controller.getTheory(v.to.toMPath))
  }

  /**
    * Splits a view up in Pairs of FinalConstants ; takes only those, that are directly elements
    * of (dom x cod)
    * @param v view
    * @param dom view's domain
    * @param cod view's codomain
    * @return
    */

  def getPairs(v:View, dom:Theory, cod:Theory) ( implicit controller : Controller) : List[(FinalConstant,FinalConstant)]= {
    val domconsts = v.getDeclarations.flatMap(d => d.name.head match {
      case ComplexStep(p) if p==dom.path => Some(d)
      case default => None
    }) collect {
      case c: FinalConstant if c.df.isDefined => c
    }
    (for {o <- domconsts} yield (o.name.tail,o.df.get)).filter(p =>
      p._2.head.get.module == cod.path
    ).map(p => (getConst(dom, p._1),getConst(cod, p._2.head.get.name))) collect {
      case (c1:FinalConstant,c2:FinalConstant) => (c1,c2)
    }
  }

  def getConst(th : Theory, name : LocalName): FinalConstant = {
    name.toList match {
      case head::Nil => th.get(LocalName(head)) match {
        case o : FinalConstant => o
      }
      case submoduleName::tail =>
        val submodule = th.get(LocalName(submoduleName)).asInstanceOf[ModuleOrLink]
        getConstSub(submodule, tail)
      case default => ???
    }
  }

  def getConstSub(m : ModuleOrLink, name : LocalName): FinalConstant = {
    m match {
      case th : Theory => getConst(th, name)
      case st: Structure =>
        st.get(ComplexStep(st.from.toMPath)/name) match {case c : FinalConstant => c}
    }
  }

  /**
    * As above but takes TWO views
    * @param v1 : dom -> cod
    * @param v2 : cod -> dom and returns the paired declarations sorted by type:
    *           (constant -> constant)
    *           (constant -> term) and
    *           (term <- constant)
    * @return paired declarations as a triple of lists
    */

  def getPairs(v1:View, v2:Option[View], dom:Theory, cod:Theory)
  : (List[(FinalConstant,FinalConstant)],List[(FinalConstant,Term)],List[(Term,FinalConstant)]) = {

    val consts1 = ((for {o <- v1.getDeclarations if o.name.head.toString=="["+dom.path+"]"} yield o) collect {
      case c: FinalConstant if c.df.isDefined => c
    }).map(c => Try({
      val c1 = dom.get(c.name.tail)
      val c2 = if (c.df.get.head.get.module==cod.path) cod.get(c.df.get.head.get.name) match {
        case d:FinalConstant => d
        case _ => c.df.get
      } else c.df.get
      (c1,c2)
    })) collect {case Success((a,b)) => (a,b)}

    val consts2 = v2 match {
      case Some(v) => ((for {o <- v.getDeclarations if o.name.head.toString == "[" + cod.path + "]"} yield o) collect {
        case c: FinalConstant if c.df.isDefined => c
      }).map(c => Try({
        val c1 = cod.get(c.name.tail)
        val c2 = if (c.df.get.head.get.module == dom.path) dom.get(c.df.get.head.get.name) match {
          case d: FinalConstant => d
          case _ => c.df.get
        } else c.df.get
        (c2, c1)
      })) collect {case Success((a,b)) => (a,b)}
      case None => List()
    }
    val all = (consts1:::consts2).distinct
    for(i <- 0 to all.length-2 ; j <- i + 1 until all.length) if (all(i)._1==all(j)._1 || all(i)._2==all(j)._2) return (Nil,Nil,Nil)
    (all collect {case (c:FinalConstant,d:FinalConstant) => (c,d)},
      all collect {case (c:FinalConstant,t:Term) => (c,t)},
      all collect {case (t:Term, c:FinalConstant) => (t,c)})
  }
}

/** Generic BuildTarget for creating intersections
  *
  * @tparam GE type of GraphEvaluator with which to measure quality of the resulting graph, eg counting declarations
  * @tparam I type of the Intersecter to use, eg binary intersections
  */
class FindIntersecter[I <: Intersecter, GE <: GraphEvaluator](intersecter : I, graphEvaluator : GE) extends BuildTarget {

  var syntaxPresenter : MMTSyntaxPresenter = _

  override def start(args: List[String]): Unit = {
    super.start(args)

    controller.extman.addExtension(intersecter)
    controller.extman.addExtension(graphEvaluator)
    try {
      syntaxPresenter = controller.extman.get(classOf[MMTSyntaxPresenter]).head
    } catch {
      case e : Exception =>
        syntaxPresenter = new MMTSyntaxPresenter()
        controller.extman.addExtension(syntaxPresenter)
    }
  }

  /** Searches for views over which to intersect and then does so
    *
    * views are found via the viewfinder
    * intersections done via intersecter of parameterized type I
    * intersection graphs are evaluated using evaluation function of parameterized type GE
    *
    * @param a Archive in which intersections are searched for
    * @return intersected graph as pair of theories and views and a numeric value of their evaluation
    */
  def findIntersections(a : Archive) : List[((List[Theory],List[View]), Int)] = {
    //apply viewfinder
    val viewFinder = new ViewFinder
    if (controller.extman.get(classOf[Preprocessor]).exists(p => p.key != "" && a.id.startsWith(p.key))) {
      val preproc = (SimpleParameterPreprocessor + DefinitionExpander).withKey(a.id)
      controller.extman.addExtension(preproc)
    }
    controller.extman.addExtension(viewFinder, List(a.id))
    val theories = a.allContent.flatMap({(p:MPath) => try {controller.get(p) match { case dt : Theory => Some(p) case _ => None}} catch {case e : Exception => None}})
    while(!viewFinder.isInitialized) {
      Thread.sleep(500)
    }
    val views = theories.flatMap(t => viewFinder.find(t, a.id).filter(v => v.from!=v.to).filter(v => v.getDeclarations.nonEmpty))
    /*
    //export views to file
    implicit val fw = new FileWriter(new java.io.File(a.root + "/export/intersections/"+a.id+"View.mmt"))
    views.foreach(v => controller.add(v))
    views.foreach(syntaxPresenter.apply(_))
    fw.done
    */

    //phase 2 : intersect
    val intersections = views.map(v => (intersecter.intersectGraph(v), (v.from.toMPath, v.to.toMPath)))

    //phase 3 : sort
    //This may need some cleaning up
    val res = intersections.map { case ((l1, l2, l3, v), (t1, t2)) => ((l2.flatMap(p => List(p._1, p._2)) ++ l1 ++ l3, v.flatMap(p => List(p._1, p._2))), (t1, t2)) }.map(i => (i, graphEvaluator.eval(i._1._1, i._1._2))).sortBy(_._2)
    val filterSet = mutable.HashSet[Theory]()
    res.filter(_._2>0 | true).filter(_ match {
      case (((theories, views), (t1, t2)), int) =>
        val dependencies = List(controller.getTheory(t1), controller.getTheory(t2))
        !dependencies.exists(filterSet.contains) && {
          filterSet ++= dependencies
          true
        }
      case default => false
    }).map(r => (r._1._1, r._2))
  }

  /** a string identifying this build target, used for parsing commands, logging, error messages */
  def key: String = "intersections"

  /** clean intersections in a given archive */
  def clean(a: Archive, in: FilePath): Unit = {
    val file = new java.io.File(a.root.toString + "/export/intersections/"+a.id+".mmt")
    file.delete()
  }

  /** build or update intersections in a given archive */
  def build(a: Archive, w: Build, in: FilePath, errorCont: Option[ErrorHandler]): Unit = {
    val res = findIntersections(a)
    implicit val fw: FileWriter = new FileWriter(new java.io.File(a.root.toString + "/export/intersections/"+a.id+".mmt"))
    res.foreach(r => {
      r._1._1.foreach(syntaxPresenter.apply(_))
      r._1._2.foreach(syntaxPresenter.apply(_))
    })
    fw.done
  }
}

class FindBinaryIntersecter extends FindIntersecter(new BinaryIntersecter, new KnowledgeGainGraphEvaluator)

/**
  * Abstract class for GraphEvaluators.
  * eval(g1)>eval(g2) <=> g1 is better than g2
  */
abstract class GraphEvaluator extends Extension{
  def apply(graph : List[Theory], views : List[View]=List.empty[View]): Int = eval(graph, views)

  /** Evalutes a graph given as a list of theories and views according to an evaluation function
    *
    * @param theories theories of the graph
    * @param views views of the graph
    * @return evaluation as integer value
    */
  def eval(theories : List[Theory], views : List[View]=List.empty[View]) : Int
}

/**
  * Graph Evaluator that counts the number of declarations and structures
  */
class KnowledgeGraphEvaluator extends GraphEvaluator {
  override def eval(theories : List[Theory], views : List[View]=List.empty[View]) : Int = {
    var ind = 0
    for (theory <- theories) {
      for (dec <- theory.getDeclarations) {
        dec match {
          case _ : Constant =>
            ind +=1
          case _ =>
        }
      }
    }
    for (view <- views) {
      controller.get(view.from.toMPath).getDeclarations.map(
        {
          case const : Constant =>
            if (const.df.nonEmpty) {
              ind +=1
            }
          case _ =>
        }
      )
    }
    ind
  }
}

/**
  * Graph Evaluator that counts the number of declarations and structures
  */
class RepresentationGraphEvaluator extends GraphEvaluator {
  override def eval(theories : List[Theory], views : List[View]=List.empty[View]) : Int = {
    var count = 0
    for (theory <- theories) {
      count -= theory.getDeclarations.length
    }
    count
  }
}

/**
  * GraphEvaluator that calculates #ind/#rep
  */
class KnowlDivRepGraphEvaluator extends GraphEvaluator {
  override def eval(theories : List[Theory], views : List[View]=List.empty[View]) : Int = {
    var ind = 0
    var rep = 1
    for (theory <- theories) {
      for (dec <- theory.getDeclarations) {
        dec match {
          case _ : Constant =>
            ind +=1
            rep +=1
          case _ => rep +=1
        }
      }
    }
    for (view <- views) {
      controller.get(view.from.toMPath).getDeclarations.map(
        {
          case const : Constant =>
            if (const.df.nonEmpty) {
              ind +=1
            }
          case _ =>
        }
      )
    }
    ind/rep
  }
}

/**
  * GraphEvaluator that calculates Knowledge gain(#ind-#rep)
  */
class KnowledgeGainGraphEvaluator extends GraphEvaluator {
  override def eval(theories: List[Theory], views : List[View]=List.empty[View]): Int = {
    var count = 0
    for (theory <- theories) {
      for (dec <- theory.getDeclarations) {
        dec match {
          case s: Structure => count -= 1
          case _ =>
        }
      }
    }
    for (view <- views) {
      controller.get(view.from.toMPath).getDeclarations.map(
        {
          case const : Constant =>
            if (const.df.nonEmpty) {
              count +=1
            }
          case _ =>
        }
      )
    }
    count
  }
}