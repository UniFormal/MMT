package info.kwarc.mmt.api.refactoring

import java.io.PrintWriter

import info.kwarc.mmt.api.archives.{Archive, BuildTarget, Update}
import info.kwarc.mmt.api.{ComplexStep, GlobalName, LocalName, MPath}
import info.kwarc.mmt.api.frontend.{Controller, Extension}
import info.kwarc.mmt.api.modules.{ModuleOrLink, Theory, View}
import info.kwarc.mmt.api.objects.{Context, Term}
import info.kwarc.mmt.api.presentation.Presenter
import info.kwarc.mmt.api.symbols.{Declaration, FinalConstant, IdentityTranslator, Include, PlainInclude, Renamer, SimpleDeclaredStructure, Structure, TraversingTranslator}
import info.kwarc.mmt.api.utils.FilePath

import scala.collection.mutable
import scala.util.{Success, Try}

class Intersecter extends Extension {
  val DEBUG = true

  private def debugOut(arg: Any) = if (DEBUG) println(arg)

  /**
    *
    * @param view
    * @return
    */
  def apply(view: View) = {
    intersect(view)
  }

  /** Intersect two theories over view
    *
    * Creates intersection of domain and codomain of view over said view.
    * Lower level theories will be intersected recursively.
    *
    * @param view View over which theories are intersected
    * @return
    */
  def intersect(view: View): (List[Theory], List[(Theory, Theory)], List[Theory]) = {
    val intersections = collection.mutable.HashMap[(MPath, MPath), (Theory, Theory)]()
    val view_map = ViewSplitter(view)(controller).toMap
    val view_map_inverse = ViewSplitter(view)(controller).map(_.swap).toMap
    val th1 = controller.getTheory(view.from.toMPath)
    val th2 = controller.getTheory(view.to.toMPath)
    val renamings = mutable.HashMap[GlobalName, GlobalName]()
    intersect(th1, th2, view_map, view_map_inverse, intersections, renamings)
    val rem1 = remainder1(th1, th2, view_map, view_map_inverse, intersections, renamings)
    val rem2 = remainder2(th1, th2, view_map, view_map_inverse, intersections, renamings)
    (rem1, intersections.values.toList, rem2)
  }

  protected def interSectionDeclarations(th1 : Theory, th2 : Theory, view_map: collection.immutable.Map[FinalConstant, FinalConstant]) = {
    th1.getDeclarations.flatMap {
      _ match {
        case c: FinalConstant => Some(c)
        case PlainInclude(from, to) => Some(PlainInclude(from, to)) //TODO correct theories
        case s: Structure => None //TODO
        //TODO all cases???
        case default => None
      }
    }.filter {
      _ match {
        case c: FinalConstant => view_map.contains(c) && view_map.get(c).get.parent == th2.path
        case PlainInclude(from, to) => true
      }
    }
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
    interSectionDeclarations(th1, th2, view_map).map(addDeclaration(_, int1, renamings))

    //Add constants from th2
    interSectionDeclarations(th2, th1, view_map_inverse).map(addDeclaration(_, int2, renamings))

    (int1, int2)
  }

  /** Recursive method to generate intersections
    *
    * Recursively generates intersections for dependencies of th1 and th2
    *
    * @param th1 left side theory to intersect
    * @param th2 right side theory to intersect
    * @param view_map mapping of view to intersect over
    * @param view_map_inverse reverse mapping of view to intersect over
    * @param intersections map of generated intersections, is filled recursively in this method
    * @param renamings map of renamings of GlobalNames
    * @return
    */
  protected def intersect(th1: Theory, th2: Theory, view_map: collection.immutable.Map[FinalConstant, FinalConstant], view_map_inverse: collection.immutable.Map[FinalConstant, FinalConstant], intersections: mutable.HashMap[(MPath, MPath), (Theory, Theory)], renamings: mutable.HashMap[GlobalName, GlobalName]): Unit = {
    debugOut("call: "+th1+th2)
    if (intersections.contains((th1.path, th2.path))) { //TODO optimize multiple calls for empty theory
      return
    }
    val includes1 = collect_structures(th1)
    val includes2 = collect_structures(th2)

    //Recurse through dependent Theories of th1 and th2
    //th1:
    includes1.foreach(_ match {
      case SimpleDeclaredStructure(home, name, from, isImplicit) =>
        intersect(controller.getTheory(from), th2, view_map, view_map_inverse, intersections, renamings)
        //remainder1(controller.getTheory(from), th2, view_map, view_map_inverse, intersections, renamings)
      case default => ???
    })
    //th2:
    includes2.foreach(_ match {
      case SimpleDeclaredStructure(home, name, from, isImplicit) =>
        intersect(th1, controller.getTheory(from), view_map, view_map_inverse, intersections, renamings)
        //remainder2(th1, controller.getTheory(from), view_map, view_map_inverse, intersections, renamings)
      case default => ???
    })

    //Generate intersection of th1 and th2 over view and add them to intersections map
    var int1 = Theory.empty(th1.parent, LocalName(th1.name.toString+"∩"+th2.name.toString), th1.meta)
    var int2 = Theory.empty(th2.parent, LocalName(th2.name.toString+"∩"+th1.name.toString), th2.meta)

    //Generate inclusions of dependent intersections
    includes1.foreach{_ match {
      case Include(_, from, args) =>
        intersections.get((from, th1.path)).foreach (
          _ match { case (pre_int1, pre_int2) =>
            int1.add(PlainInclude(int1.path, pre_int1.path))
            int2.add(PlainInclude(int2.path, pre_int2.path))
          }
        )
      case s : Structure => //TODO
      case default => ???
    }}
    includes2.foreach{_ match {case Include(_, from, args) =>
      case Include(_, from, args) =>
        intersections.get((th2.path, from)).foreach (
          _ match { case (pre_int1, pre_int2) =>
            int1.add(PlainInclude(int1.path, pre_int1.path))
            int2.add(PlainInclude(int2.path, pre_int2.path))
          }
        )
      case s : Structure => //TODO
      case default => ???
    }}

    fillConstantsIntersection(th1, th2, int1, int2, view_map, view_map_inverse, renamings)

    //check that intersection is not trivial
    if (int1.getDeclarations.nonEmpty) {
      if (int1.getDeclarations.length==1) {
        int1.getDeclarations.head match {
          case PlainInclude(from, to) => int1 = controller.getTheory(from)
          case default => controller.add(int1)
        }
      }
      else controller.add(int1)
      if (int2.getDeclarations.length==1) {
        int1.getDeclarations.head match {
          case PlainInclude(from, to) => int2 = controller.getTheory(from)
          case default => controller.add(int2)
        }
      }
      else controller.add(int2)
      intersections.put((th1.path, th2.path), (int1, int2))
    }

  }

  /** Fill remainder of intersection with constants
    *
    * @param rem remainder theory to be filled
    * @param targetTheory original theory from which to fill remainder
    * @param intersectedTheory intersection between targetTheory and largest theory of the other side
    * @param intersected collection of intersected constants
    * @param intersection
    * @param renamings
    * @return
    */
  protected def fillConstantsRemainder(rem : Theory, targetTheory : Theory, intersectedTheory : Theory, intersected: Set[FinalConstant], intersection : Theory, renamings : mutable.HashMap[GlobalName, GlobalName]) : Theory  = {
    rem.add(PlainInclude(intersection.path, rem.path))
    targetTheory.getDeclarations.filter(_ match {
      case c : FinalConstant => !intersected.contains(c)
      case PlainInclude(from, to) => false
      case s : Structure => false //TODO
        //TODO all cases???
      case default => ???
    }).foreach(addDeclaration(_, rem, renamings))

    rem
  }

  /** Recursively create remainders of the right side theories of the intersection
    *
    * @param th1 left side theory
    * @param th2 right side theory
    * @param view_map
    * @param view_map_inverse
    * @param intersections
    * @param renamings
    */
  protected def remainder1(th1: Theory, th2: Theory, view_map: collection.immutable.Map[FinalConstant, FinalConstant], view_map_inverse: collection.immutable.Map[FinalConstant, FinalConstant], intersections: mutable.HashMap[(MPath, MPath), (Theory, Theory)], renamings: mutable.HashMap[GlobalName, GlobalName]): List[Theory] = {
    val includes1 = collect_structures(th1)
    val rem = Theory.empty(th1.parent, LocalName(th1.name.toString+"'"), th1.meta) // T'

    //Recurse through dependent Theories of th1
    val rem_list = includes1.flatMap(_ match {
      case Include(_, from, args) =>
        remainder1(controller.getTheory(from), th2, view_map, view_map_inverse, intersections, renamings)
      case SimpleDeclaredStructure(home, name, from, isImplicit) =>
        remainder1(controller.getTheory(from), th2, view_map, view_map_inverse, intersections, renamings)
      case default => ???
    })
    //Fill remainder with remaining constants
    fillConstantsRemainder(rem, th1, th2, view_map.keySet, intersections.get((th1.path, th2.path)).get._1, renamings)
    return rem :: rem_list
  }

  /** Recursively create remainders of the right side theories of the intersection
    *
    * @param th1 left side theory
    * @param th2 right side theroy
    * @param view_map
    * @param view_map_inverse
    * @param intersections
    * @param renamings
    */
  protected def remainder2(th1: Theory, th2: Theory, view_map: collection.immutable.Map[FinalConstant, FinalConstant], view_map_inverse: collection.immutable.Map[FinalConstant, FinalConstant], intersections: mutable.HashMap[(MPath, MPath), (Theory, Theory)], renamings : mutable.HashMap[GlobalName, GlobalName]): List[Theory] = {
    val includes2 = collect_structures(th2)
    val rem = Theory.empty(th2.parent, LocalName(th2.name.toString+"'"), th2.meta) // T'

    //Recurse through dependent Theories of th2
    val rem_list = includes2.flatMap(_ match {
      case Include(_, from, args) =>
        remainder2(th1, controller.getTheory(from), view_map, view_map_inverse, intersections, renamings)
      case SimpleDeclaredStructure(home, name, from, isImplicit) =>
        remainder1(controller.getTheory(from), th2, view_map, view_map_inverse, intersections, renamings)
      case default => ???
    })
    //Fill remainder with remaining constants
    fillConstantsRemainder(rem, th2, th1, view_map_inverse.keySet, intersections.get((th1.path, th2.path)).get._2, renamings)
    rem :: rem_list
  }

  /** Move declaration to another theory
    *
    * Moves a declaration to a different theory, substituting all global names with the renaming
    *
    * @param dec declaration to be moved
    * @param th new home theory
    * @param renamings
    * @return
    */
  protected def addDeclaration(dec : Declaration, th : Theory, renamings : mutable.HashMap[GlobalName, GlobalName]) = {
    val renamer = Renamer(name => renamings.get(name))
    val translator = TraversingTranslator(renamer)

    val renamed = dec.translate(th.toTerm, LocalName.empty,translator, Context())
    th.add(renamed)
    renamings.put(dec.path, renamed.path)
    renamed
  }

  /** As above, but for collections
    *
    * @param decs collection of declarations
    * @param th target theory
    * @param renamings
    * @return
    */
  protected def addDeclaration(decs : Iterable[Declaration], th : Theory, renamings : mutable.HashMap[GlobalName, GlobalName]) : List[Declaration] = {
    decs.map(addDeclaration(_, th, renamings)).toList
  }

  /**
    *
    * @param theory
    * @return
    */
  protected def collect_structures(theory : Theory) = theory.getDeclarations.filter(_ match {
    case s : Structure => true
    case default => false
  })
}

object ViewSplitter {

  def apply(v : View) ( implicit controller : Controller) : List[(FinalConstant,FinalConstant)] = {
    getPairs(v, controller.getTheory(v.from.toMPath), controller.getTheory(v.to.toMPath))
  }

  /**
    * Splits a view up in Pairs of FinalConstants ; takes only those, that are directly elements
    * of (dom x cod)
    * @param v
    * @param dom
    * @param cod
    * @return
    */

  def getPairs(v:View, dom:Theory, cod:Theory) : List[(FinalConstant,FinalConstant)]= {
    val domconsts = v.getDeclarations.filter(_.name.head match {case ComplexStep(p) => p==dom.path}) collect {
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
        case o : FinalConstant => {
          o
        }
      }
      case submoduleName::tail => {
        val submodule = th.get(LocalName(submoduleName)).asInstanceOf[ModuleOrLink]
        getConstSub(submodule, tail)
      }
      case default => ???
    }
  }

  def getConstSub(m : ModuleOrLink, name : LocalName): FinalConstant = {
    m match {
      case th : Theory => getConst(th, name)
      case st: Structure => {
        println("namest:"+ComplexStep(st.from.toMPath)/name)
        st.get(ComplexStep(st.from.toMPath)/name) match {case c : FinalConstant => c}
      }
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
    for(i <- 0 to all.length-2 ; j <- i+1 to all.length-1) if (all(i)._1==all(j)._1 || all(i)._2==all(j)._2) return (Nil,Nil,Nil)
    (all collect {case (c:FinalConstant,d:FinalConstant) => (c,d)},
      all collect {case (c:FinalConstant,t:Term) => (c,t)},
      all collect {case (t:Term, c:FinalConstant) => (t,c)})
  }
}

