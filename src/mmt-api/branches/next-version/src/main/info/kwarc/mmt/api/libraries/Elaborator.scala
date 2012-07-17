package info.kwarc.mmt.api.libraries
import info.kwarc.mmt.api._
import frontend._
import modules._
import symbols._
import patterns._
import objects._
import utils.MyList.fromList
import collection.immutable.{HashSet, HashMap}

//import objects.Conversions._

/** an Elaborator takes a StructuralElement and produces further StructuralElement that are the result of elaborating the former */
abstract class Elaborator {
  /** @param e the StructuralElement that is elaborated
   * @param cont a function that is applied to each produced StructuralElement */

  def apply(e: StructuralElement)(implicit cont: StructuralElement => Unit) : Unit

  protected def rewrite(t : Term)(implicit rules : HashMap[Path,Term]) : Term = t match {
    case OMID(path) => rules.get(path) match {
      case None => t
      case Some(tm) => tm
    }
    case OMA(f, args) => OMA(rewrite(f), args.map(rewrite))
    case OMBINDC(b, context, condition, body) =>
      val nwctx = Context(context.variables.map(v =>
        VarDecl(v.name, v.tp.map(rewrite), v.df.map(rewrite), v.attrs.map(p => (p._1, rewrite(p._2))) :_*)
      ) :_ *)
      OMBINDC(rewrite(b), nwctx, condition.map(rewrite), rewrite(body))
    case OMM(arg,via) => OMM(rewrite(arg), rewrite(via))
    case OME(err, args) => OME(rewrite(err), args.map(rewrite))
    case OMATTR(arg, key, value) => OMATTR(rewrite(arg), key, rewrite(value)) //TODO maybe handle key (here) & uri (below)
    case OMREF(uri, value, under) => OMREF(uri, value.map(rewrite), Substitution(under.subs.map(s => Sub(s.name, rewrite(s.target))) :_*))
    case _ => t
  }

}


/**
 * Elaborates modules by enriching with induced statements
 */
class ModuleElaborator(controller : Controller) extends Elaborator {
  private val content = controller.globalLookup
  private val modules = controller.globalLookup.getAllPaths().map(controller.globalLookup.get(_)).toList

  var totalImports : Int = 0
  var thys : Int = 0
  var decls : Int = 0


  def printStatistics = {
    println("totalImports : " + totalImports)
    println("thys : " + thys)
    println("decls : " + decls)
  }


  private def etaReduce(t : Term) : Term = {  //TODO check context
    t match {
      case OMBIND(binder, context, OMA(fun, args)) =>
        val expectedArgs = context.components.map(vd => OMV(vd.name))
        if (expectedArgs == args) {
          fun
        } else {
          t
        }
      case OMBIND(binder, context, OMBIND(binder2, context2, OMA(fun, args))) =>
        val expectedArgs = context.components.map(vd => OMV(vd.name)) ::: context2.components.map(vd => OMV(vd.name))
        if (expectedArgs == args) {
          fun
        } else {
          t
        }
      case OMBIND(binder, context, OMBIND(binder2, context2, OMBIND(binder3, context3, OMA(fun, args)))) =>
        val expectedArgs = context.components.map(vd => OMV(vd.name)) ::: context2.components.map(vd => OMV(vd.name))  ::: context3.components.map(vd => OMV(vd.name))
        if (expectedArgs == args) {
          fun
        } else {
          t
        }
      case _ => t
    }
  }

  def getIncludes(t : Term) : HashSet[MPath] = {
    var includes : HashSet[MPath] = new HashSet[MPath]()
    content.get(t.toMPath).components collect {
      case s : DeclaredStructure =>
      if (s.name.isAnonymous)
        includes += s.from.toMPath
    }

    includes
  }

  private def importsTo(t : DeclaredTheory) : List[DeclaredStructure] = {
    var imports : List[DeclaredStructure] = Nil

    var includes : HashSet[MPath] = new HashSet[MPath]()

    t.components collect {
      case s : DeclaredStructure =>
      if (s.name.isAnonymous) {
        includes += s.from.toMPath
        includes ++= getIncludes(s.from)

      } else {
        imports ::= s
      }
    }

    includes.map(p => Include(OMMOD(t.path), OMMOD(p))).toList ::: imports

  }

  /**
   * @param e the ContentElement to elaborate
   * @param cont a continuation function to call on every generated StructuralElement (e.g., Controller.add)
   */
  def apply(e: StructuralElement)(implicit cont: StructuralElement => Unit) : Unit = e match {
    case t : DeclaredTheory =>
      val elabImports : List[DeclaredStructure] =  importsTo(t)

      totalImports += elabImports.length
      thys += 1



      var newDecs = new HashSet[Constant]()

      var rewriteRules = new HashMap[Path,Term]



      //s.home == t.path

      elabImports map {s =>
        s.from match {
          case OMMOD(p) =>
            if (s.domain.isEmpty) { //import is an include
            val impThy = content.get(p)
              impThy.components collect  {
                case c : Constant =>
                  val nwName = new LocalName(NamedStep(c.home.toMPath.toPath) :: Nil) / (c.name)
                  val nwHome = OMMOD(t.path)

                  rewriteRules += (c.home.toMPath ? c.name -> OMID(nwHome.toMPath ? nwName))

                  newDecs += c
              }
            } else { // import is a struct defined by a link
              s.domain map {x =>
                val ass = s.get(x)
                ass match {
                  case conAss : ConstantAssignment =>
                    val genCons = new Constant(conAss.home, conAss.name, None, Some(conAss.target), None, None)
                    newDecs += genCons
                  case _ => None
                }
              }
            }

          case _ =>
            None
        }
      }



      val nt = new DeclaredTheory(t.parent, t.name, t.meta)

      newDecs foreach {
        case c : Constant =>
          val nwName = new LocalName(NamedStep(c.home.toMPath.toPath) :: Nil) / (c.name)
          val nwHome = OMMOD(t.path)

          val ntp = c.tp.map(rewrite(_)(rewriteRules))
          val ndf = c.tp.map(rewrite(_)(rewriteRules))

          val nc = new Constant(nwHome, nwName, ntp, ndf, c.rl, c.not)
          nt.add(nc)
          decls += 1
        case _ => nt.add(_)
      }

      t.components collect {
        case c : Constant =>
          newDecs += c
          nt.add(c)
          decls += 1
      }

      cont(nt)

      modules collect {
        case v : DeclaredView =>
          v.from match {
            case OMMOD(p) =>
              var viewRewrRules = new HashMap[Path,Term]

              v.components collect {
                case ca : ConstantAssignment =>
                  println((p ? ca.name).toString + " #->#" + etaReduce(ca.target).toString)
                  viewRewrRules += (p ? ca.name -> etaReduce(ca.target))
              }


              if (p == t.path) {    // view from this theory
              val nwIndThy = new DeclaredTheory(v.to.toMPath.parent, LocalPath(List(v.to.toMPath.name.last + "^" +  escape(v.path.toPath) + "^" + escape(t.path.toPath))), t.meta)
                newDecs foreach { c =>
                  val nc = new Constant(c.home, c.name, c.tp.map(rewrite(_)(viewRewrRules)), c.df.map(rewrite(_)(viewRewrRules)), c.rl, c.not)
                  nwIndThy.add(nc)
                  decls += 1
                }
                cont(nwIndThy)
              }  else {
                elabImports foreach {i =>
                  i.from match {
                    case OMMOD(path) =>
                      if (p == path) {    // view from some imported theory

                      val nwIndThy = new DeclaredTheory(t.parent, LocalPath(List(v.to.toMPath.name.last + "^" +  escape(v.path.toPath) + "^" + escape(t.path.toPath))), t.meta)
                        newDecs foreach { c =>
                          val nc = new Constant(c.home, c.name, c.tp.map(rewrite(_)(viewRewrRules)), c.df.map(rewrite(_)(viewRewrRules)), c.rl, c.not)

                          nwIndThy.add(nc)
                          decls += 1
                        }
                        cont(nwIndThy)
                      }
                  }
                }
              }
          }
      }

    case v : DeclaredView => cont(v)
    case _ => None

  }

  def escape(s : String) = {
    s.replace("/","|").replace("?","!")
  }
}

