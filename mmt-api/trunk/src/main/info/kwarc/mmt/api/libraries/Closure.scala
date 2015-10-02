package info.kwarc.mmt.api.libraries
import info.kwarc.mmt.api._
import frontend._
import info.kwarc.mmt.api.objects.{ComplexTheory, OMDL, OMPMOD}
import info.kwarc.mmt.api.refactoring.{Moduleadder, Consthash}
import modules._
import symbols._
import patterns._

import scala.util.{Success, Try}

class Closer(controller: Controller) {
  private val lup = controller.globalLookup

  /**
   * recursive loads all theories included into p
   */
  def apply(p: MPath) {
    val d = lup.getO(p)
    d match {
      case Some(d: DeclaredTheory) =>
        //TODO keep track of which
        //d.status = Elaborated
        //d.getIncludes foreach apply
        getIncludes(d, true)
        d.getNamedStructures foreach (s => apply(s.from.toMPath))
        //d.getNamedStructures foreach flatten(d)
      case _ =>
    }
  }

  def flatten(th: DeclaredTheory)(s: Structure): Unit = {
    if (!s.hasBeenElaborated) {
      val dom: List[LocalName] = try {
        lup.getO(s.from.toMPath).getOrElse(Nil) match {
          case t: DeclaredTheory =>
            flattenInclude(t)
          case _ => Nil
        }
      } catch {
        case e: Exception => Nil
      }
      dom.foreach { name =>
        val d = lup.get(s.path / name)
        d.setOrigin(FromStructure(s.path))
        d match {
          //case d:Declaration => th.add(d)
          case c: FinalConstant => th.add(c)
          case _ => {}
        }

      }
      // s.setOrigin(HasBeenElaborated)
    }
  }

    def flattenInclude(th: DeclaredTheory): List[LocalName] = {
      // TODO Replace this by the actual thing
      val incls = getIncludes(th, false)
      incls.flatMap(t => t.getConstants.map(c =>
        //c.name
        LocalName((ComplexStep(t.path) / c.name).steps)
      )).toList
    }

    def getStructureDecls(s: DeclaredStructure) = {
      if (!s.isImplicit) try {
        controller.globalLookup.getO(s.from.toMPath).getOrElse(Nil) match {
          case t: DeclaredTheory =>
            val x = flattenInclude(t)
            val y = flattenInclude(t).map(name => controller.localLookup.get(s.path / name))
            y
          case _ => Nil
        }
      } catch {
        case e: Exception => Nil
      }
      else List(s)
    }

    def getIncludes(th: DeclaredTheory, withmeta: Boolean): Set[DeclaredTheory] = {
      def getIncludesIt(th2: DeclaredTheory, donep: Set[MPath], donet: Set[DeclaredTheory]): (Set[MPath], Set[DeclaredTheory]) = {
        (if (withmeta) th2.getIncludes else th2.getIncludesWithoutMeta).foldLeft((donep + th2.path, donet + th2))((sets, path) =>
          if (sets._1 contains path) sets
          else lup.getO(path) match {
            case Some(x: DeclaredTheory) => getIncludesIt(x, sets._1, sets._2)
            case _ => sets
          }
        )
      }
      getIncludesIt(th, Set(), Set())._2
    }
  }