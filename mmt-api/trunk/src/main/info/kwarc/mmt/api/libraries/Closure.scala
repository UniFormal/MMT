package info.kwarc.mmt.api.libraries
import info.kwarc.mmt.api._
import frontend._
import info.kwarc.mmt.api.refactoring.{Moduleadder, Consthash}
import modules._
import symbols._

//TODO @DM: remove this class
class Closer(controller: Controller) {
  private val lup = controller.globalLookup

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