/**
  * The "DIAG FROM FILE" operator returning the whole diagram for a given MMT filename.
  */

package info.kwarc.mmt.moduleexpressions.operators

import info.kwarc.mmt.api.{DPath, MPath}
import info.kwarc.mmt.api.modules.{Module, Theory}
import info.kwarc.mmt.api.checking.{CheckingCallback, ComputationRule, History}
import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.uom._
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lf.Strings

import scala.io.Source

object DiagramFromFile extends UnaryConstantScala(Combinators._path, "diagram_from_file")

/**
  * @todo The "DIAG FROM FILE" operator is implemented in a very hacky way currently as there is currently
  *       no other way to get all modules declared in a file from within a [[ComputationRule]].
  *       An API addition needs to be discussed.
  */
object ComputeDiagramFromFile extends ComputationRule(DiagramFromFile.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = tm match {
    case DiagramFromFile(Strings(file)) =>
      val modules = getModulesForFile(file, solver.lookup)
      Simplify(Common.asAnonymousDiagram(solver, modules).toTerm)

    case _ =>
      RecurseOnly(List(1))
  }

  private def getModulesForFile(file: String, lookup: Lookup): Set[Module] = {
    val bufferedSource = Source.fromFile(file)
    val surfaceSyntax = bufferedSource.mkString
    bufferedSource.close()

    val namespace = {
      raw"namespace\s+([^❚ ]+)".r.findFirstMatchIn(surfaceSyntax).get.group(1)
    }
    val theories = {
      val theoryNameRegex = raw"theory\s+([^ =]+)".r

      theoryNameRegex.findAllMatchIn(surfaceSyntax).map(_.group(1))
    }

    val modulePaths = theories.map(DPath(URI(namespace)) ? _)

    modulePaths.flatMap(lookup.getO(_).map(_.asInstanceOf[Module])).toSet
  }
}