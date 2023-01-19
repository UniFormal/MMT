package info.kwarc.mmt.lean

import info.kwarc.mmt.api.utils

class LeanImporter extends {
   def apply {
     val exportedCommands = TextExportParser.parseFile("C:\\other\\systems\\lean\\mathlib_nat.out")
     val fw = utils.File.Writer(utils.File("C:\\other\\systems\\lean\\mathlib_nat.mmt"))
     val modifications = exportedCommands.collect { case ExportedModification(mod) =>
       mod
     }
     modifications.toList.foreach {
       case a: AxiomMod => fw.println("axiom: " + a.name)
       case d: DefMod => fw.println("definition: " + d.name + " : " + d.ty.toShort())
       case _ =>
     }
     val notations = Map() ++ exportedCommands.
       collect { case ExportedNotation(not) => not.fn -> not }.
       reverse // the beautiful unicode notation is exported first
     // Modification = Declaration
     // CompiledModification = elaboration into .decls and .rules and the check tasks called by .check
     // import modifications: AxiomMod, DefMod directly, the others (inductive and quotient) by compiling

   }

   def translateExpr(e: Expr) = {
     e match {
       case Lam(dom, bod) =>
     }
   }
}
