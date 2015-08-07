package info.kwarc.mmt.pvs

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.objects.{Term, OMID, OMA}
import info.kwarc.mmt.api.symbols.{Constant, DeclaredStructure}
import utils._

object PVSTheory {
   val dpath = DPath(URI.http colon "pvs.csl.sri.com")
   val name = "PVS"
   val path = dpath ? name
   def sym(s: String) = path ? s
   val fun = sym("fun")
   val tm = sym("tm")
   val prod = sym("prod")
   val tuple = sym("tuple")
   val predsub = sym("predsub")
   val union = sym("union")
   val asType = sym("asType")
   val expr = sym("expr")
   val tp = sym("tp")
   //val variable = sym("var")
   val formula = sym("formula")
   // val LFapply = DPath((URI.http colon "cds.omdoc.org") / "urtheories") ? "LF" ? "apply"
   val LFarrow = DPath((URI.http colon "cds.omdoc.org") / "urtheories") ? "LF" ? "arrow"
   // TODO: think about names
   def subtp(th:DeclaredTheory,name:String,of:Term) =
      DeclaredStructure(OMID(th.path), LocalName(name), sym("SubtypeDecl").apply(of), false)
   def ofTypeDecl(th:DeclaredTheory,name:String,of:Term) =
      DeclaredStructure(OMID(th.path),LocalName(name),sym("ofTypeDecl").apply(of),false)
}