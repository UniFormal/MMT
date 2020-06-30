package info.kwarc.mmt.frameit

import info.kwarc.mmt.api.{DPath, GlobalName, LocalName, MPath, Path}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects.OMS
import info.kwarc.mmt.api.ontology.{IsConstant, IsTheory}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, FinalConstant}
import info.kwarc.mmt.api.uom.{RealizedType, StandardDouble}
import info.kwarc.mmt.api.utils.URI

object Dictionary {
  val typeURI =  Path.parse("http://cds.omdoc.org/urtheories?Typed?type")
  val factCollection = MPath(DPath(URI("http://BenniDoes.Stuff")), LocalName.parse("FactCollection"))
  val sketchURI = Path.parse("http://mathhub.info/MitM/Foundation?InformalProofs?proofsketch")
  val jderURI = Path.parse("http://mathhub.info/MitM/Foundation?Logic?ded")
  //val jderURI = DPath(URI.http colon("mathhub.info/MitM/Foundation")) ? "Logic" ?"ded"
  val jdoteqURI = Path.parse("http://mathhub.info/MitM/Foundation?Logic?eq")
  //val jdoteqURI = DPath(URI.http colon("mathhub.info/MitM/Foundation")) ? "Logic" ?"eq"
}
