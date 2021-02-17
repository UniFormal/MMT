package info.kwarc.mmt.stex
/*
import info.kwarc.mmt.api._
import symbols._
import modules._
import documents._
import ontology._
import objects._
import uom.OMLiteral.OMSTR


object IsPrimarySymbol extends CustomUnary("primarySymbol")
object IsMathStructure extends CustomUnary("mathStructure")

object IsHypernymOf extends CustomBinary("hypernymOf", "is hypernym of", "is hyponym of")
//extension info.kwarc.mmt.stex.sTeXExtractor
class sTeXExtractor extends RelationalExtractor {
  def allUnary : List[Unary] = List(IsPrimarySymbol,IsMathStructure)

  def allBinary : List[Binary] = List(IsHypernymOf)

  def apply(e : StructuralElement)(implicit f: RelationalElement => Unit) = {
    e match {
    case m : Theory =>
      var foundPrimarySymbol = false
      m.getDeclarations collect {
      case c : Constant =>
      if (c.metadata.get(sTeXMetaData.rolePath).map(_.value).contains(OMSTR("primary"))) {
          foundPrimarySymbol = true
          f(IsPrimarySymbol(c.path))
        }
      }
      if (foundPrimarySymbol) {
        f(IsMathStructure(m.path))
        m.getDeclarations collect {
          case i@PlainInclude(from,_) =>
            if (!i.metadata.get(sTeXMetaData.rolePath).map(_.value).contains(OMSTR("conservative-extension"))) {
              f(IsHypernymOf(m.path, from))
            }
        }}
    case _ => //do nothing
  }}
}

 */
