package info.kwarc.mmt.api.informal

import info.kwarc.mmt.api._
import ontology._
import symbols._

object IRels {
  val isDefinedBy = CustomBinary("isDefinedBy", "is defined by", "is definition for")
  
  val allBinary = List(isDefinedBy)
  val allUnary = Nil
}

class IRelExtractor extends RelationalExtractor{
  /** all unary relations that this extractor can generate (extract) */
  def allUnary : List[Unary] = IRels.allUnary
  
  /** all binary relations that this extractor can generate (extract) */
  def allBinary : List[Binary] = IRels.allBinary
  
  /** apply a continuation function to every relational element of a StructuralElement */
  def apply(e : StructuralElement)(implicit f: RelationalElement => Unit) : Unit = e match {
    case c : Constant => //TODO extract isDefined relations (from metadata most likely)
    case _ => //nothing to do
  }
}