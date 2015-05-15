package info.kwarc.mmt.api.informal

import info.kwarc.mmt.api._
import ontology._
import modules._
import symbols._
import objects._

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
    case d : DeclaredModule => d.getDeclarations foreach {
      case c : Constant => 
        c.metadata.getValues(Informal.constant("role")) foreach {
          case OMA(OMS(p), args) if p == Informal.constant("defines") => args foreach {
            case OMS(d) => f(Relation(IRels.isDefinedBy, d, c.path))
            case o => println(o)//nothing to do
          }
          case m => println(m)//nothing to do
        }
      case _ => //nothing to do  
    }
    case _ => //nothing to do
  }
}