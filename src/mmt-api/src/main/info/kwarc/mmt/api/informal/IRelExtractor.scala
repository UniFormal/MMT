package info.kwarc.mmt.api.informal

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.ontology.rdf.{Database, ULO}
import ontology._
import modules._
import symbols._
import objects._

object IRels {
  private val ns = Database.mmt_uri / "informal"
  val isDefinedBy = CustomBinary("isDefinedBy", "is defined by", "is definition for",ns)
  val isExemplifiedBy = CustomBinary("isExemplifiedBy", "is exemplified by", "is example for",ns)
  val isExercise = CustomUnary("exercise",ns)
  val isExample = CustomUnary("example",ns)
  val isDefinition = CustomUnary("definition",ns)

  val allBinary = List(isDefinedBy, isExemplifiedBy)
  val allUnary = List(isExercise, isDefinition, isExample)
}

class IRelExtractor extends RelationalExtractor{
  /** all unary relations that this extractor can generate (extract) */
  def allUnary : List[Unary] = IRels.allUnary

  /** all binary relations that this extractor can generate (extract) */
  def allBinary : List[Binary] = IRels.allBinary

  /** apply a continuation function to every relational element of a StructuralElement */
  def apply(e : StructuralElement)(implicit f: RelationalElement => Unit) : Unit = e match {
    case d : Module => d.getDeclarations foreach {
      case c : Constant =>
        c.metadata.getValues(Informal.constant("role")) foreach {
          case OMA(OMS(p), args) if p == Informal.constant("defines") =>
            f(Individual(c.path, IRels.isDefinition))
            args foreach {
              case OMS(d) => f(Relation(IRels.isDefinedBy, d, c.path))
              case o => println(o)//nothing to do
            }
          case OMA(OMS(p), args) if p == Informal.constant("example") =>
            f(Individual(c.path, IRels.isExample))
            args foreach {
              case OMS(d) => f(Relation(IRels.isExemplifiedBy, d, c.path))
              case o => println(o)//nothing to do
            }
          case OMS(p) if p == Informal.constant("exercise") =>
            f(Individual(c.path, IRels.isExercise))
          case m => //nothing to do
        }
      case _ => //nothing to do
    }
    case _ => //nothing to do
  }
}
