import info.kwarc.mmt.api.uom._

object EnumerateTest {
  def enumerateSemanticType(st: SemanticType, m: Int) {

    val it = st.enumerate(m).getOrElse {return}

    while (it.hasNext) {
      println(st.toString(it.next))
    }

  }

  def main(args: Array[String]) {

    /** SemanticTypes:
      * StandardInt
      * StandardNat
      * StandardPositive
      * StandardRat
      * ComplexRat
      * StandardBool
      * ListType(SemanticType)
      * TupleType(SemanticType, dimension)
      * IntModulo (class)
      * Product (class)
      * StandardString
      * Not Working: IntModulo*/

    //Any randomization and quick count should use a mode selection (numbers only)
    //StandardBool only has 2 values, mode selection should have no influence
    //modes for numbers: 0 = random, else the number determines increment
    enumerateSemanticType(StandardNat, 5)

  }
}