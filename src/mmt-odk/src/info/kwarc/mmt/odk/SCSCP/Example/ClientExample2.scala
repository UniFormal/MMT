package info.kwarc.mmt.odk.SCSCP.Example

import info.kwarc.mmt.api.NamespaceMap

object ClientExample2 {
  def main(args: Array[String]): Unit = {
    /*
    // prepare a computation for GAP
    // here we compute the nr of conjugacy classes
    import info.kwarc.mmt.odk.OpenMath._
    val NrConjugacyClasses = OMSymbol("TransitiveGroup", "scscp_transient_1", None, None)
    val computation = OMApplication(NrConjugacyClasses, List(om_term), None, None)

    // fetch the resulting expression from GAP
    import info.kwarc.mmt.odk.SCSCP.Client.SCSCPClient
    val client = SCSCPClient("scscp.gap-system.org")
    val om_result = client(computation).fetchExpression()
    client.quit()

    /**
      * OMInteger(92,None)
      */
    println(om_result)

    // and turn the result back into an MMT term
    val mmt_result = GAPEncoding.encode(om_result)

    /**
      * 92
      */
    println(mmt_result)
    */
  }
}
