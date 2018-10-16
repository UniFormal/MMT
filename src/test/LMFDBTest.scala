import info.kwarc.mmt.api.{LocalName, Path}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.odk.NatLiterals

object LMFDBTest extends MagicTest("lmfdb", "mitm", "scscp") {
  def run : Unit = {
    hl("extension info.kwarc.mmt.odk.LMFDB.Plugin")

    // shortcut to swaerxh
    def s(s: String): OMID = OMID(Path.parseMS(s, controller.getNamespaceMap))

    val degree = NatLiterals(3) // or whatever


    // a *term* representing a query that finds a hecke form of degree 3
    val queryTerm = QMTQuery.I(
      OML(LocalName("lmfdb"), None, None),
      OMBINDC(
        OMID(QMTQuery.Comprehension),
        Context(VarDecl(LocalName("x"), None, None, None, None)),
        List(
          QMTQuery.Related(
            QMTQuery.Literal(s("http://www.lmfdb.org/db?hmf_forms")),
            QMTRelationExp.ToObject(OMID(QMTBinaries.Declares))
          ),
          QMTProp.Holds(
            OMV(LocalName("x")),
            OMBINDC(
              OMID(QMTJudgements.Equals),
              Context(VarDecl(LocalName("x"),None, None, None, None)),
              List(s("http://mathhub.info/MitM/smglom/algebra?HilbertNewforms?base_field_degree")(OMV(LocalName("x"))), degree)
            )
          ))
      )
    )

    val query = Query.parse(queryTerm)(controller.extman.get(classOf[QueryFunctionExtension]), controller.relman)
    print(query)

    val evaluated = controller.evaluator(query)
    print(evaluated)



    // load the (default) configuration
    //hl("mitm use")

    // load a non-default configuration
    // see mmt-api/resources/mitm/config.default.json for an example
    //hl("mitm use /path/to/config.json")

    // turn on scscp on localhost:26134
   //hl("scscp on 26134")
  }
}
