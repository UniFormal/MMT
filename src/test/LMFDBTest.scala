import Graphtester.controller
import MitMTest.controller
import info.kwarc.mmt.MitM.{MitM, MitMSystems}
import info.kwarc.mmt.MitM.VRESystem.{MitMComputation, MitMComputationTrace}
import info.kwarc.mmt.api.{LocalName, utils}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.web.JSONBasedGraphServer
import info.kwarc.mmt.odk.LFX.LFList
import info.kwarc.mmt.odk.LMFDB.LMFDB
import info.kwarc.mmt.odk.OpenMath.Coding.{OMMiTMCoding, OMXMLCoding}
import info.kwarc.mmt.odk.{IntegerLiterals, LFX, StringLiterals}

// "impl-rule-gen"
object LMFDBTest extends MagicTest("lmfdb", "mitm", "scscp", "debug") {
  override val gotoshell: Boolean = false

  override def doFirst: Unit = {
    super.doFirst()
    // Copied here because these lines were removed from MagicTest.
    // Please reevaluate if they are necessary. If in doubt, leave them. They are just slow.)
    controller.handleLine("extension info.kwarc.mmt.pvs.PVSImporter")
    controller.handleLine(("extension info.kwarc.mmt.api.ontology.AlignmentsServer " + alignmentspath).trim)
    controller.extman.addExtension(new DependencyGraphExporter)
    controller.extman.addExtension(new DeclarationTreeExporter)
    controller.extman.addExtension(new JSONBasedGraphServer)
    controller.extman.addExtension(new PathGraphExporter)
  }

  def run : Unit = {
    hl("extension info.kwarc.mmt.odk.LMFDB.Plugin")

    // the query from sage (xml, but a string)
    val sageQuery: String = """<OMA xmlns="http://www.openmath.org/OpenMath"><OMS cdbase="http://gl.mathhub.info/MMT/LFX/Datatypes" name="map" cd="ListSymbols"/><OMA><OMS cdbase="http://opendreamkit.org" name="ODKQuery" cd="Systems"/><OMA><OMS cdbase="http://cds.omdoc.org/qmt" name="I" cd="QMTQuery"/><OMSTR>lmfdb</OMSTR><OMA><OMS cdbase="http://cds.omdoc.org/qmt" name="SliceUntil" cd="QMTQuery"/><OMSTR>10</OMSTR><OMBIND><OMS cdbase="http://cds.omdoc.org/qmt" name="Comprehension" cd="QMTQuery"/><OMBVAR><OMV name="x"/></OMBVAR><OMA><OMS cdbase="http://cds.omdoc.org/mmt" name="MultiBody" cd="OpenMath"/><OMA><OMS cdbase="http://cds.omdoc.org/qmt" name="Related" cd="QMTQuery"/><OMA><OMS cdbase="http://cds.omdoc.org/qmt" name="Literal" cd="QMTQuery"/><OMS cdbase="http://www.lmfdb.org/db" name="" cd="hmf_forms"/></OMA><OMA><OMS cdbase="http://cds.omdoc.org/qmt" name="ToObject" cd="QMTRelationExp"/><OMS cdbase="http://cds.omdoc.org/qmt" name="Declares" cd="QMTBinaries"/></OMA></OMA><OMA><OMS cdbase="http://cds.omdoc.org/qmt" name="And" cd="QMTProp"/><OMA><OMS cdbase="http://cds.omdoc.org/qmt" name="Holds" cd="QMTProp"/><OMV name="x"/><OMBIND><OMS cdbase="http://cds.omdoc.org/qmt" name="Equals" cd="QMTJudgements"/><OMBVAR><OMV name="x"/></OMBVAR><OMA><OMS cdbase="http://cds.omdoc.org/mmt" name="MultiBody" cd="OpenMath"/><OMA><OMS cdbase="http://mathhub.info/MitM/smglom/algebra" name="base_field_degree" cd="HilbertNewforms"/><OMV name="x"/></OMA><OMI>2</OMI></OMA></OMBIND></OMA><OMA><OMS cdbase="http://cds.omdoc.org/qmt" name="Holds" cd="QMTProp"/><OMV name="x"/><OMBIND><OMS cdbase="http://cds.omdoc.org/qmt" name="Equals" cd="QMTJudgements"/><OMBVAR><OMV name="x"/></OMBVAR><OMA><OMS cdbase="http://cds.omdoc.org/mmt" name="MultiBody" cd="OpenMath"/><OMA><OMS cdbase="http://mathhub.info/MitM/smglom/algebra" name="dimension" cd="HilbertNewforms"/><OMV name="x"/></OMA><OMI>2</OMI></OMA></OMBIND></OMA></OMA></OMA></OMBIND></OMA></OMA></OMA><OMS cdbase="http://mathhub.info/MitM/smglom/algebra" name="base_field_degree" cd="HilbertNewforms"/></OMA>"""

    // decode the xml
    val xmlCoder = new OMXMLCoding
    val obj = xmlCoder.decode(scala.xml.XML.loadString(sageQuery))

    // and turn it into MMT objects
    val mitmCoder = new OMMiTMCoding(controller)
    val term = mitmCoder.encode(obj).asInstanceOf[OMA]

    // The same term constructed directly
    val toobj = OMA(OMS(QMTRelationExp.ToObject),OMS(QMTBinaries.Declares) :: Nil)
    val related = OMA(OMS(QMTQuery.Related),OMA(OMS(QMTQuery.Literal),OMMOD(LMFDB.dbPath ? "hmf_hecke"):: Nil) :: toobj :: Nil)

    val deg = OMA(OMS((MitM.basepath / "smglom" / "algebra") ? "HilbertNewforms" ? "base_field_degree"),OMV("x"):: Nil)
    val dim = OMA(OMS((MitM.basepath / "smglom" / "algebra") ? "HilbertNewforms" ? "dimension"),OMV("x"):: Nil)
    val holds1 = OMA(OMS(QMTProp.Holds),OMV("x") :: OMBINDC(OMS(QMTJudgements.Equals),VarDecl(LocalName("x")),deg :: IntegerLiterals(2) :: Nil) :: Nil)
    val holds2 = OMA(OMS(QMTProp.Holds),OMV("x") :: OMBINDC(OMS(QMTJudgements.Equals),VarDecl(LocalName("x")),dim :: IntegerLiterals(2) :: Nil) :: Nil)
    val and = OMA(OMS(QMTProp.And),holds1 :: holds2 :: Nil)

    val comp = OMBINDC(OMS(QMTQuery.Comprehension),VarDecl(LocalName("x")),related :: and :: Nil)
    val slice = OMA(OMS(QMTQuery.SliceUntil),StringLiterals("10") :: comp :: Nil)
    val query = OMA(OMS(MitMSystems.querysym),OMA(OMS(QMTQuery.I),StringLiterals("lmfdb") :: slice :: Nil) :: Nil)

    val term2 = LFX.Map(query,OMS((MitM.basepath / "smglom" / "algebra") ? "HeckeEigenvalues" ? "heckePolynomial"))

    // check that the constructed term is equal to the parsed one
    // assert(term == term2)

    // create a mitm computation and controller
    implicit val trace: MitMComputationTrace = new MitMComputationTrace(None)
    val mitmComp = new MitMComputation(controller)

    // and run it
    val result = mitmComp.simplify(term2, None)
    print("result: " + result)

    sys.exit(9)
  }
}
