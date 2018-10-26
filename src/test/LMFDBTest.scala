import info.kwarc.mmt.MitM.VRESystem.{MitMComputation, MitMComputationTrace}
import info.kwarc.mmt.api.objects._

import info.kwarc.mmt.odk.OpenMath.Coding.{OMMiTMCoding, OMXMLCoding}

object LMFDBTest extends MagicTest("lmfdb", "mitm", "scscp", "impl-rule-gen", "debug") {
  override val gotoshell: Boolean = false
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

    // create a mitm computation and controller
    implicit val trace: MitMComputationTrace = new MitMComputationTrace(false)
    val mitmComp = new MitMComputation(controller)

    // and run it
    val result = mitmComp.simplify(term, None)
    print("result: " + result)

    sys.exit(9)
  }
}
