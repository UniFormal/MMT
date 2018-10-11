package info.kwarc.mmt.odk.Singular

import info.kwarc.mmt.MitM.MitMSystems
import info.kwarc.mmt.MitM.VRESystem.VREWithAlignmentAndSCSCP
import info.kwarc.mmt.odk.OpenMath.OMSymbol

class SingularSystem extends VREWithAlignmentAndSCSCP("Singular",MitMSystems.singularsym,OMSymbol("MitM_Evaluate", "scscp_transient_1", None, None), "ODK/Singular")
