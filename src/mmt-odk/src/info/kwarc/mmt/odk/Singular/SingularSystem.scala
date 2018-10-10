package info.kwarc.mmt.odk.Singular

import info.kwarc.mmt.MiTM.MiTMSystems
import info.kwarc.mmt.MiTM.VRESystem.VREWithAlignmentAndSCSCP
import info.kwarc.mmt.odk.OpenMath.OMSymbol

class SingularSystem extends VREWithAlignmentAndSCSCP("Singular",MiTMSystems.singularsym,OMSymbol("MitM_Evaluate", "scscp_transient_1", None, None), "ODK/Singular")
