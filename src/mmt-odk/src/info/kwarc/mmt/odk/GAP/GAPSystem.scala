package info.kwarc.mmt.odk.GAP

import info.kwarc.mmt.MiTM.MiTMSystems
import info.kwarc.mmt.MiTM.VRESystem.VREWithAlignmentAndSCSCP
import info.kwarc.mmt.odk.OpenMath.OMSymbol

class GAPSystem extends VREWithAlignmentAndSCSCP("GAP",MiTMSystems.gapsym,OMSymbol("MitM_Evaluate", "scscp_transient_1", None, None), "ODK/GAP")
