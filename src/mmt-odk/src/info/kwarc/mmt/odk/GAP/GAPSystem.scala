package info.kwarc.mmt.odk.GAP

import info.kwarc.mmt.MitM.MitMSystems
import info.kwarc.mmt.MitM.VRESystem.VREWithAlignmentAndSCSCP
import info.kwarc.mmt.odk.OpenMath.OMSymbol

class GAPSystem extends VREWithAlignmentAndSCSCP("GAP",MitMSystems.gapsym,OMSymbol("MitM_Evaluate", "scscp_transient_1", None, None), "ODK/GAP")
