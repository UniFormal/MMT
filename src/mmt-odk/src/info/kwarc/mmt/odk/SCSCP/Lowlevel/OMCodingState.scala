package info.kwarc.mmt.odk.SCSCP.Lowlevel

/** the OpenMath Coding State of an SCSCP instance */
sealed abstract class OMCodingState

case object AutoState extends OMCodingState

case object XMLState extends OMCodingState
case object JSONState extends OMCodingState