package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api.utils._

sealed abstract class Modification

case object Added extends Modification

case object Deleted extends Modification

case object Modified extends Modification

case object Unmodified extends Modification

object Modification {
  /**
   * @param input the file to be compiled/built
   * @param result the output file of the previous compile
   * @return status of input file, obtained by comparing to result file
   */
  def apply(input: File, result: File): Modification = {
    if (!result.exists) Added
    else if (!input.exists) Deleted
    else {
      val lastRun = result.lastModified
      val lastChanged = input.lastModified
      if (lastRun < lastChanged) Modified
      else Unmodified
    }
  }
}
