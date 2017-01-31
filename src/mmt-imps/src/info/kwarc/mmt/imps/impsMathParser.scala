package info.kwarc.mmt.imps

import info.kwarc.mmt.api.GeneralError
import info.kwarc.mmt.api.utils.Unparsed

package object impsMathParser
{
  def parseIMPSMath(s : String) : Option[IMPSMathExpr] =
  {
    if (s.startsWith("lambda("))
    {
      parseIMPSLambda(s)
    }
    else { None }
  }
  //parseIMPSMath(new Unparsed(s, msg => throw GeneralError(msg)))

  private def parseIMPSMath(in : Unparsed) : Option[IMPSMathExpr] =
  {
    None
  }

  private def parseIMPSLambda(s: String) : Option[IMPSMathExpr] =
  {
    var str : String = ""
    if ((s.startsWith("lambda(", 0)) && (s.endsWith(")")))
    {
      str = s.drop(7)         // drop "lambda("
      str = str.dropRight(1)  // drop ")"

      val arr : Array[String] = str.split(',')
      val p : String = arr.last

      for (pp <- arr.init)
      {

      }
      Some(IMPSLambda())
    } else { None }
  }
}