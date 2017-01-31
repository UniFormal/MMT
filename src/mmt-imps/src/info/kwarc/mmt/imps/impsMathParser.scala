package info.kwarc.mmt.imps

import info.kwarc.mmt.api.GeneralError
import info.kwarc.mmt.api.utils.Unparsed

package object impsMathParser
{
  def parseIMPSMath(s : String) : Option[IMPSMathExp] =
  {
    if (s.startsWith("lambda("))
    {
      parseIMPSLambda(s)
    }
    else { None }
  }
  //parseIMPSMath(new Unparsed(s, msg => throw GeneralError(msg)))

  private def parseIMPSMath(in : Unparsed) : Option[IMPSMathExp] =
  {
    None
  }

  private def parseIMPSLambda(s: String) : Option[IMPSMathExp] =
  {
    var str : String = ""
    if ((s.startsWith("lambda(", 0)) && (s.endsWith(")")))
    {
      str = s.drop("lambda(".length) // drop "lambda("
      str = str.dropRight(1)         // drop ")"

      val arr : Array[String] = str.split(',')
      val p : String = arr.last

      var vars : List[(IMPSMathExp, Option[IMPSMathExp])] = List.empty

      for (pp <- arr.init)
      {
        if (pp.contains(':'))
        {
          val var_and_sort : Array[String] = pp.split(':')
          vars = vars ::: List((IMPSVar(var_and_sort(0)), Some(IMPSSort(var_and_sort(1)))))
        }
        else
        {
          vars = vars ::: List((IMPSVar(pp), None))
        }
      }

      val t_opt : Option[IMPSMathExp] = parseIMPSMath(p)

      if (t_opt.isDefined)
      {
        Some(IMPSLambda(vars, t_opt.get))
      } else { None }
    } else { None }
  }
}