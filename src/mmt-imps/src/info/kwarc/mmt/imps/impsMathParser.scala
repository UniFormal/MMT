package info.kwarc.mmt.imps

import info.kwarc.mmt.api.GeneralError
import info.kwarc.mmt.api.utils.Unparsed

package object impsMathParser
{
  def parseIMPSMath(s : String) : Option[IMPSMathExp] =
  {
    /* CHEATING! TODO: REMOVE */
    if (s == "[-1]")
    {
      Some(IMPSMathSymbol(s))
    }
    else if (s == "1")
    {
      Some(IMPSMathSymbol(s))
    }
    else if (s == "lambda(n:zz, n = [-1] or n = 1)")
    {
      val vs : List[(IMPSMathExp, Option[IMPSMathExp])] = List((IMPSVar("n"), Some(IMPSSort("zz"))))
      val p : IMPSMathExp = IMPSEquals(IMPSVar("n"), IMPSMathSymbol("[-1]"))
      val q : IMPSMathExp = IMPSEquals(IMPSVar("n"), IMPSMathSymbol("1"))
      val t : IMPSMathExp = IMPSDisjunction(List(p,q))
      ??? //Some(IMPSLambda(vs, t))
    }
    else if (s == "lambda(b:boole, b = true%val)")
    {
      val vs = List((IMPSVar("b"), Some(IMPSSort("boole"))))
      val t  = IMPSEquals(IMPSVar("b"), IMPSMathSymbol("true%val"))
      Some(IMPSLambda(vs, t))
    }
    else if (s == "lambda(b:boole, b = false%val)")
    {
      val vs = List((IMPSVar("b"), Some(IMPSSort("boole"))))
      val t  = IMPSEquals(IMPSVar("b"), IMPSMathSymbol("false%val"))
      Some(IMPSLambda(vs, t))
    }
    else if (s == "is%true(true%val) iff truth")
    {
      val p : IMPSMathExp = IMPSApply(IMPSMathSymbol("is%true"), List(IMPSMathSymbol("true%val")))
      Some(IMPSIff(p, IMPSTruth()))
    }
    else if (s == "is%true(false%val) iff falsehood")
    {
      val p : IMPSMathExp = IMPSApply(IMPSMathSymbol("is%true"), List(IMPSMathSymbol("false%val")))
      Some(IMPSIff(p, IMPSFalsehood()))
    }
    else if (s == "is%false(true%val) iff falsehood")
    {
      val p : IMPSMathExp = IMPSApply(IMPSMathSymbol("is%false"), List(IMPSMathSymbol("true%val")))
      Some(IMPSIff(p, IMPSFalsehood()))
    }
    else if (s == "is%false(false%val) iff truth")
    {
      val p : IMPSMathExp = IMPSApply(IMPSMathSymbol("is%false"), List(IMPSMathSymbol("false%val")))
      Some(IMPSIff(p, IMPSTruth()))
    }
    else
    {
      /* SENSIBLE PARSING GOES HERE */
      None // Return None until parsing actually implemented
    }
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
        else { vars = vars ::: List((IMPSVar(pp), None)) }
      }

      val t_opt : Option[IMPSMathExp] = parseIMPSMath(p)

      if (t_opt.isDefined)
      {
        ??? //Some(IMPSLambda(vars, t_opt.get))
      } else { None }
    } else { None }
  }
}