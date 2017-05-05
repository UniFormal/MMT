package info.kwarc.mmt.imps

import info.kwarc.mmt.api.GeneralError
import info.kwarc.mmt.api.utils.Unparsed

package object impsMathParser
{
  def parseIMPSMath(s : String) : Option[IMPSMathExp] =
  {
    val cheating = false

    if (cheating)
    {
      /* CHEATING! TODO: REMOVE */
      if (s == "\"[-1]\"")
      {
        Some(IMPSMathSymbol(s))
      }
      else if (s == "\"1\"")
      {
        Some(IMPSMathSymbol(s))
      }
      else if (s == "\"lambda(n:zz, n = [-1] or n = 1)\"")
      {
        val vs : List[(IMPSVar, Option[IMPSSortRef])] = List((IMPSVar("n"), Some(IMPSSortRef("zz"))))
        val p : IMPSMathExp = IMPSEquals(IMPSVar("n"), IMPSMathSymbol("[-1]"))
        val q : IMPSMathExp = IMPSEquals(IMPSVar("n"), IMPSMathSymbol("1"))
        val t : IMPSMathExp = IMPSDisjunction(List(p,q))
        Some(IMPSLambda(vs, t))
      }
      else if (s == "\"lambda(b:boole, b = true%val)\"")
      {
        val vs = List((IMPSVar("b"), Some(IMPSSortRef("boole"))))
        val t  = IMPSEquals(IMPSVar("b"), IMPSMathSymbol("true%val"))
        Some(IMPSLambda(vs, t))
      }
      else if (s == "\"lambda(b:boole, b = false%val)\"")
      {
        val vs = List((IMPSVar("b"), Some(IMPSSortRef("boole"))))
        val t  = IMPSEquals(IMPSVar("b"), IMPSMathSymbol("false%val"))
        Some(IMPSLambda(vs, t))
      }
      else if (s == "\"is%true(true%val) iff truth\"")
      {
        val p : IMPSMathExp = IMPSApply(IMPSMathSymbol("is%true"), List(IMPSMathSymbol("true%val")))
        Some(IMPSIff(p, IMPSTruth()))
      }
      else if (s == "\"is%true(false%val) iff falsehood\"")
      {
        val p : IMPSMathExp = IMPSApply(IMPSMathSymbol("is%true"), List(IMPSMathSymbol("false%val")))
        Some(IMPSIff(p, IMPSFalsehood()))
      }
      else if (s == "\"is%false(true%val) iff falsehood\"")
      {
        val p : IMPSMathExp = IMPSApply(IMPSMathSymbol("is%false"), List(IMPSMathSymbol("true%val")))
        Some(IMPSIff(p, IMPSFalsehood()))
      }
      else if (s == "\"is%false(false%val) iff truth\"")
      {
        val p : IMPSMathExp = IMPSApply(IMPSMathSymbol("is%false"), List(IMPSMathSymbol("false%val")))
        Some(IMPSIff(p, IMPSTruth()))
      }
      else if (s == "\"falsehood implies truth\"")
      {
        Some(IMPSImplication(IMPSFalsehood(), IMPSTruth()))
      }
      None
    }
    else
    {
      var store : Option[IMPSMathExp] = None
      var input : String = s

      input = input.trim
      if (input.startsWith("\"")) { input = input.drop(1) }
      if (input.endsWith("\""))   { input = input.dropRight(1)}
      input = input.trim

      /* This takes as reference the IMPS Manual pg. 65 */

      if      (input == "truth")             { store = Some(IMPSTruth()) }
      else if (input == "falsehood")         { store = Some(IMPSFalsehood()) }
      else if (input.startsWith("not("))     { store = parseIMPSNot(input)}
      else if (input.startsWith("lambda("))  { store = parseIMPSLambda(input) }
      else if (input.startsWith("forall("))  { ??? }
      else if (input.startsWith("forsome(")) { ??? }
      else if (input.startsWith("if_form(")) { ??? }
      else if (input.startsWith("if("))      { ??? }
      else if (input.startsWith("iota("))    { ??? }
      else if (input.startsWith("iota_p("))  { ??? }
      else if (input.startsWith("#("))       { ??? }
      else
      {
        /* Binding hierarchies taken from IMPS manual pg. 150*/

        if (input.contains(" iff "))
        {
          val iff_l = input.split(" iff ")
          if (iff_l.length == 2) {
            val t1 = parseIMPSMath(iff_l(0))
            val t2 = parseIMPSMath(iff_l(1))
            if (t1.isDefined && t2.isDefined) {
              store = Some(IMPSIff(t1.get,t2.get))
            }
          } else { println("[Error]:   iff with wrong number of parameters") }
        }
        else if (input.contains(" and ")) {
          val and_l = input.split(" and ")
          var and_lp : List[IMPSMathExp] = List.empty
          for (a <- and_l)
          {
            val q : Option[IMPSMathExp] = parseIMPSMath(a)
            if (q.isDefined) {
              and_lp = and_lp ::: List(q.get)
            } else {
              println("[Error]:   Couldn't parse conjunct " + a.toString)
            }
          }
          store = Some(IMPSConjunction(and_lp))
        }
        else if (input.contains(" or ")) {
          val or_l = input.split(" or ")
          var or_lp : List[IMPSMathExp] = List.empty
          for (a <- or_l)
          {
            val q : Option[IMPSMathExp] = parseIMPSMath(a)
            if (q.isDefined) {
              or_lp = or_lp ::: List(q.get)
            } else {
              println("[Error]:   Couldn't parse disjunct " + a.toString)
            }
          }
          store = Some(IMPSDisjunction(or_lp))
        }
        else if (input.contains(" implies "))
        {
          val impl_l = input.split(" implies ")
          if (impl_l.length == 2) {
            val t1 = parseIMPSMath(impl_l(0))
            val t2 = parseIMPSMath(impl_l(1))
            if (t1.isDefined && t2.isDefined) {
              store = Some(IMPSImplication(t1.get,t2.get))
            }
          } else { println("[Error]:   implies with wrong number of parameters") }
        }
        else if (input.endsWith(")")) {
          if (input.startsWith("("))
          {
            parseIMPSMath(input.dropRight(1).drop(1))
          }
          else {
            val n = input.indexWhere(c => c.toString == "(")
            println("[Debug]: inp. (" + n + ") " + input)
            val f : String = input.substring(0,n)
            val pars = input.drop(f.length + 1).dropRight(1).split(",")
            println("[Debug]: function symbol " + f)
            var parsedpars : List[IMPSMathExp] = List.empty
            for (p <- pars)
            {
              val q : Option[IMPSMathExp] = parseIMPSMath(p)
              if (q.isDefined) {
                parsedpars = parsedpars ::: List(q.get)
              } else {
                println("[Error]:   Couldn't parse parameter " + p.toString)
              }
            }
            val fp = parseIMPSMath(f)
            if (fp.isDefined) { store = Some(IMPSApply(fp.get, parsedpars)) }
          }
        }
        else
        {
          // ??? I guess ???
          store = Some(IMPSMathSymbol(input))
        }
      }

      if (store.isDefined)
      {
        println("\n[Success]: Parsed this math expression: " + s)
        println("[Success]: " + store.get.toString)
      }
      else
      {
        // If we haven't parsed successfully by now, we failed
        println("\n[Error]:   Failed parsing this math expression: " + s)
        println("[Error]:   " + store.get.toString)
      }
      store
    }
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

      var vars : List[(IMPSVar, Option[IMPSSortRef])] = List.empty

      for (pp <- arr.init)
      {
        if (pp.contains(':'))
        {
          val var_and_sort : Array[String] = pp.split(':')
          vars = vars :+ (IMPSVar(var_and_sort(0)), Some(IMPSSortRef(var_and_sort(1))))
        }
        else { vars = vars :+ (IMPSVar(pp), None) }
      }

      val t_opt : Option[IMPSMathExp] = parseIMPSMath(p)

      if (t_opt.isDefined)
      {
        Some(IMPSLambda(vars, t_opt.get))
      } else { None }
    } else { None }
  }

  private def parseIMPSNot(s : String) : Option[IMPSMathExp] =
  {
    if (s.startsWith("not(") && s.endsWith(")"))
    {
      val str   : String              = s.drop("not(".length).dropRight(1)
      val inner : Option[IMPSMathExp] = parseIMPSMath(str)

      if (inner.isDefined) { Some(IMPSNegation(inner.get)) }
      else { None }
    }
    else { None }
  }
}