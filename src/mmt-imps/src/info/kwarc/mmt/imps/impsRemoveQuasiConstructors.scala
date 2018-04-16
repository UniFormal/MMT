package info.kwarc.mmt.imps

import info.kwarc.mmt.imps.impsMathParser.freshVar

package object impsRemoveQuasiConstructors
{
  // TODO: Idea: Mark which Unknowns are the same, should help the typechecker inferring unknowns.

  var hashCount : Int = 0

  def freshHash() : Int =
  {
    hashCount += 1
    hashCount.toString.hashCode()
  }

  def removeQCs(input : IMPSMathExp, addCs : List[IMPSMathExp], cntxt : List[(IMPSVar,IMPSSort)]) : IMPSMathExp =
  {
    input match
    {
      case IMPSVar(_)
           | IMPSIndividual()
           | IMPSMathSymbol(_)
           | IMPSTruth()
           | IMPSFalsehood()        => input

      case IMPSNegation(p)        => IMPSNegation(removeQCs(p,addCs,cntxt))
      case IMPSIf(p,t1,t2)        => IMPSIf(removeQCs(p,addCs,cntxt),removeQCs(t1,addCs,cntxt),removeQCs(t2,addCs,cntxt))
      case IMPSIff(p, q)          => IMPSIff(removeQCs(p,addCs,cntxt), removeQCs(q,addCs,cntxt))
      case IMPSIfForm(p,q,r)      => IMPSIfForm(removeQCs(p,addCs,cntxt),removeQCs(q,addCs,cntxt),removeQCs(r,addCs,cntxt))
      case IMPSEquals(a,b)        => IMPSEquals(removeQCs(a,addCs,cntxt),removeQCs(b,addCs,cntxt))
      case IMPSDisjunction(ls)    => IMPSDisjunction(ls.map(l => removeQCs(l,addCs,cntxt)))
      case IMPSConjunction(ls)    => IMPSConjunction(ls.map(l => removeQCs(l,addCs,cntxt)))
      case IMPSLambda(vs,t)       => IMPSLambda(vs,removeQCs(t,addCs,cntxt))
      case IMPSForAll(vs,p)       => IMPSForAll(vs,removeQCs(p,addCs,cntxt))
      case IMPSForSome(vs,r)      => IMPSForSome(vs,removeQCs(r,addCs,cntxt))
      case IMPSImplication(p,q)   => IMPSImplication(removeQCs(p,addCs,cntxt),removeQCs(q,addCs,cntxt))
      case IMPSApply(f,ts)        => IMPSApply(removeQCs(f,addCs,cntxt),ts.map(t => removeQCs(t,addCs,cntxt)))
      case IMPSIota(v1,s1,p)      => IMPSIota(v1,s1,removeQCs(p,addCs,cntxt))
      case IMPSIotaP(v1,s1,p)     => IMPSIotaP(v1,s1,removeQCs(p,addCs,cntxt))
      case IMPSIsDefined(r)       => IMPSIsDefined(removeQCs(r,addCs,cntxt))
      case IMPSIsDefinedIn(r,s)   => IMPSIsDefinedIn(removeQCs(r,addCs,cntxt),s)
      case IMPSUndefined(s)       => IMPSUndefined(s)
      case IMPSTotal(f,bs)        => IMPSTotal(removeQCs(f,addCs,cntxt),bs)
      case IMPSNonVacuous(p)      => IMPSNonVacuous(removeQCs(p,addCs,cntxt))
      case IMPSQuasiEquals(p,q)   => IMPSQuasiEquals(removeQCs(p,addCs,cntxt),removeQCs(q,addCs,cntxt))

      case IMPSQCPred2Indicator(pred_u) =>
      {
        // "lambda(s:[uu,prop], lambda(x:uu, if(s(x), an%individual, ?unit%sort)))"
        val pred  : IMPSMathExp = removeQCs(pred_u,addCs,cntxt)

        var uu : IMPSSort = IMPSUnknownSort(freshHash())

        if (cntxt.map(_._1).contains(pred_u)) {
          val nsrt : IMPSSort = cntxt.find(c => c._1 == pred_u).get._2
          assert(nsrt.isInstanceOf[IMPSBinaryFunSort])
          nsrt match { case IMPSBinaryFunSort(s1,s2) => {
            assert(s2 == IMPSAtomSort("prop"))
            uu = s1
          } }
        }

        val s_srt = IMPSBinaryFunSort(uu,IMPSAtomSort("prop"))

        val s_var = (freshVar("s",List(pred)          ::: addCs), s_srt)
        val x_var = (freshVar("x",List(pred,s_var._1) ::: addCs), uu)

        val appl    = IMPSApply(s_var._1,List(x_var._1))
        val target  = IMPSIf(appl,IMPSIndividual(),IMPSUndefined(IMPSAtomSort("unitsort")))
        val resolve = IMPSApply(IMPSLambda(List(s_var), IMPSLambda(List(x_var),target)), List(pred))

        resolve
      }

      case IMPSQCSort2Indicator(sort_u) =>
      {
        // "lambda(e:uu, lambda(x:uu, an%individual))"
        val sort  : IMPSMathExp = removeQCs(sort_u,addCs,cntxt)

        var uu : IMPSSort = IMPSUnknownSort(freshHash())

        if (cntxt.map(_._1).contains(sort_u)) {
          uu = cntxt.find(c => c._1 == sort_u).get._2
        }

        val e_srt = uu

        val y_var = (freshVar("x",List(sort)          ::: addCs), uu)
        val e_var = (freshVar("e",List(sort,y_var._1) ::: addCs), e_srt)

        val inner   : IMPSMathExp = IMPSLambda(List(y_var),IMPSIndividual())
        val resolve : IMPSMathExp = IMPSApply(IMPSLambda(List(e_var),inner), List(sort))

        resolve
      }

      case IMPSQCIn(e1_u,e2_u) =>
      {
        // "lambda(x:uu,a:sets[uu], #(a(x)))"
        val e1    : IMPSMathExp = removeQCs(e1_u,addCs,cntxt)
        val e2    : IMPSMathExp = removeQCs(e2_u,List(e1) ::: addCs,cntxt)

        var uu : IMPSSort = IMPSUnknownSort(freshHash())

        if (cntxt.map(_._1).contains(e1_u)) {
          uu = cntxt.find(c => c._1 == e1_u).get._2
        } else if (cntxt.map(_._1).contains(e2_u)) {
          cntxt.find(c => c._1 == e2_u).get._2 match {
            case IMPSSetSort(nsrt) => uu = nsrt
            case IMPSBinaryFunSort(nsrt,usrt) => {
              assert(usrt == IMPSAtomSort("unit%sort"))
              uu = nsrt
            }
            case _                 => assert(false)
          }
        }

        val x_var = (freshVar("x",List(e1,e2)          ::: addCs), uu)
        val a_var = (freshVar("a",List(e1,e2,x_var._1) ::: addCs), IMPSSetSort(uu))

        val inner  : IMPSMathExp = IMPSIsDefined(IMPSApply(a_var._1,List(x_var._1)))
        val lambda : IMPSMathExp = IMPSLambda(List(x_var,a_var), inner)

        IMPSApply(lambda,List(e1,e2))
      }

      case IMPSQCSubsetEQ(e1_u, e2_u) =>
      {
        // "lambda(a,b:sets[uu], forall(x:uu, (x in a) implies (x in b)))"
        val e1    : IMPSMathExp = removeQCs(e1_u,addCs,cntxt)
        val e2    : IMPSMathExp = removeQCs(e2_u,List(e1) ::: addCs,cntxt)

        var uu : IMPSSort = IMPSUnknownSort(freshHash())

        if (cntxt.map(_._1).contains(e1_u)) {
          cntxt.find(c => c._1 == e1_u).get._2 match {
            case IMPSSetSort(nsrt) => uu = nsrt
            case IMPSBinaryFunSort(nsrt,usrt) => {
              assert(usrt == IMPSAtomSort("unit%sort"))
              uu = nsrt
            }
          }
        } else if  (cntxt.map(_._1).contains(e2_u)) {
          cntxt.find(c => c._1 == e2_u).get._2 match {
            case IMPSSetSort(nsrt) => uu = nsrt
            case IMPSBinaryFunSort(nsrt,usrt) => {
              assert(usrt == IMPSAtomSort("unit%sort"))
              uu = nsrt
            }
          }
        }

        val a_var = (freshVar("a",List(e1,e2) ::: addCs), IMPSSetSort(uu))
        val b_var = (freshVar("b",List(e1,e2,a_var._1) ::: addCs), IMPSSetSort(uu))
        val x_var = (freshVar("x",List(e1,e2,a_var._1,b_var._1) ::: addCs), uu)

        var addConstraints : List[IMPSMathExp]
        = List(e1,e2,a_var._1,b_var._1,x_var._1) ::: addCs

        val in1   : IMPSMathExp = removeQCs(IMPSQCIn(x_var._1,a_var._1), addConstraints,cntxt)
        addConstraints = addConstraints :+ in1
        val in2   : IMPSMathExp = removeQCs(IMPSQCIn(x_var._1,b_var._1), addConstraints,cntxt)

        val forall = IMPSForAll(List(x_var), IMPSImplication(in1,in2))
        val lambda = IMPSLambda(List(a_var,b_var),forall)

        IMPSApply(lambda,List(e1,e2))
      }

      case IMPSQCSubset(e1_u, e2_u) =>
      {
        // Not used?
        // "lambda(a,b:sets[uu], (a subseteq b) and not(a = b))"
        val e1    : IMPSMathExp = removeQCs(e1_u,addCs,cntxt)
        val e2    : IMPSMathExp = removeQCs(e2_u,List(e1) ::: addCs,cntxt)

        var uu : IMPSSort = IMPSUnknownSort(freshHash())

        val a_srt = if (cntxt.map(_._1).contains(e1_u)) { cntxt.find(c => c._1 == e1_u).get._2 }
        else { IMPSSetSort(uu) }

        val b_srt = if (cntxt.map(_._1).contains(e2_u)) { cntxt.find(c => c._1 == e2_u).get._2 }
        else { IMPSSetSort(uu) }

        val a_var = (freshVar("a",List(e1,e2) ::: addCs), a_srt)
        val b_var = (freshVar("b",List(e1,e2,a_var._1) ::: addCs), b_srt)

        var addConstraints : List[IMPSMathExp]
        = List(e1,e2,a_var._1,b_var._1) ::: addCs

        val subs : IMPSMathExp = IMPSQCSubsetEQ(a_var._1,b_var._1)
        val eq   : IMPSMathExp = removeQCs(IMPSEquals(a_var._1,b_var._1), addConstraints,cntxt)
        val neg  : IMPSMathExp = IMPSNegation(eq)

        val lambda = IMPSLambda(List(a_var,b_var), IMPSConjunction(List(subs,neg)))
        IMPSApply(lambda,List(e1,e2))
      }

      case IMPSQCEmptyIndicator(srt_u) =>
      {
        // "lambda(e:uu, lambda(x:uu,?unit%sort))"
        println(" > ei srt: " + srt_u)
        println(" > ei cntnxt: " + cntxt)

        var uu : IMPSSort = IMPSUnknownSort(freshHash())

        if (cntxt.map(_._1).contains(srt_u)) {
          uu = cntxt.find(c => c._1 == srt_u).get._2
        } else if (srt_u.isInstanceOf[IMPSUndefined]) {
          srt_u match {
            case IMPSUndefined(us) => uu = us
          }
        }

        val e_var = (freshVar("e",List(srt_u) ::: addCs), uu)
        val x_var = (freshVar("x",List(srt_u,e_var._1) ::: addCs), uu)

        val inner = IMPSLambda(List(x_var), IMPSUndefined(IMPSAtomSort("unitsort")))

        val addConstraints = addCs :+ inner

        val srt : IMPSMathExp = removeQCs(srt_u,addConstraints,cntxt :+ x_var :+ e_var)
        IMPSApply(IMPSLambda(List(e_var), inner),List(srt))
      }

      case IMPSQCNonemptyIndicator(srt_u) =>
      {
        // "lambda(a:sets[uu], forsome(x:uu, x in a))"
        var uu : IMPSSort = IMPSUnknownSort(freshHash())

        if (cntxt.map(_._1).contains(srt_u)) {
          cntxt.find(c => c._1 == srt_u).get._2 match {
            case IMPSSetSort(nsrt)            => uu = nsrt
            case IMPSBinaryFunSort(nsrt,usrt) => {
              assert(usrt == IMPSAtomSort("unit%sort"))
              uu = nsrt
            }
          }
        } else if (srt_u.isInstanceOf[IMPSUndefined]) {
          srt_u match {
            case IMPSUndefined(us) => uu = us
          }
        }

        val x_var = (freshVar("x", List(srt_u) ::: addCs), uu)
        val a_var = (freshVar("a", List(srt_u,x_var._1) ::: addCs), IMPSSetSort(uu))

        var addConstraints : List[IMPSMathExp] = List(srt_u,x_var._1,a_var._1) ::: addCs

        val forsome = removeQCs(IMPSForSome(List(x_var),IMPSQCIn(x_var._1,a_var._1)),addConstraints,cntxt)
        val lambda  = IMPSLambda(List(a_var),forsome)

        addConstraints = addConstraints :+ forsome :+ lambda
        val srt : IMPSMathExp = removeQCs(srt_u,addConstraints,cntxt :+ x_var :+ a_var)
        IMPSApply(lambda,List(srt))
      }

      case IMPSQCEmptyIndicatorQ(srt_u) =>
      {
        // "lambda(a:sets[uu], forall(x:uu, not(x in a)))"
        println(" > eiq srt: " + srt_u)
        println(" > eiq cntx: " + cntxt)

        var uu : IMPSSort = IMPSUnknownSort(freshHash())

        if (cntxt.map(_._1).contains(srt_u)) {
          uu = cntxt.find(c => c._1 == srt_u).get._2
        } else if (srt_u.isInstanceOf[IMPSUndefined]) {
          srt_u match {
            case IMPSUndefined(us) => uu = us
          }
        }

        val x_var = (freshVar("x", List(srt_u) ::: addCs), uu)
        val a_var = (freshVar("a", List(srt_u,x_var._1) ::: addCs), IMPSSetSort(uu))

        var addConstraints : List[IMPSMathExp] = List(srt_u,x_var._1,a_var._1) ::: addCs

        val forall = removeQCs(IMPSForAll(List(x_var),IMPSNegation(IMPSQCIn(x_var._1,a_var._1))),addConstraints,cntxt)
        val lambda = IMPSLambda(List(a_var),forall)

        addConstraints = addConstraints :+ forall :+ lambda

        val srt : IMPSMathExp = removeQCs(srt_u,addConstraints,cntxt :+ x_var :+ a_var)
        IMPSApply(lambda,List(srt))
      }

      case IMPSQCComplement(s_u) =>
      {
        // "lambda(s:sets[uu], indic(x:uu, (not #(s(x)))))"
        val srt : IMPSMathExp = removeQCs(s_u,addCs,cntxt)

        var uu : IMPSSort = IMPSUnknownSort(freshHash())

        val s_srt = if (cntxt.map(_._1).contains(s_u)) { cntxt.find(c => c._1 == s_u).get._2 }
        else { IMPSSetSort(uu) }

        val x_var = (freshVar("x", List(srt) ::: addCs), uu)
        val s_var = (freshVar("s", List(srt,x_var._1) ::: addCs), s_srt)

        var addConstraints : List[IMPSMathExp] = List(srt,x_var._1,s_var._1) ::: addCs

        val indic  = IMPSLambda(List(x_var),IMPSNegation(IMPSIsDefined(IMPSApply(s_var._1,List(x_var._1)))))
        val lambda = IMPSLambda(List(s_var),indic)
        val pred2indic = removeQCs(IMPSQCPred2Indicator(lambda),addConstraints,cntxt)
        IMPSApply(pred2indic,List(srt))
      }

      case IMPSQCUnion(u1,u2) =>
      {
        // "lambda(s,t:sets[uu], indic(x:uu, #(s(x)) or #(t(x))))"
        val srt1 : IMPSMathExp = removeQCs(u1,addCs,cntxt)
        val srt2 : IMPSMathExp = removeQCs(u2,List(srt1) ::: addCs,cntxt)

        var uu : IMPSSort = IMPSUnknownSort(freshHash())

        val s_srt = if (cntxt.map(_._1).contains(u1)) { cntxt.find(c => c._1 == u1).get._2 }
        else { IMPSSetSort(uu) }

        val t_srt = if (cntxt.map(_._1).contains(u2)) { cntxt.find(c => c._1 == u2).get._2 }
        else { IMPSSetSort(uu) }

        val x_var = (freshVar("x", List(srt1,srt2) ::: addCs), uu)
        val s_var = (freshVar("s", List(srt1,srt2,x_var._1) ::: addCs), s_srt)
        val t_var = (freshVar("t", List(srt1,srt2,x_var._1,s_var._1) ::: addCs), t_srt)

        var addConstraints : List[IMPSMathExp] = List(srt1,srt2,x_var._1,s_var._1,t_var._1) ::: addCs

        val def1   = IMPSIsDefined(IMPSApply(s_var._1,List(x_var._1)))
        val def2   = IMPSIsDefined(IMPSApply(t_var._1,List(x_var._1)))
        val or     = IMPSDisjunction(List(def1,def2))
        val indic  = removeQCs(IMPSQCPred2Indicator(IMPSLambda(List(x_var),or)),addConstraints,cntxt)
        val lambda = IMPSLambda(List(s_var,t_var),indic)

        IMPSApply(lambda,List(srt1,srt2))
      }

      case IMPSQCIntersection(i1,i2) =>
      {
        // "lambda(s,t:sets[uu], indic(x:uu, #(s(x)) and #(t(x))))"
        val srt1 : IMPSMathExp = removeQCs(i1,addCs,cntxt)
        val srt2 : IMPSMathExp = removeQCs(i2,List(srt1) ::: addCs,cntxt)

        var uu : IMPSSort = IMPSUnknownSort(freshHash())

        val s_srt = if (cntxt.map(_._1).contains(i1)) { cntxt.find(c => c._1 == i1).get._2 }
        else { IMPSSetSort(uu) }

        val t_srt = if (cntxt.map(_._1).contains(i2)) { cntxt.find(c => c._1 == i2).get._2 }
        else { IMPSSetSort(uu) }

        val x_var = (freshVar("x", List(srt1,srt2) ::: addCs), uu)
        val s_var = (freshVar("s", List(srt1,srt2,x_var._1) ::: addCs), s_srt)
        val t_var = (freshVar("t", List(srt1,srt2,x_var._1,s_var._1) ::: addCs), t_srt)

        var addConstraints : List[IMPSMathExp] = List(srt1,srt2,x_var._1,s_var._1,t_var._1) ::: addCs

        val def1   = IMPSIsDefined(IMPSApply(s_var._1,List(x_var._1)))
        val def2   = IMPSIsDefined(IMPSApply(t_var._1,List(x_var._1)))
        val and    = IMPSConjunction(List(def1,def2))
        val indic  = removeQCs(IMPSQCPred2Indicator(IMPSLambda(List(x_var),and)),addConstraints,cntxt)
        val lambda = IMPSLambda(List(s_var,t_var),indic)

        IMPSApply(lambda,List(srt1,srt2))
      }

      case IMPSQCDifference(d1,d2) =>
      {
        // "lambda(s,t:sets[uu], indic(x:uu, #(s(x)) and (not #(t(x)))))"
        val srt1 : IMPSMathExp = removeQCs(d1,addCs,cntxt)
        val srt2 : IMPSMathExp = removeQCs(d2,List(srt1) ::: addCs,cntxt)

        var uu : IMPSSort = IMPSUnknownSort(freshHash())

        val s_srt = if (cntxt.map(_._1).contains(d1)) { cntxt.find(c => c._1 == d1).get._2 }
        else { IMPSSetSort(uu) }

        val t_srt = if (cntxt.map(_._1).contains(d2)) { cntxt.find(c => c._1 == d2).get._2 }
        else { IMPSSetSort(uu) }

        val x_var = (freshVar("x", List(srt1,srt2) ::: addCs), uu)
        val s_var = (freshVar("s", List(srt1,srt2,x_var._1) ::: addCs), s_srt)
        val t_var = (freshVar("t", List(srt1,srt2,x_var._1,s_var._1) ::: addCs), t_srt)

        var addConstraints : List[IMPSMathExp] = List(srt1,srt2,x_var._1,s_var._1,t_var._1) ::: addCs

        val def1   = IMPSIsDefined(IMPSApply(s_var._1,List(x_var._1)))
        val def2   = IMPSNegation(IMPSIsDefined(IMPSApply(t_var._1,List(x_var._1))))
        val and    = IMPSConjunction(List(def1,def2))
        val indic  = removeQCs(IMPSQCPred2Indicator(IMPSLambda(List(x_var),and)),addConstraints,cntxt)
        val lambda = IMPSLambda(List(s_var,t_var),indic)

        IMPSApply(lambda,List(srt1,srt2))
      }

      case IMPSQCSymDifference(sd1,sd2) =>
      {
        // "lambda(s,t:sets[uu],indic(x:uu, (#(s(x)) and (not #(t(x)))) or ((not #(s(x))) and #(t(x)))))"
        val srt1 : IMPSMathExp = removeQCs(sd1,addCs,cntxt)
        val srt2 : IMPSMathExp = removeQCs(sd2,List(srt1) ::: addCs,cntxt)

        var uu : IMPSSort = IMPSUnknownSort(freshHash())

        val s_srt = if (cntxt.map(_._1).contains(sd1)) { cntxt.find(c => c._1 == sd1).get._2 }
        else { IMPSSetSort(uu) }

        val t_srt = if (cntxt.map(_._1).contains(sd2)) { cntxt.find(c => c._1 == sd2).get._2 }
        else { IMPSSetSort(uu) }

        val x_var = (freshVar("x", List(srt1,srt2) ::: addCs), uu)
        val s_var = (freshVar("s", List(srt1,srt2,x_var._1) ::: addCs), s_srt)
        val t_var = (freshVar("t", List(srt1,srt2,x_var._1,s_var._1) ::: addCs), t_srt)

        var addConstraints : List[IMPSMathExp] = List(srt1,srt2,x_var._1,s_var._1,t_var._1) ::: addCs

        val def1   = IMPSIsDefined(IMPSApply(s_var._1,List(x_var._1)))
        val def2   = IMPSIsDefined(IMPSApply(t_var._1,List(x_var._1)))
        val and1   = IMPSConjunction(List(def1,IMPSNegation(def2)))
        val and2   = IMPSConjunction(List(IMPSNegation(def1),def2))
        val or     = IMPSDisjunction(List(and1,and2))
        val indic  = removeQCs(IMPSQCPred2Indicator(IMPSLambda(List(x_var),or)),addConstraints,cntxt)
        val lambda = IMPSLambda(List(s_var,t_var),indic)

        IMPSApply(lambda,List(srt1,srt2))
      }

      case IMPSQCDisjoint(dj1,dj2) =>
      {
        // "lambda(s,t:sets[uu], forall(x:uu, not(x in s) or not(x in t)))"
        val srt1 : IMPSMathExp = removeQCs(dj1,addCs,cntxt)
        val srt2 : IMPSMathExp = removeQCs(dj2,List(srt1) ::: addCs,cntxt)

        var uu : IMPSSort = IMPSUnknownSort(freshHash())

        val s_srt = if (cntxt.map(_._1).contains(dj1)) { cntxt.find(c => c._1 == dj1).get._2 }
        else { IMPSSetSort(uu) }

        val t_srt = if (cntxt.map(_._1).contains(dj2)) { cntxt.find(c => c._1 == dj2).get._2 }
        else { IMPSSetSort(uu) }

        val x_var = (freshVar("x", List(srt1,srt2) ::: addCs), uu)
        val s_var = (freshVar("s", List(srt1,srt2,x_var._1) ::: addCs), s_srt)
        val t_var = (freshVar("t", List(srt1,srt2,x_var._1,s_var._1) ::: addCs), t_srt)

        var addConstraints : List[IMPSMathExp] = List(srt1,srt2,x_var._1,s_var._1,t_var._1) ::: addCs

        val not1   = IMPSNegation(removeQCs(IMPSQCIn(x_var._1,s_var._1), addConstraints,cntxt))
        addConstraints = addConstraints :+ not1
        val not2   = IMPSNegation(removeQCs(IMPSQCIn(x_var._1,t_var._1), addConstraints,cntxt))
        val or     = IMPSDisjunction(List(not1,not2))
        val forall = IMPSForAll(List(x_var),or)
        val lambda = IMPSLambda(List(s_var,t_var),forall)

        IMPSApply(lambda, List(srt1,srt2))
      }

      /* Quasi-constructor of DOOM! */
      case IMPSQCPartitionQ(w_u,s_u) =>
      {
        // "lambda(w:sets[sets[uu]],s:sets[uu], forall(u,v:sets[uu],
        //      ((not (u = v)) and (u in w) and (v in w)) implies (u disj v))
        //           and forall(x:uu, (x in s) iff forsome(u:sets[uu], (u in w) and (x in u))))"
        val w_d : IMPSMathExp = removeQCs(w_u,addCs,cntxt)
        val s_d : IMPSMathExp = removeQCs(s_u,List(w_d) ::: addCs,cntxt)

        var uu : IMPSSort = IMPSUnknownSort(freshHash())

        val w_srt = if (cntxt.map(_._1).contains(w_u)) { cntxt.find(c => c._1 == w_u).get._2 }
        else { IMPSSetSort(IMPSSetSort(uu)) }

        val s_srt = if (cntxt.map(_._1).contains(s_u)) { cntxt.find(c => c._1 == s_u).get._2 }
        else { IMPSSetSort(uu) }

        val x_var = (freshVar("x", List(w_d,w_u) ::: addCs), uu)
        val u_var = (freshVar("u", List(w_d,w_u,x_var._1) ::: addCs), IMPSSetSort(uu))
        val o_var = (freshVar("o", List(w_d,w_u,x_var._1,u_var._1) ::: addCs), IMPSSetSort(uu))
        val v_var = (freshVar("v", List(w_d,w_u,x_var._1,u_var._1,o_var._1) ::: addCs), IMPSSetSort(uu))
        val s_var = (freshVar("s", List(w_d,w_u,x_var._1,u_var._1,o_var._1,v_var._1) ::: addCs), s_srt)
        val w_var = (freshVar("w", List(w_d,w_u,x_var._1,u_var._1,o_var._1,v_var._1,s_var._1) ::: addCs), w_srt)

        var addConstraints : List[IMPSMathExp] = List(w_d,s_d,x_var._1,u_var._1,o_var._1,v_var._1,s_var._1,w_var._1) ::: addCs

        // LHS

        val c1 = IMPSNegation(IMPSEquals(u_var._1,v_var._1))
        val c2 = removeQCs(IMPSQCIn(u_var._1,w_var._1),addConstraints,cntxt)
        addConstraints = addConstraints :+ c2
        val c3 = removeQCs(IMPSQCIn(v_var._1,w_var._1),addConstraints,cntxt)
        addConstraints = addConstraints :+ c3

        val disj = removeQCs(IMPSQCDisjoint(u_var._1,v_var._1),addConstraints,cntxt)
        addConstraints = addConstraints :+ disj

        val impl         = IMPSImplication(IMPSConjunction(List(c1,c2,c3)), disj)
        val left_forall  = IMPSForAll(List(u_var,v_var),impl)

        // RHS

        val oiw          = removeQCs(IMPSQCIn(o_var._1,w_var._1),addConstraints,cntxt)
        addConstraints = addConstraints :+ oiw
        val xio          = removeQCs(IMPSQCIn(x_var._1,o_var._1),addConstraints,cntxt)
        addConstraints = addConstraints :+ xio

        val forsome      = IMPSForSome(List(o_var),IMPSConjunction(List(oiw,xio)))
        val rin          = removeQCs(IMPSQCIn(x_var._1,s_var._1),addConstraints,cntxt)
        addConstraints = addConstraints :+ rin

        val right_forall = IMPSForAll(List(x_var),IMPSIff(rin,forsome))

        // Final

        val inner  = IMPSConjunction(List(left_forall,right_forall))
        val lambda = IMPSLambda(List(w_var,s_var),inner)

        IMPSApply(lambda,List(w_d,s_d))
      }

      case IMPSQCSingleton(n) =>
      {
        // "lambda(a:uu, indic(x:uu, x=a))"
        val srt : IMPSMathExp = removeQCs(n,addCs,cntxt)

        var uu : IMPSSort = IMPSUnknownSort(freshHash())

        val a_srt = if (cntxt.map(_._1).contains(n)) { cntxt.find(c => c._1 == n).get._2 }
        else { uu }

        val a_var = (freshVar("a",List(srt) ::: addCs), a_srt)
        val x_var = (freshVar("x",List(srt,a_var._1) ::: addCs), uu)

        var addConstraints : List[IMPSMathExp] = List(srt,x_var._1,a_var._1) ::: addCs

        val inner  = IMPSLambda(List(x_var), IMPSEquals(x_var._1,a_var._1))
        val indic  = removeQCs(IMPSQCPred2Indicator(inner),addConstraints,cntxt)
        val lambda = IMPSLambda(List(a_var), indic)

        IMPSApply(lambda,List(srt))
      }

      case IMPSQCBigUnion(f_u) =>
      {
        // "lambda(f:[index,sets[uu]], indic(x:uu, forsome(i:index, #(f(i)(x)))))"
        val g : IMPSMathExp = removeQCs(f_u,addCs,cntxt)

        var index : IMPSSort = IMPSUnknownSort(freshHash())
        var uu    : IMPSSort = IMPSUnknownSort(freshHash())

        val f_srt = if (cntxt.map(_._1).contains(f_u)) {
          val srt = cntxt.find(c => c._1 == f_u).get._2
          assert(srt.isInstanceOf[IMPSBinaryFunSort])
          srt match { case IMPSBinaryFunSort(s1,s2) => {
            index = s1
            assert(s2.isInstanceOf[IMPSSetSort] || s2.isInstanceOf[IMPSBinaryFunSort])
            s2 match {
              case IMPSSetSort(setsrt) => uu = setsrt
              case IMPSBinaryFunSort(t1,t2) => {
                assert(t2 == IMPSAtomSort("unit%sort"))
                uu = t1
              }
            }
          }
          }
          srt
        }
        else { IMPSBinaryFunSort(index,IMPSSetSort(uu)) }

        val f_var = (freshVar("f",List(g) ::: addCs), f_srt)
        val x_var = (freshVar("x",List(g,f_var._1) ::: addCs), uu)
        val i_var = (freshVar("i",List(g,f_var._1,x_var._1) ::: addCs), index)

        var addConstraints : List[IMPSMathExp] = List(g,x_var._1,f_var._1,i_var._1) ::: addCs

        val inner  = IMPSForSome(List(i_var),IMPSIsDefined(IMPSApply(f_var._1,List(i_var._1,x_var._1))))
        val indic  = IMPSLambda(List(x_var),inner)
        val pred   = removeQCs(IMPSQCPred2Indicator(indic),addConstraints,cntxt)
        val lambda = IMPSLambda(List(f_var),pred)

        IMPSApply(lambda,List(g))
      }

      case IMPSQCBigIntersection(f_u) =>
      {
        // "lambda(f:[index,sets[uu]], indic(x:uu, forall(i:index, #(f(i)(x)))))"
        val g : IMPSMathExp = removeQCs(f_u,addCs,cntxt)

        var index : IMPSSort = IMPSUnknownSort(freshHash())
        var uu    : IMPSSort = IMPSUnknownSort(freshHash())

        val f_srt = if (cntxt.map(_._1).contains(f_u)) {
          val srt = cntxt.find(c => c._1 == f_u).get._2
          assert(srt.isInstanceOf[IMPSBinaryFunSort])
          srt match { case IMPSBinaryFunSort(s1,s2) => {
              index = s1
            assert(s2.isInstanceOf[IMPSSetSort] || s2.isInstanceOf[IMPSBinaryFunSort])
            s2 match {
              case IMPSSetSort(setsrt) => uu = setsrt
              case IMPSBinaryFunSort(t1,t2) => {
                assert(t2 == IMPSAtomSort("unit%sort"))
                uu = t1
              }
            }
            }
          }
          srt
        }
        else { IMPSBinaryFunSort(index,IMPSSetSort(uu)) }

        val f_var = (freshVar("f",List(g) ::: addCs), f_srt)
        val x_var = (freshVar("x",List(g,f_var._1) ::: addCs), uu)
        val i_var = (freshVar("i",List(g,f_var._1,x_var._1) ::: addCs), index)

        var addConstraints : List[IMPSMathExp] = List(g,x_var._1,f_var._1,i_var._1) ::: addCs

        val inner  = IMPSForAll(List(i_var),IMPSIsDefined(IMPSApply(f_var._1,List(i_var._1,x_var._1))))
        val indic  = IMPSLambda(List(x_var),inner)
        val pred   = removeQCs(IMPSQCPred2Indicator(indic),addConstraints,cntxt)
        val lambda = IMPSLambda(List(f_var),pred)

        IMPSApply(lambda,List(g))
      }

      case IMPSQCMComposition(g_u,f_u) =>
      {
        // "lambda(f:[ind_2,ind_3],g:[ind_1,ind_2], lambda(x:ind_1, f(g(x))))"
        val g : IMPSMathExp = removeQCs(g_u,addCs,cntxt)
        val f : IMPSMathExp = removeQCs(f_u,List(g) ::: addCs,cntxt)

        val x_var = (freshVar("x",List(f,g) ::: addCs), IMPSAtomSort("ind_1"))
        val f_var = (freshVar("f",List(f,g,x_var._1) ::: addCs), IMPSBinaryFunSort(IMPSAtomSort("ind_2"),IMPSAtomSort("ind_3")))
        val g_var = (freshVar("g",List(f,g,x_var._1,f_var._1) ::: addCs), IMPSBinaryFunSort(IMPSAtomSort("ind_1"),IMPSAtomSort("ind_2")))

        val inner  = IMPSLambda(List(x_var),IMPSApply(f_var._1,List(IMPSApply(g_var._1,List(x_var._1)))))
        val lambda = IMPSLambda(List(f_var,g_var),inner)

        IMPSApply(lambda,List(f,g))
      }

      case IMPSQCMDomain(f_u) =>
      {
        // "lambda(f:[ind_1,ind_2], indic(x:ind_1, #(f(x))))"
        val f : IMPSMathExp = removeQCs(f_u,addCs,cntxt)

        val x_var = (freshVar("x",List(f) ::: addCs), IMPSAtomSort("ind_1"))
        val f_var = (freshVar("f",List(f,x_var._1) ::: addCs), IMPSBinaryFunSort(IMPSAtomSort("ind_1"),IMPSAtomSort("ind_2")))

        var addConstraints : List[IMPSMathExp] = List(f,f_var._1,x_var._1) ::: addCs

        val inner  = IMPSLambda(List(x_var),IMPSIsDefined(IMPSApply(f_var._1,List(x_var._1))))
        val indic  = removeQCs(IMPSQCPred2Indicator(inner),addConstraints,cntxt)
        val lambda = IMPSLambda(List(f_var), indic)

        IMPSApply(lambda,List(f))
      }

      case IMPSQCMRange(f_u) =>
      {
        // "lambda(f:[ind_1,ind_2], indic(y:ind_2, forsome(x:ind_1, y=f(x))))"
        val f : IMPSMathExp = removeQCs(f_u,addCs,cntxt)

        val x_var = (freshVar("x",List(f) ::: addCs), IMPSAtomSort("ind_1"))
        val y_var = (freshVar("y",List(f,x_var._1) ::: addCs), IMPSAtomSort("ind_2"))
        val f_var = (freshVar("f",List(f,x_var._1,y_var._1) ::: addCs), IMPSBinaryFunSort(IMPSAtomSort("ind_1"),IMPSAtomSort("ind_2")))

        var addConstraints : List[IMPSMathExp] = List(f,f_var._1,x_var._1,y_var._1) ::: addCs

        val inner  = IMPSLambda(List(y_var),IMPSForSome(List(x_var),IMPSEquals(y_var._1,IMPSApply(f_var._1,List(x_var._1)))))
        val indic  = removeQCs(IMPSQCPred2Indicator(inner),addConstraints,cntxt)
        val lambda = IMPSLambda(List(f_var), indic)

        IMPSApply(lambda,List(f))
      }

      case IMPSQCMImage(f_u,s_u) =>
      {
        // "lambda(f:[ind_1,ind_2],a:sets[ind_1], indic(y:ind_2, forsome(x:ind_1, (x in a) and y=f(x))))"
        val f = removeQCs(f_u,addCs,cntxt)
        val s = removeQCs(s_u,List(f) ::: addCs,cntxt)

        val x_var = (freshVar("x",List(f,s) ::: addCs),IMPSAtomSort("ind_1"))
        val y_var = (freshVar("y",List(f,s,x_var._1) ::: addCs), IMPSAtomSort("ind_2"))
        val a_var = (freshVar("a",List(f,s,x_var._1,y_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("ind_1")))
        val f_var = (freshVar("f",List(f,s,x_var._1,y_var._1,a_var._1) ::: addCs), IMPSBinaryFunSort(IMPSAtomSort("ind_1"),IMPSAtomSort("ind_2")))

        var addConstraints : List[IMPSMathExp] = List(f,s,x_var._1,y_var._1,a_var._1,f_var._1) ::: addCs

        val in      = removeQCs(IMPSQCIn(x_var._1,a_var._1),addConstraints,cntxt)
        addConstraints = addConstraints :+ in
        val and     = IMPSConjunction(List(in,IMPSEquals(y_var._1,IMPSApply(f_var._1,List(x_var._1)))))
        val forsome = IMPSForSome(List(x_var),and)
        val indic   = removeQCs(IMPSQCPred2Indicator(IMPSLambda(List(y_var),forsome)),addConstraints,cntxt)
        val lambda  = IMPSLambda(List(f_var),indic)

        IMPSApply(lambda,List(f,s))
      }

      case IMPSQCMInverseImage(f_u,b_u) =>
      {
        // "lambda(f:[ind_1,ind_2],b:sets[ind_2], b oo f)"
        val f = removeQCs(f_u,addCs,cntxt)
        val b = removeQCs(b_u,List(f) ::: addCs,cntxt)

        val b_var = (freshVar("b",List(f,b) ::: addCs),IMPSSetSort(IMPSAtomSort("ind_2")))
        val f_var = (freshVar("f",List(f,b,b_var._1) ::: addCs),IMPSBinaryFunSort(IMPSAtomSort("ind_1"),IMPSAtomSort("ind_2")))

        var addConstraints : List[IMPSMathExp] = List(f_var._1,b_var._1,f,b) ::: addCs

        val comp = removeQCs(IMPSQCMComposition(b_var._1,f_var._1),addConstraints,cntxt)
        IMPSApply(comp,List(f,b))
      }

      case IMPSQCMInverse(f_u) =>
      {
        // "lambda(f:[ind_1,ind_2],lambda(x:ind_2, iota(y:ind_1, f(y)=x)))"
        val f = removeQCs(f_u,addCs,cntxt)

        val y_var = (freshVar("y",List(f) ::: addCs), IMPSAtomSort("ind_1"))
        val x_var = (freshVar("x",List(f,y_var._1) ::: addCs), IMPSAtomSort("ind_2"))
        val f_var = (freshVar("f",List(f,y_var._1,x_var._1) ::: addCs), IMPSBinaryFunSort(IMPSAtomSort("ind_1"),IMPSAtomSort("ind_2")))

        val eq     = IMPSEquals(IMPSApply(f,List(y_var._1)), x_var._1)
        val iota   = IMPSIota(y_var._1,y_var._2,eq)
        val inner  = IMPSLambda(List(x_var),iota)
        val lambda = IMPSLambda(List(f_var),inner)

        IMPSApply(lambda,List(f))
      }

      case IMPSQCMId(f_u) =>
      {
        // "lambda(a:sets[ind_1],lambda(x:ind_1, if(x in a, x, ?ind_1)))"
        val f = removeQCs(f_u,addCs,cntxt)

        val x_var = (freshVar("x",List(f) ::: addCs),IMPSAtomSort("ind_1"))
        val a_var = (freshVar("a",List(f,x_var._1) ::: addCs),IMPSSetSort(IMPSAtomSort("ind_1")))

        val addConstraints : List[IMPSMathExp] = List(x_var._1,a_var._1,f) ::: addCs

        val in  = removeQCs(IMPSQCIn(x_var._1,a_var._1),addConstraints,cntxt)
        val ifp = IMPSIf(in, x_var._1, IMPSUndefined(IMPSAtomSort("ind_1")))
        val inn = IMPSLambda(List(x_var),ifp)
        val lam = IMPSLambda(List(a_var),inn)

        IMPSApply(lam,List(f))
      }

      case IMPSQCMRestrict(f_u,a_u) =>
      {
        // "lambda(f:[ind_1,ind_2],a:sets[ind_1],lambda(x:ind_1, if(x in a, f(x), ?ind_2)))"
        val f = removeQCs(f_u,addCs,cntxt)
        val a = removeQCs(f_u,List(f) ::: addCs,cntxt)

        val x_var = (freshVar("x",List(f,a) ::: addCs), IMPSAtomSort("ind_1"))
        val a_var = (freshVar("a",List(f,a,x_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("ind_1")))
        val f_var = (freshVar("f",List(f,a,x_var._1,a_var._1) ::: addCs), IMPSBinaryFunSort(IMPSAtomSort("ind_1"),IMPSAtomSort("ind_2")))

        var addConstraints = List(f,a,x_var._1,a_var._1,f_var._1) ::: addCs

        val in  = removeQCs(IMPSQCIn(x_var._1,a_var._1),addConstraints,cntxt)
        val ifp = IMPSIf(in,IMPSApply(f_var._1,List(x_var._1)),IMPSUndefined(IMPSAtomSort("ind_2")))
        val inn = IMPSLambda(List(x_var),ifp)
        val lam = IMPSLambda(List(f_var,a_var),inn)

        IMPSApply(lam, List(f,a))
      }

      case IMPSQCMRestrict2(f_u,a_u,b_u) =>
      {
        // "lambda(f:[ind_1,ind_2,ind_3],a:sets[ind_1],b:sets[ind_2],lambda(x:ind_1,y:ind_2, if(x in a and y in b, f(x,y), ?ind_3)))"
        val f = removeQCs(f_u,addCs,cntxt)
        val a = removeQCs(a_u,List(f) ::: addCs,cntxt)
        val b = removeQCs(b_u,List(f,a) ::: addCs,cntxt)

        val x_var = (freshVar("x",List(f,a,b) ::: addCs), IMPSAtomSort("ind_1"))
        val y_var = (freshVar("y",List(f,a,b,x_var._1) ::: addCs), IMPSAtomSort("ind_2"))
        val a_var = (freshVar("a",List(f,a,b,x_var._1,y_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("ind_1")))
        val b_var = (freshVar("b",List(f,a,b,x_var._1,y_var._1,a_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("ind_2")))
        val f_var = (freshVar("f",List(f,a,b,x_var._1,y_var._1,a_var._1,b_var._1) ::: addCs),
          IMPSNaryFunSort(List(IMPSAtomSort("ind_1"),IMPSAtomSort("ind_2"), IMPSAtomSort("ind_3"))))

        var addConstraints = List(f,a,b,x_var._1,y_var._1,a_var._1,b_var._1,f_var._1) ::: addCs

        val in1 = removeQCs(IMPSQCIn(x_var._1,a_var._1),addConstraints,cntxt)
        addConstraints = addConstraints :+ in1
        val in2 = removeQCs(IMPSQCIn(y_var._1,b_var._1),addConstraints,cntxt)

        val ifp = IMPSIf(IMPSConjunction(List(in1,in2)),IMPSApply(f_var._1,List(x_var._1,y_var._1)),IMPSUndefined(IMPSAtomSort("ind_3")))
        val inn = IMPSLambda(List(x_var,y_var),ifp)
        val lam = IMPSLambda(List(f_var,a_var,b_var),inn)

        IMPSApply(lam, List(f,a,b))
      }

      case IMPSQCMSurjective(f_u) =>
      {
        // "lambda(f:[ind_1,ind_2],forall(x:ind_1, x in dom(f)) and forall(y:ind_2, y in ran(f)))"
        val f = removeQCs(f_u,addCs,cntxt)

        val x_var = (freshVar("x",List(f) ::: addCs), IMPSAtomSort("ind_1"))
        val y_var = (freshVar("y",List(f,x_var._1) ::: addCs), IMPSAtomSort("ind_2"))
        val f_var = (freshVar("f",List(f,x_var._1,y_var._1) ::: addCs), IMPSBinaryFunSort(IMPSAtomSort("ind_1"),IMPSAtomSort("ind_2")))

        var addConstraints = List(f,x_var._1,y_var._1,f_var._1) ::: addCs

        val dom   = removeQCs(IMPSQCMDomain(f_var._1),addConstraints,cntxt)
        addConstraints = addConstraints :+ dom
        val in1   = removeQCs(IMPSQCIn(x_var._1,dom),addConstraints,cntxt)
        addConstraints = addConstraints :+ in1

        val ran   = removeQCs(IMPSQCMRange(f_var._1),addConstraints,cntxt)
        addConstraints = addConstraints :+ ran
        val in2   = removeQCs(IMPSQCIn(y_var._1,ran),addConstraints,cntxt)
        addConstraints = addConstraints :+ in2

        val fora1 = IMPSForAll(List(x_var),in1)
        val fora2 = IMPSForAll(List(y_var),in2)
        val lambda = IMPSLambda(List(f_var),IMPSConjunction(List(fora1,fora2)))

        IMPSApply(lambda,List(f))
      }

      case IMPSQCMInjective(f_u) =>
      {
        // "lambda(f:[ind_1,ind_2],forall(x_1,x_2:ind_1, f(x_1)=f(x_2) implies x_1=x_2))"
        val f = removeQCs(f_u,addCs,cntxt)

        val x1_var = (freshVar("x_1",List(f) ::: addCs),IMPSAtomSort("ind_1"))
        val x2_var = (freshVar("x_2",List(f,x1_var._1) ::: addCs),IMPSAtomSort("ind_1"))
        val  f_var = (freshVar("f",List(f,x1_var._1,x2_var._1) ::: addCs),IMPSBinaryFunSort(IMPSAtomSort("ind_1"),IMPSAtomSort("ind_2")))

        val eq1 = IMPSEquals(IMPSApply(f_var._1,List(x1_var._1)),IMPSApply(f_var._1,List(x2_var._1)))
        val eq2 = IMPSEquals(x1_var._1,x2_var._1)

        val fora = IMPSForAll(List(x1_var,x2_var),IMPSImplication(eq1,eq2))
        val lamb = IMPSLambda(List(f_var),fora)

        IMPSApply(lamb,List(f))
      }

      case IMPSQCMBijective(f_u) =>
      {
        // "lambda(f:[ind_1,ind_2], surjective_q(f) and injective_q(f))"
        val f = removeQCs(f_u,addCs,cntxt)
        val f_var = (freshVar("f", List(f) ::: addCs), IMPSBinaryFunSort(IMPSAtomSort("ind_1"),IMPSAtomSort("ind_2")))

        var addConstraints = List(f,f_var._1) ::: addCs

        val sur = removeQCs(IMPSQCMSurjective(f_var._1),addConstraints,cntxt)
        addConstraints = addConstraints :+ sur
        val inj = removeQCs(IMPSQCMInjective(f_var._1),addConstraints,cntxt)

        val and = IMPSConjunction(List(sur,inj))
        val lam = IMPSLambda(List(f_var),and)

        IMPSApply(lam,List(f))
      }

      case IMPSQCMSurjectiveOn(f_u,a_u,b_u) =>
      {
        // "lambda(f:[ind_1,ind_2],a:sets[ind_1],b:sets[ind_2],dom(f)=a and ran(f)=b)"
        val f = removeQCs(f_u,addCs,cntxt)
        val a = removeQCs(a_u,List(f) ::: addCs,cntxt)
        val b = removeQCs(b_u,List(f,a) ::: addCs,cntxt)

        val a_var = (freshVar("a",List(f,a,b) ::: addCs), IMPSSetSort(IMPSAtomSort("ind_1")))
        val b_var = (freshVar("b",List(f,a,b,a_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("ind_2")))
        val f_var = (freshVar("f",List(f,a,b,a_var._1,b_var._1) ::: addCs), IMPSBinaryFunSort(IMPSAtomSort("ind_1"),IMPSAtomSort("ind_2")))

        var addConstraints = List(f,a,b,f_var._1,a_var._1,b_var._1) ::: addCs

        val dom = removeQCs(IMPSQCMDomain(f_var._1),addConstraints,cntxt)
        addConstraints = addConstraints :+ dom
        val ran = removeQCs(IMPSQCMRange(f_var._1),addConstraints,cntxt)

        val eq1 = IMPSEquals(dom,a_var._1)
        val eq2 = IMPSEquals(ran,b_var._1)

        val lambda = IMPSLambda(List(f_var,a_var,b_var), IMPSConjunction(List(eq1,eq2)))

        IMPSApply(lambda,List(f,a,b))
      }

      case IMPSQCMInjectiveOn(f_u,a_u) =>
      {
        // "lambda(f:[ind_1,ind_2],a:sets[ind_1],forall(x_1,x_2:ind_1,((x_1 in a) and (x_2 in a) and f(x_1)=f(x_2)) implies x_1=x_2))"
        val f = removeQCs(f_u,addCs,cntxt)
        val a = removeQCs(a_u,List(f) ::: addCs,cntxt)

        val x1_var = (freshVar("x_1", List(f,a) ::: addCs), IMPSAtomSort("ind_1"))
        val x2_var = (freshVar("x_2", List(f,a,x1_var._1) ::: addCs), IMPSAtomSort("ind_1"))
        val  a_var = (freshVar("a", List(f,a,x1_var._1,x2_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("ind_1")))
        val  f_var = (freshVar("f", List(f,a,x1_var._1,x2_var._1,a_var._1)), IMPSBinaryFunSort(IMPSAtomSort("ind_1"),IMPSAtomSort("ind_2")))

        var addConstraints = List(f,a,f_var._1,a_var._1,x1_var._1,x2_var._1) ::: addCs

        val a1 = removeQCs(IMPSQCIn(x1_var._1,a_var._1),addConstraints,cntxt)
        addConstraints = addConstraints :+ a1
        val a2 = removeQCs(IMPSQCIn(x2_var._1,a_var._1),addConstraints,cntxt)
        addConstraints = addConstraints :+ a2
        val a3 = IMPSEquals(IMPSApply(f_var._1,List(x1_var._1)),IMPSApply(f_var._1,List(x2_var._1)))

        val and = IMPSConjunction(List(a1,a2,a3))
        val imp = IMPSImplication(and,IMPSEquals(x1_var._1,x2_var._1))
        val lam = IMPSLambda(List(f_var,a_var),IMPSForAll(List(x1_var,x2_var),imp))

        IMPSApply(lam,List(f,a))
      }

      case IMPSQCMBijectiveOn(f_u,a_u,b_u) =>
      {
        // "lambda(f:[ind_1,ind_2],a:sets[ind_1],b:sets[ind_2],surjective_on_q(f,a,b) and injective_on_q(f,a))"
        val f = removeQCs(f_u,addCs,cntxt)
        val a = removeQCs(a_u,List(f) ::: addCs,cntxt)
        val b = removeQCs(b_u,List(f,a) ::: addCs,cntxt)

        val a_var = (freshVar("a",List(f,a,b) ::: addCs), IMPSSetSort(IMPSAtomSort("ind_1")))
        val b_var = (freshVar("a",List(f,a,b,a_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("ind_2")))
        val f_var = (freshVar("f",List(f,a,b,a_var._1,b_var._1) ::: addCs), IMPSBinaryFunSort(IMPSAtomSort("ind_1"),IMPSAtomSort("ind_2")))

        var addConstraints = List(f,a,b,f_var._1,a_var._1,b_var._1) ::: addCs

        val sur = removeQCs(IMPSQCMSurjectiveOn(f_var._1,a_var._1,b_var._1),addConstraints,cntxt)
        addConstraints = addConstraints :+ sur
        val inj = removeQCs(IMPSQCMInjectiveOn(f_var._1,a_var._1),addConstraints,cntxt)

        val lam = IMPSLambda(List(f_var,a_var,b_var), IMPSConjunction(List(sur,inj)))

        IMPSApply(lam,List(f,a,b))
      }
    }
  }
}
