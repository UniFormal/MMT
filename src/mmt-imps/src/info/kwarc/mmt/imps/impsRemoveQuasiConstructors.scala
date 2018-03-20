package info.kwarc.mmt.imps

import info.kwarc.mmt.imps.impsMathParser.freshVar

package object impsRemoveQuasiConstructors
{
  def removeQCs(input : IMPSMathExp, addCs : List[IMPSMathExp]) : IMPSMathExp =
  {
    input match
    {
      case IMPSVar(_)
           | IMPSIndividual()
           | IMPSMathSymbol(_)
           | IMPSTruth()
           | IMPSFalsehood()        => input

      case IMPSNegation(p)        => IMPSNegation(removeQCs(p,addCs))
      case IMPSIf(p,t1,t2)        => IMPSIf(removeQCs(p,addCs),removeQCs(t1,addCs),removeQCs(t2,addCs))
      case IMPSIff(p, q)          => IMPSIff(removeQCs(p,addCs), removeQCs(q,addCs))
      case IMPSIfForm(p,q,r)      => IMPSIfForm(removeQCs(p,addCs),removeQCs(q,addCs),removeQCs(r,addCs))
      case IMPSEquals(a,b)        => IMPSEquals(removeQCs(a,addCs),removeQCs(b,addCs))
      case IMPSDisjunction(ls)    => IMPSDisjunction(ls.map(l => removeQCs(l,addCs)))
      case IMPSConjunction(ls)    => IMPSConjunction(ls.map(l => removeQCs(l,addCs)))
      case IMPSLambda(vs,t)       => IMPSLambda(vs,removeQCs(t,addCs))
      case IMPSForAll(vs,p)       => IMPSForAll(vs,removeQCs(p,addCs))
      case IMPSForSome(vs,r)      => IMPSForSome(vs,removeQCs(r,addCs))
      case IMPSImplication(p,q)   => IMPSImplication(removeQCs(p,addCs),removeQCs(q,addCs))
      case IMPSApply(f,ts)        => IMPSApply(removeQCs(f,addCs),ts.map(t => removeQCs(t,addCs)))
      case IMPSIota(v1,s1,p)      => IMPSIota(v1,s1,removeQCs(p,addCs))
      case IMPSIotaP(v1,s1,p)     => IMPSIotaP(v1,s1,removeQCs(p,addCs))
      case IMPSIsDefined(r)       => IMPSIsDefined(removeQCs(r,addCs))
      case IMPSIsDefinedIn(r,s)   => IMPSIsDefinedIn(removeQCs(r,addCs),s)
      case IMPSUndefined(s)       => IMPSUndefined(s)
      case IMPSTotal(f,bs)        => IMPSTotal(removeQCs(f,addCs),bs)
      case IMPSNonVacuous(p)      => IMPSNonVacuous(removeQCs(p,addCs))
      case IMPSQuasiEquals(p,q)   => IMPSQuasiEquals(removeQCs(p,addCs),removeQCs(q,addCs))

      case IMPSQCPred2Indicator(pred_u) =>
      {
        // "lambda(s:[uu,prop], lambda(x:uu, if(s(x), an%individual, ?unit%sort)))"
        val pred  : IMPSMathExp = removeQCs(pred_u,addCs)

        val s_var = (freshVar("s",List(pred)          ::: addCs), IMPSBinaryFunSort(IMPSAtomSort("uu"),IMPSAtomSort("prop")))
        val x_var = (freshVar("x",List(pred,s_var._1) ::: addCs), IMPSAtomSort("uu"))

        val appl    = IMPSApply(s_var._1,List(x_var._1))
        val target  = IMPSIf(appl,IMPSIndividual(),IMPSUndefined(IMPSAtomSort("unitsort")))
        val resolve = IMPSApply(IMPSLambda(List(s_var), IMPSLambda(List(x_var),target)), List(pred))

        resolve
      }

      case IMPSQCSort2Indicator(sort_u) =>
      {
        // "lambda(e:uu, lambda(x:uu, an%individual))"
        val sort  : IMPSMathExp = removeQCs(sort_u,addCs)

        val y_var = (freshVar("x",List(sort)          ::: addCs), IMPSAtomSort("uu"))
        val e_var = (freshVar("e",List(sort,y_var._1) ::: addCs), IMPSAtomSort("uu"))

        val inner   : IMPSMathExp = IMPSLambda(List(y_var),IMPSIndividual())
        val resolve : IMPSMathExp = IMPSApply(IMPSLambda(List(e_var),inner), List(sort))

        resolve
      }

      case IMPSQCIn(e1_u,e2_u) =>
      {
        // "lambda(x:uu,a:sets[uu], #(a(x)))"
        val e1    : IMPSMathExp = removeQCs(e1_u,addCs)
        val e2    : IMPSMathExp = removeQCs(e2_u,List(e1) ::: addCs)

        val x_var = (freshVar("x",List(e1,e2)          ::: addCs), IMPSAtomSort("uu"))
        val a_var = (freshVar("a",List(e1,e2,x_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("uu")))

        val target  : IMPSMathExp = IMPSIsDefined(IMPSApply(a_var._1,List(x_var._1)))
        val resolve : IMPSMathExp = IMPSApply(IMPSLambda(List(x_var,a_var),target),List(e1,e2))

        resolve
      }

      case IMPSQCSubsetEQ(e1_u, e2_u) =>
      {
        // "lambda(a,b:sets[uu], forall(x:uu, (x in a) implies (x in b)))"
        val e1    : IMPSMathExp = removeQCs(e1_u,addCs)
        val e2    : IMPSMathExp = removeQCs(e2_u,List(e1) ::: addCs)

        val a_var = (freshVar("a",List(e1,e2) ::: addCs), IMPSSetSort(IMPSAtomSort("uu")))
        val b_var = (freshVar("b",List(e1,e2,a_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("uu")))
        val x_var = (freshVar("x",List(e1,e2,a_var._1,b_var._1) ::: addCs), IMPSAtomSort("uu"))

        var addConstraints : List[IMPSMathExp]
        = List(e1,e2,a_var._1,b_var._1,x_var._1) ::: addCs

        val in1   : IMPSMathExp = removeQCs(IMPSQCIn(x_var._1,a_var._1), addConstraints)
        addConstraints = addConstraints :+ in1
        val in2   : IMPSMathExp = removeQCs(IMPSQCIn(x_var._1,b_var._1), addConstraints)

        val forall = IMPSForAll(List(x_var), IMPSImplication(in1,in2))
        val lambda = IMPSLambda(List(a_var,b_var),forall)

        IMPSApply(lambda,List(e1,e2))
      }

      case IMPSQCSubset(e1_u, e2_u) =>
      {
        // Not used?
        // "lambda(a,b:sets[uu], (a subseteq b) and not(a = b))"
        val e1    : IMPSMathExp = removeQCs(e1_u,addCs)
        val e2    : IMPSMathExp = removeQCs(e2_u,List(e1) ::: addCs)

        val a_var = (freshVar("a",List(e1,e2) ::: addCs), IMPSSetSort(IMPSAtomSort("uu")))
        val b_var = (freshVar("b",List(e1,e2,a_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("uu")))

        var addConstraints : List[IMPSMathExp]
        = List(e1,e2,a_var._1,b_var._1) ::: addCs

        val subs : IMPSMathExp = IMPSQCSubsetEQ(a_var._1,b_var._1)
        val eq   : IMPSMathExp = removeQCs(IMPSEquals(a_var._1,b_var._1), addConstraints)
        val neg  : IMPSMathExp = IMPSNegation(eq)

        val lambda = IMPSLambda(List(a_var,b_var), IMPSConjunction(List(subs,neg)))
        IMPSApply(lambda,List(e1,e2))
      }

      case IMPSQCEmptyIndicator(srt_u) =>
      {
        // "lambda(e:uu, lambda(x:uu,?unit%sort))"
        val srt : IMPSMathExp = removeQCs(srt_u,addCs)

        val e_var = (freshVar("e",List(srt) ::: addCs), IMPSAtomSort("uu"))
        val x_var = (freshVar("x",List(srt,e_var._1) ::: addCs), IMPSAtomSort("uu"))

        val inner = IMPSLambda(List(x_var), IMPSUndefined(IMPSAtomSort("unitsort")))
        IMPSApply(IMPSLambda(List(e_var), inner),List(srt))
      }

      case IMPSQCNonemptyIndicator(srt_u) =>
      {
        // "lambda(a:sets[uu], forsome(x:uu, x in a))"
        val srt : IMPSMathExp = removeQCs(srt_u,addCs)

        val x_var = (freshVar("x", List(srt) ::: addCs), IMPSAtomSort("uu"))
        val a_var = (freshVar("a", List(srt,x_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("uu")))

        var addConstraints : List[IMPSMathExp] = List(srt,x_var._1,a_var._1) ::: addCs

        val forsome = removeQCs(IMPSForSome(List(x_var),IMPSQCIn(x_var._1,a_var._1)),addConstraints)
        val lambda  = IMPSLambda(List(a_var),forsome)
        IMPSApply(lambda,List(srt))
      }

      case IMPSQCEmptyIndicatorQ(srt_u) =>
      {
        // "lambda(a:sets[uu], forall(x:uu, not(x in a)))"
        val srt : IMPSMathExp = removeQCs(srt_u,addCs)

        val x_var = (freshVar("x", List(srt) ::: addCs), IMPSAtomSort("uu"))
        val a_var = (freshVar("a", List(srt,x_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("uu")))

        var addConstraints : List[IMPSMathExp] = List(srt,x_var._1,a_var._1) ::: addCs

        val forall = removeQCs(IMPSForAll(List(x_var),IMPSNegation(IMPSQCIn(x_var._1,a_var._1))),addConstraints)
        val lambda  = IMPSLambda(List(a_var),forall)
        IMPSApply(lambda,List(srt))
      }

      case IMPSQCComplement(s_u) =>
      {
        // "lambda(s:sets[uu], indic(x:uu, (not #(s(x)))))"
        val srt : IMPSMathExp = removeQCs(s_u,addCs)

        val x_var = (freshVar("x", List(srt) ::: addCs), IMPSAtomSort("uu"))
        val s_var = (freshVar("s", List(srt,x_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("uu")))

        var addConstraints : List[IMPSMathExp] = List(srt,x_var._1,s_var._1) ::: addCs

        val indic  = IMPSLambda(List(x_var),IMPSNegation(IMPSIsDefined(IMPSApply(s_var._1,List(x_var._1)))))
        val lambda = IMPSLambda(List(s_var),indic)
        val pred2indic = removeQCs(IMPSQCPred2Indicator(lambda),addConstraints)
        IMPSApply(pred2indic,List(srt))
      }

      case IMPSQCUnion(u1,u2) =>
      {
        // "lambda(s,t:sets[uu], indic(x:uu, #(s(x)) or #(t(x))))"
        val srt1 : IMPSMathExp = removeQCs(u1,addCs)
        val srt2 : IMPSMathExp = removeQCs(u2,List(srt1) ::: addCs)

        val x_var = (freshVar("x", List(srt1,srt2) ::: addCs), IMPSAtomSort("uu"))
        val s_var = (freshVar("s", List(srt1,srt2,x_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("uu")))
        val t_var = (freshVar("t", List(srt1,srt2,x_var._1,s_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("uu")))

        var addConstraints : List[IMPSMathExp] = List(srt1,srt2,x_var._1,s_var._1,t_var._1) ::: addCs

        val def1   = IMPSIsDefined(IMPSApply(s_var._1,List(x_var._1)))
        val def2   = IMPSIsDefined(IMPSApply(t_var._1,List(x_var._1)))
        val or     = IMPSDisjunction(List(def1,def2))
        val indic  = removeQCs(IMPSQCPred2Indicator(IMPSLambda(List(x_var),or)),addConstraints)
        val lambda = IMPSLambda(List(s_var,t_var),indic)

        IMPSApply(lambda,List(srt1,srt2))
      }

      case IMPSQCIntersection(i1,i2) =>
      {
        // "lambda(s,t:sets[uu], indic(x:uu, #(s(x)) and #(t(x))))"
        val srt1 : IMPSMathExp = removeQCs(i1,addCs)
        val srt2 : IMPSMathExp = removeQCs(i2,List(srt1) ::: addCs)

        val x_var = (freshVar("x", List(srt1,srt2) ::: addCs), IMPSAtomSort("uu"))
        val s_var = (freshVar("s", List(srt1,srt2,x_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("uu")))
        val t_var = (freshVar("t", List(srt1,srt2,x_var._1,s_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("uu")))

        var addConstraints : List[IMPSMathExp] = List(srt1,srt2,x_var._1,s_var._1,t_var._1) ::: addCs

        val def1   = IMPSIsDefined(IMPSApply(s_var._1,List(x_var._1)))
        val def2   = IMPSIsDefined(IMPSApply(t_var._1,List(x_var._1)))
        val and    = IMPSConjunction(List(def1,def2))
        val indic  = removeQCs(IMPSQCPred2Indicator(IMPSLambda(List(x_var),and)),addConstraints)
        val lambda = IMPSLambda(List(s_var,t_var),indic)

        IMPSApply(lambda,List(srt1,srt2))
      }

      case IMPSQCDifference(d1,d2) =>
      {
        // "lambda(s,t:sets[uu], indic(x:uu, #(s(x)) and (not #(t(x)))))"
        val srt1 : IMPSMathExp = removeQCs(d1,addCs)
        val srt2 : IMPSMathExp = removeQCs(d2,List(srt1) ::: addCs)

        val x_var = (freshVar("x", List(srt1,srt2) ::: addCs), IMPSAtomSort("uu"))
        val s_var = (freshVar("s", List(srt1,srt2,x_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("uu")))
        val t_var = (freshVar("t", List(srt1,srt2,x_var._1,s_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("uu")))

        var addConstraints : List[IMPSMathExp] = List(srt1,srt2,x_var._1,s_var._1,t_var._1) ::: addCs

        val def1   = IMPSIsDefined(IMPSApply(s_var._1,List(x_var._1)))
        val def2   = IMPSNegation(IMPSIsDefined(IMPSApply(t_var._1,List(x_var._1))))
        val and    = IMPSConjunction(List(def1,def2))
        val indic  = removeQCs(IMPSQCPred2Indicator(IMPSLambda(List(x_var),and)),addConstraints)
        val lambda = IMPSLambda(List(s_var,t_var),indic)

        IMPSApply(lambda,List(srt1,srt2))
      }

      case IMPSQCSymDifference(sd1,sd2) =>
      {
        // "lambda(s,t:sets[uu],indic(x:uu, (#(s(x)) and (not #(t(x)))) or ((not #(s(x))) and #(t(x)))))"
        val srt1 : IMPSMathExp = removeQCs(sd1,addCs)
        val srt2 : IMPSMathExp = removeQCs(sd2,List(srt1) ::: addCs)

        val x_var = (freshVar("x", List(srt1,srt2) ::: addCs), IMPSAtomSort("uu"))
        val s_var = (freshVar("s", List(srt1,srt2,x_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("uu")))
        val t_var = (freshVar("t", List(srt1,srt2,x_var._1,s_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("uu")))

        var addConstraints : List[IMPSMathExp] = List(srt1,srt2,x_var._1,s_var._1,t_var._1) ::: addCs

        val def1   = IMPSIsDefined(IMPSApply(s_var._1,List(x_var._1)))
        val def2   = IMPSIsDefined(IMPSApply(t_var._1,List(x_var._1)))
        val and1   = IMPSConjunction(List(def1,IMPSNegation(def2)))
        val and2   = IMPSConjunction(List(IMPSNegation(def1),def2))
        val or     = IMPSDisjunction(List(and1,and2))
        val indic  = removeQCs(IMPSQCPred2Indicator(IMPSLambda(List(x_var),or)),addConstraints)
        val lambda = IMPSLambda(List(s_var,t_var),indic)

        IMPSApply(lambda,List(srt1,srt2))
      }

      case IMPSQCDisjoint(dj1,dj2) =>
      {
        // "lambda(s,t:sets[uu], forall(x:uu, not(x in s) or not(x in t)))"
        val srt1 : IMPSMathExp = removeQCs(dj1,addCs)
        val srt2 : IMPSMathExp = removeQCs(dj2,List(srt1) ::: addCs)

        val x_var = (freshVar("x", List(srt1,srt2) ::: addCs), IMPSAtomSort("uu"))
        val s_var = (freshVar("s", List(srt1,srt2,x_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("uu")))
        val t_var = (freshVar("t", List(srt1,srt2,x_var._1,s_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("uu")))

        var addConstraints : List[IMPSMathExp] = List(srt1,srt2,x_var._1,s_var._1,t_var._1) ::: addCs

        val not1   = IMPSNegation(removeQCs(IMPSQCIn(x_var._1,s_var._1), addConstraints))
        addConstraints = addConstraints :+ not1
        val not2   = IMPSNegation(removeQCs(IMPSQCIn(x_var._1,t_var._1), addConstraints))
        val or     = IMPSDisjunction(List(not1,not2))
        val forall = IMPSForAll(List(x_var),or)
        val lambda = IMPSLambda(List(s_var,t_var),forall)

        IMPSApply(lambda, List(srt1,srt2))
      }

      /* Quasi-constructor of DOOM! */
      case IMPSQCPartitionQ(w_u,s_u) =>
      {
        // "lambda(w:sets[sets[uu]],s:sets[uu], forall(u,v:sets[uu],
        //      ((not (u = v)) and (u in w) and (v in w)) implies (u disj v)) and forall(x:uu, (x in s) iff forsome(u:sets[uu], (u in w) and (x in u))))"
        val w_d : IMPSMathExp = removeQCs(w_u,addCs)
        val s_d : IMPSMathExp = removeQCs(s_u,List(w_d) ::: addCs)

        val x_var = (freshVar("x", List(w_d,w_u) ::: addCs), IMPSAtomSort("uu"))
        val u_var = (freshVar("u", List(w_d,w_u,x_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("uu")))
        val o_var = (freshVar("o", List(w_d,w_u,x_var._1,u_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("uu")))
        val v_var = (freshVar("v", List(w_d,w_u,x_var._1,u_var._1,o_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("uu")))
        val s_var = (freshVar("s", List(w_d,w_u,x_var._1,u_var._1,o_var._1,v_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("uu")))
        val w_var = (freshVar("w", List(w_d,w_u,x_var._1,u_var._1,o_var._1,v_var._1,s_var._1) ::: addCs), IMPSSetSort(IMPSSetSort(IMPSAtomSort("uu"))))

        var addConstraints : List[IMPSMathExp] = List(w_d,s_d,x_var._1,u_var._1,o_var._1,v_var._1,s_var._1,w_var._1) ::: addCs

        // LHS

        val c1 = IMPSNegation(IMPSEquals(u_var._1,v_var._1))
        val c2 = removeQCs(IMPSQCIn(u_var._1,w_var._1),addConstraints)
        addConstraints = addConstraints :+ c2
        val c3 = removeQCs(IMPSQCIn(v_var._1,w_var._1),addConstraints)
        addConstraints = addConstraints :+ c3

        val disj = removeQCs(IMPSQCDisjoint(u_var._1,v_var._1),addConstraints)
        addConstraints = addConstraints :+ disj

        val impl         = IMPSImplication(IMPSConjunction(List(c1,c2,c3)), disj)
        val left_forall  = IMPSForAll(List(u_var,v_var),impl)

        // RHS

        val oiw          = removeQCs(IMPSQCIn(o_var._1,w_var._1),addConstraints)
        addConstraints = addConstraints :+ oiw
        val xio          = removeQCs(IMPSQCIn(x_var._1,o_var._1),addConstraints)
        addConstraints = addConstraints :+ xio

        val forsome      = IMPSForSome(List(o_var),IMPSConjunction(List(oiw,xio)))
        val rin          = removeQCs(IMPSQCIn(x_var._1,s_var._1),addConstraints)
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
        val srt : IMPSMathExp = removeQCs(n,addCs)

        val a_var = (freshVar("a",List(srt) ::: addCs), IMPSAtomSort("uu"))
        val x_var = (freshVar("x",List(srt,a_var._1) ::: addCs), IMPSAtomSort("uu"))

        var addConstraints : List[IMPSMathExp] = List(srt,x_var._1,a_var._1) ::: addCs

        val inner  = IMPSLambda(List(x_var), IMPSEquals(x_var._1,a_var._1))
        val indic  = removeQCs(IMPSQCPred2Indicator(inner),addConstraints)
        val lambda = IMPSLambda(List(a_var), indic)

        IMPSApply(lambda,List(srt))
      }

      case IMPSQCBigUnion(f_u) =>
      {
        // "lambda(f:[index,sets[uu]], indic(x:uu, forsome(i:index, #(f(i)(x)))))"
        val g : IMPSMathExp = removeQCs(f_u,addCs)

        val f_var = (freshVar("f",List(g) ::: addCs), IMPSBinaryFunSort(IMPSAtomSort("index"),IMPSSetSort(IMPSAtomSort("uu"))))
        val x_var = (freshVar("x",List(g,f_var._1) ::: addCs), IMPSAtomSort("uu"))
        val i_var = (freshVar("i",List(g,f_var._1,x_var._1) ::: addCs), IMPSAtomSort("index"))

        var addConstraints : List[IMPSMathExp] = List(g,x_var._1,f_var._1,i_var._1) ::: addCs

        val inner  = IMPSForSome(List(i_var),IMPSIsDefined(IMPSApply(f_var._1,List(i_var._1,x_var._1))))
        val indic  = IMPSLambda(List(x_var),inner)
        val pred   = removeQCs(IMPSQCPred2Indicator(indic),addConstraints)
        val lambda = IMPSLambda(List(f_var),pred)

        IMPSApply(lambda,List(g))
      }

      case IMPSQCBigIntersection(f_u) =>
      {
        // "lambda(f:[index,sets[uu]], indic(x:uu, forall(i:index, #(f(i)(x)))))"
        val g : IMPSMathExp = removeQCs(f_u,addCs)

        val f_var = (freshVar("f",List(g) ::: addCs), IMPSBinaryFunSort(IMPSAtomSort("index"),IMPSSetSort(IMPSAtomSort("uu"))))
        val x_var = (freshVar("x",List(g,f_var._1) ::: addCs), IMPSAtomSort("uu"))
        val i_var = (freshVar("i",List(g,f_var._1,x_var._1) ::: addCs), IMPSAtomSort("index"))

        var addConstraints : List[IMPSMathExp] = List(g,x_var._1,f_var._1,i_var._1) ::: addCs

        val inner  = IMPSForAll(List(i_var),IMPSIsDefined(IMPSApply(f_var._1,List(i_var._1,x_var._1))))
        val indic  = IMPSLambda(List(x_var),inner)
        val pred   = removeQCs(IMPSQCPred2Indicator(indic),addConstraints)
        val lambda = IMPSLambda(List(f_var),pred)

        IMPSApply(lambda,List(g))
      }

      case IMPSQCMComposition(g_u,f_u) =>
      {
        // "lambda(f:[ind_2,ind_3],g:[ind_1,ind_2], lambda(x:ind_1, f(g(x))))"
        val g : IMPSMathExp = removeQCs(g_u,addCs)
        val f : IMPSMathExp = removeQCs(f_u,List(g) ::: addCs)

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
        val f : IMPSMathExp = removeQCs(f_u,addCs)

        val x_var = (freshVar("x",List(f) ::: addCs), IMPSAtomSort("ind_1"))
        val f_var = (freshVar("f",List(f,x_var._1) ::: addCs), IMPSBinaryFunSort(IMPSAtomSort("ind_1"),IMPSAtomSort("ind_2")))

        var addConstraints : List[IMPSMathExp] = List(f,f_var._1,x_var._1) ::: addCs

        val inner  = IMPSLambda(List(x_var),IMPSIsDefined(IMPSApply(f_var._1,List(x_var._1))))
        val indic  = removeQCs(IMPSQCPred2Indicator(inner),addConstraints)
        val lambda = IMPSLambda(List(f_var), indic)

        IMPSApply(lambda,List(f))
      }

      case IMPSQCMRange(f_u) =>
      {
        // "lambda(f:[ind_1,ind_2], indic(y:ind_2, forsome(x:ind_1, y=f(x))))"
        val f : IMPSMathExp = removeQCs(f_u,addCs)

        val x_var = (freshVar("x",List(f) ::: addCs), IMPSAtomSort("ind_1"))
        val y_var = (freshVar("y",List(f,x_var._1) ::: addCs), IMPSAtomSort("ind_2"))
        val f_var = (freshVar("f",List(f,x_var._1,y_var._1) ::: addCs), IMPSBinaryFunSort(IMPSAtomSort("ind_1"),IMPSAtomSort("ind_2")))

        var addConstraints : List[IMPSMathExp] = List(f,f_var._1,x_var._1,y_var._1) ::: addCs

        val inner  = IMPSLambda(List(y_var),IMPSForSome(List(x_var),IMPSEquals(y_var._1,IMPSApply(f_var._1,List(x_var._1)))))
        val indic  = removeQCs(IMPSQCPred2Indicator(inner),addConstraints)
        val lambda = IMPSLambda(List(f_var), indic)

        IMPSApply(lambda,List(f))
      }

      case IMPSQCMImage(f_u,s_u) =>
      {
        // "lambda(f:[ind_1,ind_2],a:sets[ind_1], indic(y:ind_2, forsome(x:ind_1, (x in a) and y=f(x))))"
        val f = removeQCs(f_u,addCs)
        val s = removeQCs(s_u,List(f) ::: addCs)

        val x_var = (freshVar("x",List(f,s) ::: addCs),IMPSAtomSort("ind_1"))
        val y_var = (freshVar("y",List(f,s,x_var._1) ::: addCs), IMPSAtomSort("ind_2"))
        val a_var = (freshVar("a",List(f,s,x_var._1,y_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("ind_1")))
        val f_var = (freshVar("f",List(f,s,x_var._1,y_var._1,a_var._1) ::: addCs), IMPSBinaryFunSort(IMPSAtomSort("ind_1"),IMPSAtomSort("ind_2")))

        var addConstraints : List[IMPSMathExp] = List(f,s,x_var._1,y_var._1,a_var._1,f_var._1) ::: addCs

        val in      = removeQCs(IMPSQCIn(x_var._1,a_var._1),addConstraints)
        addConstraints = addConstraints :+ in
        val and     = IMPSConjunction(List(in,IMPSEquals(y_var._1,IMPSApply(f_var._1,List(x_var._1)))))
        val forsome = IMPSForSome(List(x_var),and)
        val indic   = removeQCs(IMPSQCPred2Indicator(IMPSLambda(List(y_var),forsome)),addConstraints)
        val lambda  = IMPSLambda(List(f_var),indic)

        IMPSApply(lambda,List(f,s))
      }

      case IMPSQCMInverseImage(f_u,b_u) =>
      {
        // "lambda(f:[ind_1,ind_2],b:sets[ind_2], b oo f)"
        val f = removeQCs(f_u,addCs)
        val b = removeQCs(b_u,List(f) ::: addCs)

        val b_var = (freshVar("b",List(f,b) ::: addCs),IMPSSetSort(IMPSAtomSort("ind_2")))
        val f_var = (freshVar("f",List(f,b,b_var._1) ::: addCs),IMPSBinaryFunSort(IMPSAtomSort("ind_1"),IMPSAtomSort("ind_2")))

        var addConstraints : List[IMPSMathExp] = List(f_var._1,b_var._1,f,b) ::: addCs

        val comp = removeQCs(IMPSQCMComposition(b_var._1,f_var._1),addConstraints)
        IMPSApply(comp,List(f,b))
      }

      case IMPSQCMInverse(f_u) =>
      {
        // "lambda(f:[ind_1,ind_2],lambda(x:ind_2, iota(y:ind_1, f(y)=x)))"
        val f = removeQCs(f_u,addCs)

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
        val f = removeQCs(f_u,addCs)

        val x_var = (freshVar("x",List(f) ::: addCs),IMPSAtomSort("ind_1"))
        val a_var = (freshVar("a",List(f,x_var._1) ::: addCs),IMPSSetSort(IMPSAtomSort("ind_1")))

        val addConstraints : List[IMPSMathExp] = List(x_var._1,a_var._1,f) ::: addCs

        val in  = removeQCs(IMPSQCIn(x_var._1,a_var._1),addConstraints)
        val ifp = IMPSIf(in, x_var._1, IMPSUndefined(IMPSAtomSort("ind_1")))
        val inn = IMPSLambda(List(x_var),ifp)
        val lam = IMPSLambda(List(a_var),inn)

        IMPSApply(lam,List(f))
      }

      case IMPSQCMRestrict(f_u,a_u) =>
      {
        // "lambda(f:[ind_1,ind_2],a:sets[ind_1],lambda(x:ind_1, if(x in a, f(x), ?ind_2)))"
        val f = removeQCs(f_u,addCs)
        val a = removeQCs(f_u,List(f) ::: addCs)

        val x_var = (freshVar("x",List(f,a) ::: addCs), IMPSAtomSort("ind_1"))
        val a_var = (freshVar("a",List(f,a,x_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("ind_1")))
        val f_var = (freshVar("f",List(f,a,x_var._1,a_var._1) ::: addCs), IMPSBinaryFunSort(IMPSAtomSort("ind_1"),IMPSAtomSort("ind_2")))

        var addConstraints = List(f,a,x_var._1,a_var._1,f_var._1) ::: addCs

        val in  = removeQCs(IMPSQCIn(x_var._1,a_var._1),addConstraints)
        val ifp = IMPSIf(in,IMPSApply(f_var._1,List(x_var._1)),IMPSUndefined(IMPSAtomSort("ind_2")))
        val inn = IMPSLambda(List(x_var),ifp)
        val lam = IMPSLambda(List(f_var,a_var),inn)

        IMPSApply(lam, List(f,a))
      }

      case IMPSQCMRestrict2(f_u,a_u,b_u) =>
      {
        // "lambda(f:[ind_1,ind_2,ind_3],a:sets[ind_1],b:sets[ind_2],lambda(x:ind_1,y:ind_2, if(x in a and y in b, f(x,y), ?ind_3)))"
        val f = removeQCs(f_u,addCs)
        val a = removeQCs(a_u,List(f) ::: addCs)
        val b = removeQCs(b_u,List(f,a) ::: addCs)

        val x_var = (freshVar("x",List(f,a,b) ::: addCs), IMPSAtomSort("ind_1"))
        val y_var = (freshVar("y",List(f,a,b,x_var._1) ::: addCs), IMPSAtomSort("ind_2"))
        val a_var = (freshVar("a",List(f,a,b,x_var._1,y_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("ind_1")))
        val b_var = (freshVar("b",List(f,a,b,x_var._1,y_var._1,a_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("ind_2")))
        val f_var = (freshVar("f",List(f,a,b,x_var._1,y_var._1,a_var._1,b_var._1) ::: addCs),
          IMPSNaryFunSort(List(IMPSAtomSort("ind_1"),IMPSAtomSort("ind_2"), IMPSAtomSort("ind_3"))))

        var addConstraints = List(f,a,b,x_var._1,y_var._1,a_var._1,b_var._1,f_var._1) ::: addCs

        val in1 = removeQCs(IMPSQCIn(x_var._1,a_var._1),addConstraints)
        addConstraints = addConstraints :+ in1
        val in2 = removeQCs(IMPSQCIn(y_var._1,b_var._1),addConstraints)

        val ifp = IMPSIf(IMPSConjunction(List(in1,in2)),IMPSApply(f_var._1,List(x_var._1,y_var._1)),IMPSUndefined(IMPSAtomSort("ind_3")))
        val inn = IMPSLambda(List(x_var,y_var),ifp)
        val lam = IMPSLambda(List(f_var,a_var,b_var),inn)

        IMPSApply(lam, List(f,a,b))
      }

      case IMPSQCMSurjective(f_u) =>
      {
        // "lambda(f:[ind_1,ind_2],forall(x:ind_1, x in dom(f)) and forall(y:ind_2, y in ran(f)))"
        val f = removeQCs(f_u,addCs)

        val x_var = (freshVar("x",List(f) ::: addCs), IMPSAtomSort("ind_1"))
        val y_var = (freshVar("y",List(f,x_var._1) ::: addCs), IMPSAtomSort("ind_2"))
        val f_var = (freshVar("f",List(f,x_var._1,y_var._1) ::: addCs), IMPSBinaryFunSort(IMPSAtomSort("ind_1"),IMPSAtomSort("ind_2")))

        var addConstraints = List(f,x_var._1,y_var._1,f_var._1) ::: addCs

        val dom   = removeQCs(IMPSQCMDomain(f_var._1),addConstraints)
        addConstraints = addConstraints :+ dom
        val in1   = removeQCs(IMPSQCIn(x_var._1,dom),addConstraints)
        addConstraints = addConstraints :+ in1

        val ran   = removeQCs(IMPSQCMRange(f_var._1),addConstraints)
        addConstraints = addConstraints :+ ran
        val in2   = removeQCs(IMPSQCIn(y_var._1,ran),addConstraints)
        addConstraints = addConstraints :+ in2

        val fora1 = IMPSForAll(List(x_var),fora1)
        val fora2 = IMPSForAll(List(y_var),fora2)
        val lambda = IMPSLambda(List(f_var),IMPSConjunction(List(fora1,fora2)))

        IMPSApply(lambda,List(f))
      }

      case IMPSQCMInjective(f_u) =>
      {
        // "lambda(f:[ind_1,ind_2],forall(x_1,x_2:ind_1, f(x_1)=f(x_2) implies x_1=x_2))"
        val f = removeQCs(f_u,addCs)

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
        val f = removeQCs(f_u,addCs)
        val f_var = (freshVar("f", List(f) ::: addCs), IMPSBinaryFunSort(IMPSAtomSort("ind_1"),IMPSAtomSort("ind_2")))

        var addConstraints = List(f,f_var._1) ::: addCs

        val sur = removeQCs(IMPSQCMSurjective(f_var._1),addConstraints)
        addConstraints = addConstraints :+ sur
        val inj = removeQCs(IMPSQCMInjective(f_var._1),addConstraints)

        val and = IMPSConjunction(List(sur,inj))
        val lam = IMPSLambda(List(f_var),and)

        IMPSApply(lam,List(f))
      }

      case IMPSQCMSurjectiveOn(f_u,a_u,b_u) =>
      {
        // "lambda(f:[ind_1,ind_2],a:sets[ind_1],b:sets[ind_2],dom(f)=a and ran(f)=b)"
        val f = removeQCs(f_u,addCs)
        val a = removeQCs(a_u,List(f) ::: addCs)
        val b = removeQCs(b_u,List(f,a) ::: addCs)

        val a_var = (freshVar("a",List(f,a,b) ::: addCs), IMPSSetSort(IMPSAtomSort("ind_1")))
        val b_var = (freshVar("b",List(f,a,b,a_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("ind_2")))
        val f_var = (freshVar("f",List(f,a,b,a_var._1,b_var._1) ::: addCs), IMPSBinaryFunSort(IMPSAtomSort("ind_1"),IMPSAtomSort("ind_2")))

        var addConstraints = List(f,a,b,f_var._1,a_var._1,b_var._1) ::: addCs

        val dom = removeQCs(IMPSQCMDomain(f_var._1),addConstraints)
        addConstraints = addConstraints :+ dom
        val ran = removeQCs(IMPSQCMRange(f_var._1),addConstraints)

        val eq1 = IMPSEquals(dom,a_var._1)
        val eq2 = IMPSEquals(ran,b_var._1)

        val lambda = IMPSLambda(List(f_var,a_var,b_var), IMPSConjunction(List(eq1,eq2)))

        IMPSApply(lambda,List(f,a,b))
      }

      case IMPSQCMInjectiveOn(f_u,a_u) =>
      {
        // "lambda(f:[ind_1,ind_2],a:sets[ind_1],forall(x_1,x_2:ind_1,((x_1 in a) and (x_2 in a) and f(x_1)=f(x_2)) implies x_1=x_2))"
        val f = removeQCs(f_u,addCs)
        val a = removeQCs(a_u,List(f) ::: addCs)

        val x1_var = (freshVar("x_1", List(f,a) ::: addCs), IMPSAtomSort("ind_1"))
        val x2_var = (freshVar("x_2", List(f,a,x1_var._1) ::: addCs), IMPSAtomSort("ind_1"))
        val  a_var = (freshVar("a", List(f,a,x1_var._1,x2_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("ind_1")))
        val  f_var = (freshVar("f", List(f,a,x1_var._1,x2_var._1,a_var._1)), IMPSBinaryFunSort(IMPSAtomSort("ind_1"),IMPSAtomSort("ind_2")))

        var addConstraints = List(f,a,f_var._1,a_var._1,x1_var._1,x2_var._1) ::: addCs

        val a1 = removeQCs(IMPSQCIn(x1_var._1,a_var._1),addConstraints)
        addConstraints = addConstraints :+ a1
        val a2 = removeQCs(IMPSQCIn(x2_var._1,a_var._1),addConstraints)
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
        val f = removeQCs(f_u,addCs)
        val a = removeQCs(a_u,List(f) ::: addCs)
        val b = removeQCs(b_u,List(f,a) ::: addCs)

        val a_var = (freshVar("a",List(f,a,b) ::: addCs), IMPSSetSort(IMPSAtomSort("ind_1")))
        val b_var = (freshVar("a",List(f,a,b,a_var._1) ::: addCs), IMPSSetSort(IMPSAtomSort("ind_2")))
        val f_var = (freshVar("f",List(f,a,b,a_var._1,b_var._1) ::: addCs), IMPSBinaryFunSort(IMPSAtomSort("ind_1"),IMPSAtomSort("ind_2")))

        var addConstraints = List(f,a,b,f_var._1,a_var._1,b_var._1) ::: addCs

        val sur = removeQCs(IMPSQCMSurjectiveOn(f_var._1,a_var._1,b_var._1),addConstraints)
        addConstraints = addConstraints :+ sur
        val inj = removeQCs(IMPSQCMInjectiveOn(f_var._1,a_var._1),addConstraints)

        val lam = IMPSLambda(List(f_var,a_var,b_var), IMPSConjunction(List(sur,inj)))

        IMPSApply(lam,List(f,a,b))
      }
    }
  }
}
