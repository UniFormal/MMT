/* 

abstract class Feature {
   val typeConstructor: Path 
   val introSymbol: Path 
   val introIsInjective: Boolean 

   def getConstructor() : Path = {
      return typeConstructor
   }
   
   // don't repeat vars in the context when doing extensionality; can I enforce this at this stage?
   def extensionalityRule(t: TypingJudgement): Term
   
   def computationRules(tm: Term): Term
   
   def introRule(t: TypingJudgement): List[TypingJudgement] 
}
   
class UnificationProblem(report: Report) {
   
   def log(msg : => String) = report("generic unification", msg)
   
   var equation: EqualityJudgement = _
   var metaContext: Context = Context()
   var solution: Substitution = Substitution()
   
   // if true, it doesn't mean necessarily that equation has sols; look at metavars
   def unifyMMT(equation: EqualityJudgement) : Boolean = {
      
      println()
      println("Unification step in MMT")
      equation.toTerms() match {
         case (OMV(name),tm) => 
            if ( metaContext.isDeclared(name) && UnificationOperations.occurCheck(OMV(name), tm) ) {
               val sub = Sub(name, tm)
               var newEquation = UnificationOperations.replace(equation, sub)
               solution = solution  ++ sub
               unifyMMT(newEquation)
            }
            else {
               // if name is not in the metaContext, we are not interested in it. Also, it is a bound var, so we
               // cannot substitute it
               // if name is in the term, again the problem doesn't have solutions
               return false
            }
         
         // treated exactly the same as the previous case
         case (tm, OMV(name)) => 
            if ( metaContext.isDeclared(name) && UnificationOperations.occurCheck(OMV(name), tm) ) {
               val sub = Sub(name, tm)
               var newEquation = UnificationOperations.replace(equation, sub)
               solution = solution  ++ sub
               unifyMMT(newEquation)
            }
            else {
               // if name is not in the metaContext, then it is a bound variable so it cannot be substituted
               // if name is in the term, again the problem doesn't have solutions
               return false
            }
            
         case (OMID(name1), OMID(name2)) =>
            println("equality of constants")
            println()
            // smart definition expansion
            if (name1 == name2)
               return true
            else
               return false
            
         case (OMID(name), tm) => return true
            /* 
            UnificationOperations.lookupDef(name) match {
               case Some(definiens) => unifyMMT(new EqualityJudgement(context, definiens, term, T))
               case None => return false
            }
            */
         
         case (tm, OMID(name)) => return true
             // the same as above
         
         case (OMA(x, listx), OMA(y, listy)) =>
            println("OMA")
            var equations = UnificationOperations.decApp(equation)
            equations match {
               case Some(listEquations) =>   return (listEquations.map(unifyMMT)).foldLeft(true)((b,a) => b && a)
               case None => return false // think more about this case
            }
              
         case (OMBIND(b1, context1, tm1), OMBIND(b2, context2, tm2)) =>  
            if ( b1 != b2)
               return false
            else {
               UnificationOperations.decBinder(context1, context2) match {
                  case Some((subst,listEquations)) => return (listEquations.map(unifyMMT)).foldLeft(true)((b,a) => b && a) &&
                                             unifyMMT(new EqualityJudgement(context1,tm1,tm2^subst,None))
                  // for the unification problem related to tm1, tm2, the context is relevant only for the variable names
                  // context1 = context2 after substitution, so it doesn't matter which one we put
                  case None => return false
                        
               }
            }
            
         case _ => return true   
      }
   }
   
   def getSolution() : Option[Substitution]= {
      // check if we have found two values for the same variable - is this possible?
      // check if we have found values for all metavariables
   
      var listVarsSubst = List[Content]()
      for( s <- solution.components)
          listVarsSubst = (s.components).head :: listVarsSubst
      /*
      for (metavar <- metaContext)
         listVarSubst.contains(metavar)
      */
      return Some(solution)
   }
}


class UnificationProblemTT(report: Report) extends UnificationProblem(report) {
  
   override def log(msg : => String) = report("tt unification", msg)
   
   var typeTheory: List[Feature] = Nil
   
   def unifyTT(equation: EqualityJudgement)(implicit lib: Lookup) : Boolean = {
   
      println()
      println("Unification step in TT")
      /*
      var T  = equation.typeAtEquality
      var tm1 = equation.term1
      var tm2 = equation.term2
      var context = equation.context
      println(tm1)
      println(tm2)
      println(T)
      */
     
      equation.headOfType() match {
         case Some(h) => 
            println("head of type: " + h)
            
            // check to see if type is simple or composed
            var sw = false
            
            for(f <- typeTheory) {
               if( h == f.getConstructor()) {
                  // we are at a composed type
                  sw = true
                 
                  var t1 = equation.toTypingFirst()
                  var t2 = equation.toTypingSecond()
                  var tm1Eta = f.extensionalityRule(t1)
                  var tm2Eta = f.extensionalityRule(t2)
                  
                  println("extensionality application; context and type don't change")
                  println(tm1Eta.toString())
                  println(tm2Eta.toString())
                  
                  // tm1Eta should always be different from t1 and the same for tm2Eta
                  //(tm1Eta == tm1 && tm2Eta == tm2 && f.introIsInjective) 
                  // how can I enforce that in extensionality rule, in the implementation
                  
                  if (f.introIsInjective) {
                    
                     var listOfComp1 = f.introRule(t1.replaceTerm(tm1Eta))
                     var listOfComp2 = f.introRule(t2.replaceTerm(tm2Eta))
                        
                     if (listOfComp1.length != listOfComp2.length)
                        // think better, you are in a type theory
                        return false   
                     else {
                        println("introduction rule")
                        for ((ta,tb) <- listOfComp1.zip(listOfComp2)) {
                           println("first term")
                           println("-context " + ta.getContext().toString())
                           println("-term " + ta.getTerm().toString())
                           println("-type " + ta.getType().toString())
                           println("second term")
                           println("-context " + tb.getContext().toString())
                           println("-term" + tb.getTerm().toString())
                           println("-type" + tb.getType().toString())
                           println()
                           //unifyTT(new EqualityJudgement(a.getContext(),a.getTerm(),b.getTerm(),a.getType()))
                           
                           
                           UnificationOperations.decBinder(ta.getContext(),tb.getContext()) match {
                              case Some((substContext,listEquations)) =>
                                /*
                                 var h = listEquations.head
                                 println((h.getContext()).toString())
                                 println((h.getFirstTerm()).toString())
                                 println((h.getSecondTerm()).toString())
                                 println((h.getType()).toString())
                                 println(substContext.toString())
                                 */
                                 (ta.getType(),tb.getType()) match {
                                    case (None, Some(tbType)) => return false
                                    case (Some(taType), None) => return false
                                    case (Some(taType), Some(tbType)) =>
                                       // formulate a new unification problem for types
                                       /*
                                       println()
                                       println(taType.toString())
                                       println(tbType.toString())
                                       println()
                                       println()
                                       */
                                       println("unify contexts")
                                       println()
                                       var v = (listEquations.map(unifyMMT)).foldLeft(true)((b,a) => b && a)
                                       if (v) {
                                          var problem = new UnificationProblem(report)
                                          println("unify types")
                                          problem.unifyMMT(new EqualityJudgement(ta.getContext(), taType,
                                                               tbType^substContext, None))
                                          problem.getSolution() match {
                                             case None => return false
                                             case Some(substType) =>
                                                println("unify terms")
                                                v = v && unifyTT( new EqualityJudgement( ta.getContext(),
                                                                            ta.getTerm(),
                                                                            tb.getTerm()^substContext, 
                                                                            Some(tbType^substType)))
                                                return v
                                          }
                                       }
                                       else
                                          return false
                                    case (None, None) =>
                                       println("unify contexts")
                                       var v = (listEquations.map(unifyMMT)).foldLeft(true)((b,a) => b && a)
                                       println("no type for equation")
                                       println("unify terms")
                                       v = v && unifyMMT(new EqualityJudgement(ta.getContext(),
                                                                     ta.getTerm(),
                                                                     tb.getTerm()^substContext, 
                                                                     None))
                                 }
                                 
                                 
                              case None => return false
                           }
                           
                        }
                     }
                  }
                  else {
                     //var newEquation = equation.replaceTerms(tm1Eta,tm2Eta)
                     //return unifyTT(newEquation)
                     return unifyMMT(equation) // since symbol is not injective, we cannot really do but MMT unification
                  }
               }
            }
            
            if (!sw) {
               // we are at a base type
               println("beta")
               for (f <- typeTheory)  {
                  var tm1 = equation.getFirstTerm()
                  var tm2 = equation.getSecondTerm()
                  var tm1Beta = f.computationRules(tm1)
                  var tm2Beta = f.computationRules(tm2)
                  println(tm1Beta.toString())
                  println(tm2Beta.toString())
                  if (tm1 != tm1Beta || tm2 != tm2Beta)
                     return unifyTT(equation.replaceTerms(tm1Beta,tm2Beta))   
                  else 
                     return unifyMMT(equation)  
               }
            }

            return true 
         
         // should it throw an error b/c we are in the type theory case?
         case None => return unifyMMT(equation) // if the head doesn't exist, then we don't have a type => MMT unif
      
      }  
   }
}

*/