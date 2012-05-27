package info.kwarc.mmt.lf
import info.kwarc.mmt.api._
import objects._
import Conversions._

object DepTypeConstructor extends Feature {
	val typeConstructor : Path = LF.lftheory ? "Pi"
	val introSymbol : Path = LF.lftheory ? "lambda"
	val introIsInjective : Boolean = true
	
	def extensionalityRule(t: TypingJudgement): Term = {
		// x is a generic variable
		t.getType() match {
		  	case Some(Pi(x, a, b)) => return Lambda(x,a,Apply(t.getTerm(),x))
		  	case _ => return t.getTerm()
		}
	}
	
	def computationRules(tm: Term): Term = {
		tm match {
		  	case ( Apply(Lambda(x, a, tm1), tm2) ) => 
		  	  	var s = Sub(x,tm2)
		  	  	return tm1^s
		  	case Apply(tm1, tm2) => Apply(computationRules(tm1), tm2)
		  	case _ => tm
		}
	}

	
	def introRule(t: TypingJudgement) : List[TypingJudgement] = {
		// x is a generic variable
		t.getType() match {
		  	case Some(Pi(x, a, b)) => 
		  	  	var newContext = t.getContext() ++ VarDecl("x", Some(a), None)
		  	  	var newJudgement = new TypingJudgement(newContext, Apply(t.getTerm(),x), Some(b))
		  	  	return List(newJudgement)
		  	case _ => return List(t)
		}
	}
}


object Test {
	val controller = new frontend.Controller
	def log(msg : => String) = controller.report("user", msg)

	def main(args : Array[String]) : Unit = {
		controller.handle(frontend.ExecFile(new java.io.File("test-init.mmt")))
		
		//val t = new modules.DeclaredTheory(LF.lfbase, LocalPath(List("T")), Some(LF.lftheory))
		//val a = new symbols.Constant(OMID(t.path), LocalName("a"), Some(tp), None, None, None)
		//val atoa = Arrow(OMS(a.path), OMS(a.path))
		//val f = new symbols.Constant(OMID(t.path), LocalName("f"), Some(atoa), None, None, None)
		//controller.add(f)
		
		/*
		//PROBLEM 1:
		
		val tp = LF.ktype
		val a = new symbols.Constant(OMID(LF.lftheory), LocalName("a"), Some(tp), None, None, None)
		controller.add(a)
		
		val problem = new UnificationProblemTT(controller.report)
		
		val t1 = OMV("y") //OMID(f.path)
		val t2 = OMID(a.path) //OMID(f.path)
		val equation = new EqualityJudgement(Context(),t1,t2,Some(tp))
		
		problem.typeTheory::= DepTypeConstructor
		problem.equation = equation
		problem.metaContext = problem.metaContext ++ List(VarDecl("y", Some(tp), None)) //++ VarDecl("x", Some(OMV("y")), None)
		
		println("Problem")
		println("-context:")
		println(equation.context)
		println("-terms:")
		println(equation.term1.toString())
		println(equation.term2.toString())
		println("-type:")
		println(tp.toString())
		println("-metavars:")
		println(problem.metaContext.toString())
		println()
		
		val sol = problem.unifyTT(equation)(controller.globalLookup)
		log("yes/no: " + sol.toString)
		log("solution: " + problem.solution.toString)
		*/
		
		/*
		PROBLEM 2:
		*/
		
		val problem = new UnificationProblemTT(controller.report)
		
		val tp = LF.ktype
		val A = new symbols.Constant(OMID(LF.lftheory), LocalName("A"), Some(tp), None, None, None)
		controller.add(A)
		
		val depA = Pi("x", OMS(A.path), OMS(A.path)) 
		
		val c = new symbols.Constant(OMID(LF.lftheory), LocalName("c"), Some(LF.constant("A")), None, None, None)
		controller.add(c)
		
		val t1 = Lambda("z", OMS(A.path), OMV("y")) 
		val t2 = Lambda("t", OMS(A.path), OMS(c.path))
		val equation = new EqualityJudgement(Context(),t1,t2,Some(depA))
		
		problem.typeTheory::= DepTypeConstructor
		problem.equation = equation
		problem.metaContext = problem.metaContext ++ List(VarDecl("y", Some(tp), None)) //++ VarDecl("x", Some(OMV("y")), None)
			
		println("Problem")
		println("-context:")
		println(equation.context)
		println("-terms:")
		println(equation.term1.toString())
		println(equation.term2.toString())
		println("-type:")
		println(depA.toString())
		println("-metavars:")
		println(problem.metaContext.toString())
		println()
		
		val sol = problem.unifyTT(equation)(controller.globalLookup)
		log("yes/no: " + sol.toString)
		log("solution: " + problem.solution.toString)
		
		
		/*
		//PROBLEM 3:
		
		val problem = new UnificationProblemTT(controller.report)
		
		val tp = LF.ktype
		val A = new symbols.Constant(OMID(LF.lftheory), LocalName("A"), Some(tp), None, None, None)
		controller.add(A)
		
		val depA = Pi("x", OMS(A.path), OMS(A.path)) 
		
		val c = new symbols.Constant(OMID(LF.lftheory), LocalName("c"), Some(LF.constant("A")), None, None, None)
		controller.add(c)
		
		val t1 = Apply(Lambda("x", OMS(A.path), OMV("x")), "y") 
		val t2 = OMID(c.path)
		val equation = new EqualityJudgement(Context(),t1,t2,Some(OMID(A.path)))
		
		problem.typeTheory::= DepTypeConstructor
		problem.equation = equation
		problem.metaContext = problem.metaContext ++ List(VarDecl("y", Some(OMID(A.path)), None)) //++ VarDecl("x", Some(OMV("y")), None)
			
		println("Problem")
		println("-context:")
		println(equation.context)
		println("-terms:")
		println(equation.term1.toString())
		println(equation.term2.toString())
		println("-type:")
		println(A.toString())
		println("-metavars:")
		println(problem.metaContext.toString())
		println()
		
		val sol = problem.unifyTT(equation)(controller.globalLookup)
		log("yes/no: " + sol.toString)
		log("solution: " + problem.solution.toString)
		*/
		
		/*
		// PROBLEM 4: 
		
		val problem = new UnificationProblemTT(controller.report)
		
		val tp = LF.ktype
		val A = new symbols.Constant(OMID(LF.lftheory), LocalName("A"), Some(tp), None, None, None)
		controller.add(A)
		
		val depA = Pi("x", OMS(A.path), OMS(A.path)) 
		
		val c = new symbols.Constant(OMID(LF.lftheory), LocalName("c"), Some(LF.constant("A")), None, None, None)
		controller.add(c)
		
		val t1 = Apply(Lambda("x", OMS(A.path), OMV("x")), "y") 
		val t2 = Lambda("x", OMS(A.path), Apply(OMV("x"),OMV("y")))
		val equation = new EqualityJudgement(Context(),t1,t2,Some(OMID(A.path)))
		
		problem.typeTheory::= DepTypeConstructor
		problem.equation = equation
		problem.metaContext = problem.metaContext ++ List(VarDecl("y", Some(OMID(A.path)), None)) //++ VarDecl("x", Some(OMV("y")), None)
			
		println("Problem")
		println("-context:")
		println(equation.context)
		println("-terms:")
		println(equation.term1.toString())
		println(equation.term2.toString())
		println("-type:")
		println(A.toString())
		println("-metavars:")
		println(problem.metaContext.toString())
		println()
		
		val sol = problem.unifyTT(equation)(controller.globalLookup)
		log("yes/no: " + sol.toString)
		log("solution: " + problem.solution.toString)
		*/
		
		/*
		// PROBLEM 5:
		
		
		val problem = new UnificationProblemTT(controller.report)
		
		val tp = LF.ktype
		val A = new symbols.Constant(OMID(LF.lftheory), LocalName("A"), Some(tp), None, None, None)
		controller.add(A)
		
		val depA = Pi("y", OMS(A.path), OMS(A.path)) 
		
		val c = new symbols.Constant(OMID(LF.lftheory), LocalName("c"), Some(LF.constant("A")), None, None, None)
		controller.add(c)
		
		val t1 = Lambda("x", depA, Apply(OMV("x"),OMV("z"))) 
		val t2 = Lambda("x", depA, OMID(c.path))
		val equation = new EqualityJudgement(Context(),t1,t2,Some(Pi("t",OMS(A.path),depA)))
		
		problem.typeTheory::= DepTypeConstructor
		problem.equation = equation
		problem.metaContext = problem.metaContext ++ List(VarDecl("z", Some(OMID(A.path)), None)) //++ VarDecl("x", Some(OMV("y")), None)
			
		println("Problem")
		println("-context:")
		println(equation.context)
		println("-terms:")
		println(equation.term1.toString())
		println(equation.term2.toString())
		println("-type:")
		println(A.toString())
		println("-metavars:")
		println(problem.metaContext.toString())
		println()
		
		val sol = problem.unifyTT(equation)(controller.globalLookup)
		log("yes/no: " + sol.toString)
		log("solution: " + problem.solution.toString)
		*/
		

	}
}
 