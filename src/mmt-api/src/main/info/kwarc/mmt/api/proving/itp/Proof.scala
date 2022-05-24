package info.kwarc.mmt.api.proving.itp

import info.kwarc.mmt.api.LocalName
import info.kwarc.mmt.api.checking.Solver
import info.kwarc.mmt.api.objects._

import scala.collection.mutable.{ListBuffer, Map => mmap, Set => MSet}

class Proof(t : Term, globalContext : Context , igoal : Term ) {
  val currentState : ListBuffer[Goal] = ListBuffer(Goal(t,  Context(), LocalName("" , "goal")))
  /**
    * not actively used. Meant to trace intergoal dependencies (for example if let introduces new unkowns)
    */
  val goalDependencies : mmap[LocalName , ListBuffer[(LocalName,LocalName)]] = mmap.empty
  val solvedVariables : MSet[LocalName] = MSet()
  val gctx : Context = globalContext
  val initGoal : Term = igoal
  var goalcounter : Int = 0


  /**
    * the proof term that represents the proof. This proof term represents a hole as application of free variables to an unknown
    */
  var proofTerm : Term = null
  /**
    * an alternative representation of [[proofTerm]] that removes the free vars
    */
  var proofTermAlt : Term = null

  /**
    *
    * @return new unique goal name
    */
  def getNewGoalName() : LocalName =  {
    goalcounter += 1
    LocalName(""  , "goal" , goalcounter.toString )
  }

  /**
    * convenience method for updating the unknowns in the [[proofTerm]]
    * @param s
    */
  def updateProofTerm(s : Solver) = {
    proofTerm = s.substituteSolution(proofTerm)
  }

  /**
    * like [[updateProofTerm]] but as a terverser
    * @param ln
    * @param t
    */
  class UpdateGoalTraverser(ln : LocalName, t : Term) extends StatelessTraverser {
    override def traverse(tt: Term)(implicit con: Context, state: State): Term =  tt match {
      case OMV(lnn) if lnn == ln => t
      case _ => Traverser(this , tt)
    }
  }

  /**
    *
    * @param ls
    * @param t
    */
  def updateGoalProofTerm(ls : LocalName , t : Term) = {
    val tr = new UpdateGoalTraverser(ls, t)
    val res = tr.traverse(proofTermAlt)(Context() , ())
    proofTermAlt = res
  }


  /**
    *
    * @param s
    * @return
    */
  def proofStateToString(s : Solver) : String ={
    if(currentState.isEmpty) {
      return "all goals solved"
    }
    val res = ListBuffer[String]()
    res.append( currentState.head.printGoal(s))
    for(i <- currentState.tail){
      res.append(s.presentObj(i.g))
    }
    res.mkString("\n")
  }

  /**
    * convenience function to get the first goal
    * @return the first goal
    */
  def getCurrentGoal : Goal = currentState.head

  /**
    *
    * @return the conclusion/goal of the first goal
    */
  def getCurrentConc : Term = currentState.head.g

  def getCurrCtx : Context = currentState.head.ctx

  /**
    * update the first goal
    * @param g the updated value
    */
  def update(g : Goal) = {val tmp = currentState.remove(0); currentState.insert(0,g) }


  def getCurrCtxMp : Context = getCurrCtx ++ globalContext


  /**
    * generates a copy/clone of the proof
    * @return copy/clone of the current proof state
    */
  def copy : Proof = {
    val p  = new Proof(t , globalContext, initGoal)
    p.currentState.remove(0)
    for (g <- currentState){
      val newg = g.copy
      p.currentState.append(newg)
    }
    p.proofTerm = proofTerm
    p.proofTermAlt = proofTermAlt
    p.goalcounter = goalcounter
    p.solvedVariables ++= solvedVariables.toList
    p.goalDependencies ++= goalDependencies
    p
  }

  /**
    * sometimes an unknown gets solved but its solution will not necessarily be subsituted in the whole proof, this function, which gets called after
    * a tactic has been executed, takes all new solutions and substitutes them in the whole proof
    * @param s
    * @return
    */
  def updateUnknowns(s : Solver): Msg = {
    // get all solved variables. This will get all variables solved so far in this proof
    val sols = s.getSolvedVariables
    // get all unknowns
    val ps = s.getPartialSolution
    var nrem = 0
    for(i <- currentState.indices){
      val tmp = currentState(i - nrem)
      if (sols.contains(tmp.ukname)) {
        val vd = ps.get(tmp.ukname)
        val fv = vd.df.get.freeVars
        if (!fv.forall(p => tmp.ctx.exists((x : VarDecl) => x.name == p))) {
          return HasError(tmp.ukname.toString + " was solved illegally")
        }
        currentState.remove(i - nrem)
        updateGoalProofTerm(vd.name , vd.df.get)
        nrem += 1
      }
    }

    for (i <- ps){
      if (i.tp.isEmpty) return HasError("could not infer type of " + i.name)
    }

    val newsols = s.getSolvedVariables.filter(p => ! solvedVariables.contains(p))
    val newsols0 = newsols.map(x => s.getPartialSolution.get(x))
    val subs = Substitution(newsols0.map(x => Sub(x.name , x.df.get)) : _*)
    for(i <- currentState.indices){
      val Goal(g, gctx , gukname) = currentState(i)
      val newg = Goal(g.substitute(subs)(PlainSubstitutionApplier) , gctx.map(x => x.copy(tp = Some(x.tp.get.substitute(subs)(PlainSubstitutionApplier)))) , gukname  )
      currentState.update(i , newg)
    }


    solvedVariables ++= newsols
    NoMsg()
  }

  /**
    * currently not really used
    * @param ctx
    * @param gname
    * @param hname
    */
  def addukdeps( ctx : Context , gname : LocalName , hname : LocalName): Unit = {
    ctx.foreach(v => {
      lazy val deps = goalDependencies(v.name)
      if (goalDependencies.contains(v.name)){
        deps.insert(0,(gname , hname))
      }else {
        goalDependencies(v.name) = ListBuffer((gname , hname))
      }
    })
  }
}