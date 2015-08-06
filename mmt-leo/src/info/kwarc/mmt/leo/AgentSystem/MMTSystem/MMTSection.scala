package info.kwarc.mmt.leo.AgentSystem.MMTSystem

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.{Active, modules}
import info.kwarc.mmt.api.objects.{OMPMOD, ComplexTheory, Term}
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.leo.AgentSystem.{Change, Section}


/**
 * This trait capsules the formulas responsible for the formula manipulation of the
 * blackboard.
 *
 */
class GoalSection(blackboard:MMTBlackboard, goal:Goal)(implicit c: Controller,oLP:String) extends Section(blackboard) {
  override def logPrefix =oLP+"#GoalSection"

  /** this type of section only stores data which is a subtype of the AndOr tree type*/
  type ObjectType=Goal
  type PTType=ObjectType //Meaningful alias for object type

  var data:PTType = goal
  var changes: List[Change[_]] = List(new Change(this,goal,List("ADD")))


  def passiveOp(newData:Alternative,flag:String) ={
    handleChange(new Change(this,newData,List(flag)))
  }
  def passiveOp(newData:Goal,flag:String) ={
    handleChange(new Change(this,newData,List(flag)))
  }
  def passiveAdd(newData:Alternative) = passiveOp(newData,"ADD")
  def passiveAdd(newData:Goal) = passiveOp(newData,"ADD")
  def passiveDel(newData:Alternative) = passiveOp(newData,"DEL")
  def passiveDel(newData:Goal) = passiveOp(newData,"DEL")
  def passiveChange(newData:Alternative) = passiveOp(newData,"CHANGE")
  def passiveChange(newData:Goal) = passiveOp(newData,"CHANGE")


  /** statefully changes g to a simpler goal */
  def simplifyGoal(g: Goal) {
    g.setConc(blackboard.c.simplifier(g.conc, g.fullContext, blackboard.rules), blackboard.facts)
    blackboard.goalSection.passiveChange(g)
  }

  /**
   * applies one tactic to a goal and expands the resulting subgoals
   * @return true if the tactic made any progress
   */
  def applyAndExpand(at: ApplicableTactic, g: Goal): Boolean = {
    val alt = at.apply().getOrElse(return false)
    // simplify the new goal
    alt.subgoals.foreach { sg =>
      sg.parent = Some(g) // need to set this before working with the goal
      simplifyGoal(sg)
    }

    // avoid cycles/redundancy: skip alternatives with subgoals that we already try to solve
    val path = g.path
    val alreadyOnPath = alt.subgoals.exists { sg =>
      // TODO stronger equality
      path.exists { ag => (ag.context hasheq sg.context) && (ag.conc hasheq sg.conc) }
    }
    if (alreadyOnPath )
      return false

    // add the alternative to the proof tree and expand the subgoals
    g.addAlternative(alt, Some(this))
    alt.subgoals.foreach(blackboard.terms.addVarAtoms) //TODO work into goal addition

    log("************************* " + at.label + " at X **************************")
    log("\n" + data.presentHtml(0)(blackboard.presentObj, Some(g), Some(alt)))
    if (!g.isSolved) {
      // recursively process subgoals
      alt.subgoals.foreach { sg => expand(sg) }
    }
    true
  }


  /** exhaustively applies invertible tactics to a goal */
  def expand(g: Goal) {
    g.setExpansionTactics(blackboard, blackboard.invertibleBackward, blackboard.invertibleForward)
    g.getNextExpansion match {
      case Some(at) =>
        // apply the next invertible tactic, if any
        val applicable = applyAndExpand(at, g)
        if (!applicable)
        // at not applicable, try next tactic
          expand(g)
      case None =>
        g.setSearchTactics(blackboard, blackboard.searchBackward)
    }
  }


  def initialize():Unit={
    log("Initializing Goal by expanding")
    expand(data)
  }

}


class FactSection(blackboard:MMTBlackboard,shapeDepth: Int)(implicit c: Controller,oLP:String) extends Section(blackboard) {
  override def logPrefix =oLP+"#FactSection"

  type ObjectType = Facts
  var data = new Facts(blackboard:MMTBlackboard,shapeDepth: Int)
  var changes: List[Change[_]] = Nil

  var isInitialized=false

  def initialize():Unit = {
    if (!isInitialized){
      val imports = c.library.visibleDirect(ComplexTheory(blackboard.goal.context))
      imports.foreach {
        case OMPMOD(p, _) =>
          c.globalLookup.getO(p) match {
            case Some(t: modules.DeclaredTheory) =>
              t.getDeclarations.foreach {
                case c: Constant if c.status == Active => c.tp.foreach { tp =>
                  val a = Atom(c.toTerm, tp, c.rl)
                  data.addConstantAtom(a)
                }
                case _ =>
              }
            case _ =>
          }
        case _ =>
      }
      log("Initialized facts are:  \n"+data)
      isInitialized=true
    }
  }
  
  def passiveOp(fact:Fact,flag:String) = handleChange(new Change(this, fact, List(flag)))//TODO add specific fact pointers
  def passiveAdd(fact:Fact) = passiveOp(fact,"ADD")
}

class TermSection(blackboard:MMTBlackboard)(implicit c: Controller,oLP:String) extends Section(blackboard) {
  override def logPrefix =oLP+"#TermSection"
  type ObjectType = Terms
  var data = new Terms(blackboard:MMTBlackboard)
  var changes: List[Change[_]] = Nil

  def initialize() ={
    if (!blackboard.factSection.isInitialized){blackboard.factSection.initialize()}
    data.initializeTerms(blackboard.facts)
  }


  def passiveOp(flag:String) = handleChange(new Change(this, true, List(flag)))//TODO add specific Term pointers
  def passiveAdd() = passiveOp("ADD")
}

class TransitivitySection(blackboard:MMTBlackboard)(implicit c: Controller,oLP:String) extends Section(blackboard) {
  override def logPrefix =oLP+"#TransitivitySection"
  type ObjectType = List[TransitivityDB]
  var data:ObjectType= Nil
  var changes: List[Change[_]] = Nil


  def passiveOp(flag:String) = handleChange(new Change(this, true, List(flag)))//TODO add specific Term pointers
  def passiveAdd() = passiveOp("ADD")

  def initialize():Unit = {}//TODO ADD INITIALIZATION
}
