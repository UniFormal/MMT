package info.kwarc.mmt.api.proving.itp

import info.kwarc.mmt.api.LocalName
import info.kwarc.mmt.api.checking.Solver
import info.kwarc.mmt.api.objects.{Context, Sub, Term, VarDecl}

import scala.collection.mutable.ListBuffer

/**
  * represents a (sub)goal in a [[Proof]]
  * @param g the conclusion/goal of the current (sub)goal
  * @param ctx the local context of the goal
  * @param ukname the name of the unknown it represents in the proof term [[Proof.proofTerm]]/[[Proof.proofTermAlt]]
  */
case class Goal(g : Term , ctx : Context , ukname : LocalName) {



  /**
    * gives a string representation of all of the (sub)goals
    * @param s
    * @return
    */
  def printGoal(s: Solver): String = {
    val res: ListBuffer[String] = ListBuffer[String]()
    res.append("Focused Goal: " + ukname)
    res.append("---------------")
    for (i <- ctx.variables) {
      val nm = i.name.toString
      val tp = i.tp.map(x => s.presentObj(x)).getOrElse("no type")
      res.append(nm + " : " + tp)
    }
    res.append("---------------")
    res.append(s.presentObj(g))
    res.mkString("\n")
  }

  def makeUnknown(s: Solver, u: LocalName) = {
    s.Unknown(u, ctx.variables.map(x => x.toTerm).toList)
  }


  /**
    * clreates a copy/clone of the proof (used for the history in the proof manager)
    * @return
    */
  def copy: Goal = {
    val tmpg = Goal(g: Term, ctx, ukname)
    tmpg
  }

  /**
    * convenience method to get the first substitution with name `ln`
    * @param ln
    * @param ls
    * @return
    */
  def getFirst(ln: LocalName, ls: List[Sub]): Option[Term] = ls match {
    case Nil => None
    case Sub(nm, t) :: xs => {
      if (nm == ln) {
        Some(t)
      } else {
        getFirst(ln, xs)
      }
    }
  }

  /**
    * same as [[getFirst]] but for [[VarDecl]]
    * @param ln
    * @param ls
    * @return
    */
  def getFirstV(ln: LocalName, ls: List[VarDecl]): Option[VarDecl] = ls match {
    case Nil => None
    case v :: xs => {
      if (v.name == ln) {
        Some(v)
      } else {
        getFirstV(ln, xs)
      }
    }
  }

}