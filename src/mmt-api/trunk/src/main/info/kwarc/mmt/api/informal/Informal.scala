package info.kwarc.mmt.api.informal

import info.kwarc.mmt.api._
import objects._
import symbols._
import metadata._
import scala.xml._

object Informal {
   val base = DPath(utils.URI("http", "cds.omdoc.org") / "urtheories")
   
   val path = base ? "Informal"
   def constant(name : String) = path ? name
//   lazy val hoas = notations.HOAS(Apply.path, Lambda.path, OfType.path)
}

class InformalSym(name: String) {
   val path = Informal.path ? name
   val term = OMS(path)
}

object Opaque extends InformalSym("opaque") {
  def apply(child : List[Term]) = OMA(term, child)
}

object Narration extends InformalSym("narration") {
  
}

object FlexiformalXML {
  def apply(node : Node) : Term = {
    OMATTR(Opaque.term,Narration.term,OMFOREIGN(node))
  }
}

object FlexiformalTerm {
  def apply(term : Term) : Term = term
}

object FlexiformalRef {
  def apply(term : Term, node : Node) : Term = {
    OMATTR(term, Narration.term, OMFOREIGN(node))
  } 
}

object FlexiformalNode {
  def apply(node : Node, child : List[Term]) : Term = {
    OMATTR(Opaque.apply(child), Narration.term, OMFOREIGN(node))
  }
}


object Definition {
  def apply(home : Term, name : LocalName, targets : List[GlobalName], df : Term) : Constant = {
    val const = Constant(home, name, None, None, Some(df), None)
    const.metadata.add(new MetaDatum((new InformalSym("role")).path, OMA((new InformalSym("defines")).term, targets.map(OMS(_)))))
    const
  }
}

object Exercise {
  def apply(home : Term, name : LocalName, prob : Term, sol : Option[Term]) : Constant = {
    val const = Constant(home, name, None, Some(prob), sol, None)
    const.metadata.add(new MetaDatum((new InformalSym("role")).path, (new InformalSym("exercise")).term))
    const
  }
}

object Example {
  def apply(home : Term, name : LocalName, targets : List[GlobalName], df : Term) : Constant = {
    val const = Constant(home, name, None, None, Some(df), None)
    const.metadata.add(new MetaDatum((new InformalSym("role")).path, OMA((new InformalSym("example")).term, targets.map(OMS(_)))))
    const
  }
}



object PlainNarration {
  def apply(home : Term, name : LocalName, df : Term) : Constant = {
    Constant(home, name, None, None, Some(df), None)
  }
}

