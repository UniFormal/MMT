package info.kwarc.mmt.api.informal

import info.kwarc.mmt.api._
import objects._
import symbols._
import metadata._
import scala.xml._

@deprecated("needs full revision, should be deleted")
object Informal {
   val base = DPath(utils.URI("http", "cds.omdoc.org") / "urtheories")

   val path = base ? "Informal"
   def constant(name : String) = path ? name
//   lazy val hoas = notations.HOAS(Apply.path, Lambda.path, OfType.path)
}

@deprecated("needs full revision, should be deleted")
class InformalSym(name: String) {
   val path = Informal.path ? name
   val term = OMS(path)
}

@deprecated("needs full revision, should be deleted")
object Opaque extends InformalSym("opaque") {
  def apply(child : List[Term]) = OMA(term, child)
}

@deprecated("needs full revision, should be deleted")
object Narration extends InformalSym("narration")

@deprecated("needs full revision, should be deleted")
object MathMLNarration extends InformalSym("mathml-narration")

@deprecated("needs full revision, should be deleted")
object FlexiformalXML {
  def apply(node : Node) : Term = {
    OMATTR(Opaque.term,Narration.term,OMFOREIGN(node))
  }
}

@deprecated("needs full revision, should be deleted")
object FlexiformalTerm {
  def apply(term : Term) : Term = term
}

@deprecated("needs full revision, should be deleted")
object FlexiformalRef {
  def apply(term : Term, node : Node) : Term = {
    OMATTR(term, Narration.term, OMFOREIGN(node))
  }
}

@deprecated("needs full revision, should be deleted")
object FlexiformalNode {
  def apply(node : Node, child : List[(Term, List[Int])]) : Term = {
    val terms = child.map(_._1)
    val posBase = Position(0)
    val refs = child.map(_._2).zipWithIndex.map(p => (p._1, posBase / (p._2 + 1)))
    val narNode = refs.foldLeft(node)((n,ref) => rewriteNode(n, ref._1, ref._2))
    OMATTR(Opaque.apply(terms), Narration.term, OMFOREIGN(narNode))
  }

  def rewriteNode(node : Node, pos : List[Int], ref : Position) : Node = pos match {
    case Nil => <immtref pos={ref.toString}></immtref>
    case hd :: tl =>
      val child = node.child.zipWithIndex map {
        case (n,i) if i == hd =>
          rewriteNode(n, tl, ref)
        case p => p._1
      }

      new Elem(node.prefix, node.label, node.attributes, node.scope, false, child : _*)
  }
}

@deprecated("needs full revision, should be deleted")
object Definition {
  def apply(home : Term, name : LocalName, targets : List[GlobalName], df : Term, localSection : LocalName) : Constant = {
    val const = Constant(home, name, Nil, None, Some(df), None)
    const.metadata.add(new MetaDatum((new InformalSym("role")).path, OMA((new InformalSym("defines")).term, targets.map(OMS(_)))))
    const.setDocumentHome(localSection)
    const
  }
}

@deprecated("needs full revision, should be deleted")
object Assertion {
  def apply(home : Term, name : LocalName, df : Term, localSection : LocalName) : Constant = {
    val const = Constant(home, name, Nil, None, Some(df), None)
    const.metadata.add(new MetaDatum((new InformalSym("role")).path, (new InformalSym("assertion")).term))
    const.setDocumentHome(localSection)
    const
  }
}

@deprecated("needs full revision, should be deleted")
object Exercise {
  def apply(home : Term, name : LocalName, prob : Term, sol : Option[Term], localSection : LocalName) : Constant = {
    val const = Constant(home, name, Nil, Some(prob), sol, None)
    const.metadata.add(new MetaDatum((new InformalSym("role")).path, (new InformalSym("exercise")).term))
    const.setDocumentHome(localSection)
    const
  }
}

@deprecated("needs full revision, should be deleted")
object Example {
  def apply(home : Term, name : LocalName, targets : List[GlobalName], df : Term, localSection : LocalName) : Constant = {
    val const = Constant(home, name, Nil, None, Some(df), None)
    const.metadata.add(new MetaDatum((new InformalSym("role")).path, OMA((new InformalSym("example")).term, targets.map(OMS(_)))))
    const.setDocumentHome(localSection)
    const
  }
}

@deprecated("needs full revision, should be deleted")
object Proof {
  def apply(home : Term, name : LocalName, targets : List[GlobalName], df : Term, localSection : LocalName) : Constant = {
    val const = Constant(home, name, Nil, None, Some(df), None)
    const.metadata.add(new MetaDatum((new InformalSym("role")).path, OMA((new InformalSym("proof")).term, targets.map(OMS(_)))))
    const.setDocumentHome(localSection)
    const
  }
}

@deprecated("needs full revision, should be deleted")
object PlainNarration {
  def apply(home : Term, name : LocalName, df : Term, localSection : LocalName) : Constant = {
    val const = Constant(home, name, Nil, None, Some(df), None)
    const.setDocumentHome(localSection)
    const
  }
}

