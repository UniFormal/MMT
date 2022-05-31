package info.kwarc.mmt.lf.itp.InteractiveStructuredProof

import info.kwarc.mmt.api.Rule
import info.kwarc.mmt.api.checking.Solver
import info.kwarc.mmt.lf.itp.{InteractiveProof, Tactic}

import scala.util.parsing.combinator.{JavaTokenParsers, PackratParsers}
import scala.util.parsing.input.CharSequenceReader

class StructuredProofParser extends JavaTokenParsers with PackratParsers{
  def parseStructuredProof(ip : InteractiveProof , str : String) : Option[Tactic] = {

    val parsers = ip.slvr.rules.getOrdered(classOf[StructuredProofParseRule]).map(x => x.parseTactic(this, ip))

    def findparser(prs: List[Parser[(Tactic, Fixity)]], in: Input): ParseResult[(Tactic, Fixity)] = prs match {
      case Nil => Error("no applicable parser", in)
      case x :: xs => x.apply(in) match {
        case s@Success(tac, nxt) => s
        case _ => findparser(xs, in)
      }
    }
/*
    def parseloop(in: Input): Option[List[(Tactic, Fixity)]] = if (in.atEnd) Some(Nil) else findparser(parsers, in) match {
      case Success(t, nxt) => {
        parseloop(nxt).map(t :: _)

      }
      case _ => None
    }

    val tmp = parseloop(new CharSequenceReader(str))

    val orderedprio = tmp.map(x => x.sortWith((x,y) => x._2.prio < y._2.prio || x._2.prio == y._2.prio && x._2.isleft && !y._2.isleft   ).distinct)
    val tmp0 = tmp.map(_.zipWithIndex)

    def buildparsetree(ls : List[((Tactic, Fixity ),Int)], ls0 : List[(Tactic,Fixity)]) = ls0 match {
      case Nil =>
      case (t, FixityLeft(p , al))::xs => {

      }
      case(t , FixityRight(p , al)):: xs => {

      }
    }

 */

???
  }
}
