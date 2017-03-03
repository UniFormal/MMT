package info.kwarc.mmt.repl.CompletionEngine

/** Represents a grammar that can be tab completed */
sealed abstract class Grammar {
  def gobble(token : Token) : Option[(Token, Grammar)]
  def gobble(tokens : List[Token]) : Option[(List[Token], Grammar)] = tokens match {
    case h :: Nil => gobble(h) match {
      case Some((tk, g)) => Some((List(tk), g))
      case None => None
    }
    case h :: t => gobble(h) match {
      case Some((tk, g)) => g.gobble(t) match {
        case Some((tkt, gt)) => Some((tk :: tkt, gt))
        case None => None
      }
      case None => None
    }
    case Nil => None
  }

  def complete(token : Token = Token("")) : List[Token]
  def complete(tokens : List[Token]): List[(List[Token], Token)] = tokens match {
    case Nil => Nil
    case h :: Nil => complete(h).map(e => (Nil, e))
    case h1 :: h2 :: t => {
      val (ht, tg) = gobble(h1) match {
        case s @ Some(_) => s.get
        case None => return Nil
      }

      tg.complete(h2 :: t).map(ps => (ht :: ps._1, ps._2))
    }
  }

  def + (right : Grammar): Grammar
  def * : Grammar

  def ? : Grammar = OrG(List(this, Empty()))

  def | (right : Grammar) : OrG = right match {
    case OrG(b) => OrG(this :: b)
    case _ => OrG(List(this, right))
  }

  def ! : Grammar = this
}

sealed abstract class LinearGrammar extends Grammar {
  override def ! : Grammar = this
}

object LinearGrammar {
  def fromList(list : List[Node]): LinearGrammar = list match {
    case h :: t => SeqG(h, fromList(t))
    case Nil => Empty()
  }
}

/** A sequential grammar */
case class SeqG(head : Node, tail : Grammar) extends LinearGrammar {
  def gobble(token : Token) : Option[(Token, Grammar)] = head match {
    case RepeatingNode(node : Node) => gobbleNormal(token) match {
      case Some((t, g)) => Some((t, SeqG(OptionalNode(head), tail)))
      case None => None
    }
    case OptionalNode(node : Node) => gobbleNormal(token) match {
      case Some((t, g)) => Some(t, g)
      case None => tail.gobble(token)
    }
    case node : Node => gobbleNormal(token)
  }

  def + (right : Grammar): Grammar = SeqG(head, tail + right)
  def * : Grammar = SeqG(RepeatingNode(head), tail)

  private def gobbleNormal(token : Token) : Option[(Token, Grammar)] = head.applicable(token) match {
    case true => Some((token, tail))
    case false => None
  }

  def complete(token : Token = Token("")) : List[Token] = head.suggest(token)

  override def ! : Grammar = SeqG(head, tail.!)
}



/** a branching grammar */
case class OrG(branches : List[Grammar]) extends Grammar {
  def gobble(token : Token) : Option[(Token, Grammar)] = branches.flatMap(_.gobble(token)) match {
    case Nil => None
    case h :: Nil => Some(h)
    case (th, gh) :: t => Some((th, OrG(gh :: t.map(_._2))))
  }

  def + (right : Grammar): Grammar = OrG(branches.map(_ + right))
  def * : Grammar = OrG(branches.map(_*))

  def complete(token : Token = Token("")) : List[Token] = branches.flatMap(_.complete(token)).distinct

  override def ! : Grammar = branches.map(_.!).distinct match {
    case Nil => Empty()
    case h :: Nil => h
    case OrG(b) :: t => OrG(b ::: t).!
    case h :: t => OrG(h :: t)
  }
}

/** the empty grammar */
case class Empty() extends LinearGrammar {
  def gobble(token : Token) : Option[(Token, Grammar)] = None
  def complete(token : Token = Token("")) = Nil

  def + (right : Grammar): Grammar = right
  def * : Grammar = Empty()

  override def ! : Empty = this
}