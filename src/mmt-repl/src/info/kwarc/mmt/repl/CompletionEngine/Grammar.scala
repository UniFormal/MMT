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

  def * : Grammar = RepG(this, Empty())
  def + : Grammar = this + this.*

  def ? : Grammar = OptionalG(this)

  def | (right : Grammar) : OrG = right match {
    case OrG(b) => OrG(this :: b)
    case _ => OrG(List(this, right))
  }

  def ! : Grammar = this
}

object Grammar {
  def fromList(list : List[Node]): Grammar = list match {
    case h :: t => SeqG(h, fromList(t))
    case Nil => Empty()
  }
}

/** A sequential grammar */
case class SeqG(head : Node, tail : Grammar) extends Grammar {
  def gobble(token : Token) : Option[(Token, Grammar)] = head.applicable(token) match {
    case true => Some((token, tail))
    case false => None
  }

  def + (right : Grammar): Grammar = SeqG(head, tail + right)

  def complete(token : Token = Token("")) : List[Token] = head.suggest(token)

  override def ! : Grammar = SeqG(head, tail.!)
}

object OptionalG {
  def apply(tail : Grammar): OrG = {
    OrG(List(tail, Empty()))
  }
}

case class RepG(head : Grammar, tail : Grammar) extends Grammar {
  def gobble(token : Token) : Option[(Token, Grammar)] = {
    println("gobble", this)
    head.gobble(token) match {
      case Some((t, g)) => Some((t, g + this))
      case None => tail.gobble(token)
    }
  }
  def complete(token : Token) : List[Token] = {
    println("complete", this)
    head.complete(token) ::: tail.complete(token)
  }

  def +(right : Grammar) : Grammar = RepG(head, tail + right)

  override def ! : Grammar = head.! match {
    case Empty() => tail.!
    case h => RepG(h, tail.!)
  }
}


/** a branching grammar */
case class OrG(branches : List[Grammar]) extends Grammar {
  def gobble(token : Token) : Option[(Token, Grammar)] = branches.flatMap(_.gobble(token)) match {
    case Nil => None
    case h :: Nil => Some(h)
    case (th, gh) :: t => Some((th, OrG(gh :: t.map(_._2))))
  }

  def + (right : Grammar): Grammar = OrG(branches.map(_ + right))

  def complete(token : Token = Token("")) : List[Token] = branches.flatMap(_.complete(token)).distinct

  override def ! : Grammar = branches.map(_.!).distinct match {
    case Nil => Empty()
    case h :: Nil => h
    case OrG(b) :: t => OrG(b ::: t).!
    case h :: t => OrG(h :: t)
  }
}

/** the empty grammar */
case class Empty() extends Grammar {
  def gobble(token : Token) : Option[(Token, Grammar)] = None
  def complete(token : Token = Token("")) = Nil

  def + (right : Grammar): Grammar = right
  override def * : Grammar = Empty()

  override def ! : Empty = this
}