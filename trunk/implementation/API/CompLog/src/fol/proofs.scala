package fol

// the provability judgment: "variables ; assumptions |- conclusion" 
case class Provable(variables : Context, assumptions : List[Form], conclusion : Form)

abstract class Proof {
  def of(sig : Signature) : Option[Provable]  //the proved judgment, if this is a well-formed proof
  def check(sig : Signature) : Boolean = of(sig).isDefined
}

case class Axiom(vars : Context, ass : List[Form], a : Form) extends Proof {
  def of(sig : Signature) = if (ass.exists(_ == a) && a.check(sig, vars))
     Some(Provable(vars, ass, a)) else None
}
case class ConjIntro(p : Proof, q : Proof) extends Proof {
  def of(sig : Signature) = (p.of(sig), q.of(sig)) match {
    case (Some(Provable(pvars, pass, a)), Some(Provable(qvars, qass, b))) if (pvars, pass) == (qvars, qass)  
      => Some(Provable(pvars, pass, Conjunction(a,b)))
    case _ => None
  }
}
case class ConjElimR(p : Proof) extends Proof {
  def of(sig : Signature) = p.of(sig).flatMap {
    case Provable(vars, ass, Conjunction(_,b)) => Some(Provable(vars, ass, b))
    case _ => None
  }
}
case class ConjElimL(p : Proof) extends Proof {
  def of(sig : Signature) = p.of(sig).flatMap {
    case Provable(vars, ass, Conjunction(a,_)) => Some(Provable(vars, ass, a))
    case _ => None
  }
}
case class ImplIntro(assume : Form, p : Proof) extends Proof {
  def of(sig : Signature) = p.of(sig).flatMap {
    case Provable(vars, ass, b) if (ass.last == assume) && assume.check(sig, vars) 
      => Some(Provable(vars, ass.init, Implication(assume, b)))
    case _ => None
  }
}
case class ImplElim(p : Proof, q : Proof) extends Proof {
  def of(sig : Signature) = (p.of(sig),q.of(sig)) match {
    case (Some(Provable(pvars, pass, Implication(a,b))), Some(Provable(qvars, qass, c)))
      if (pvars, pass) == (qvars, qass) && a == c 
        => Some(Provable(pvars, pass, b))
    case _ => None
  }
}

case class UnivIntro(variable : String, p : Proof) extends Proof {
  def of(sig : Signature) = p.of(sig).flatMap {
    case Provable(con, ass, a) if (con.vars.last == variable)
      => Some(Provable(Context(con.vars.init), ass, Universal(variable, a)))
    case _ => None
  }
}

case class UnivElim(p : Proof, t : Term) extends Proof {
  def of(sig : Signature) = p.of(sig).flatMap {
    case Provable(con, ass, Universal(v, a)) if t.check(sig, con)
      => Some(Provable(con, ass, a.subs(con,v,t)))
    case _ => None
  }
}
