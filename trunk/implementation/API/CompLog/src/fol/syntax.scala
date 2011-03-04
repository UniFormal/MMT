package fol

case class Signature(funcsyms : List[(String, Int)], predsyms : List[(String, Int)]) {
  def check = uniqueNames((funcsyms ::: predsyms).map(_._1))
  // map(_._1) is a syntactic sugar for map(p => p._1)
  private def uniqueNames(l : List[String]) = l.length == l.removeDuplicates.length
  def hasFunc(f : String, ar : Int) = funcsyms.exists(_ == (f,ar))
  def hasPred(p : String, ar : Int) = predsyms.exists(_ == (p,ar))
}

case class Context(vars : List[String]) {
  def check(sig : Signature) : Boolean = true
  def hasVar(v : String) : Boolean = vars.exists(_ == v)
  def +(v : String) : Context = Context(vars ::: List(v)) //append a variable
  def ident = Substitution(vars.map(v => (v, Var(v)))) //identity substitution
}
case object EmptyCon extends Context(Nil)

case class Substitution(maps : List[(String, Term)]) {
   private val capturable : List[String] = maps.flatMap(_._2.freeVars).removeDuplicates
   private def rename(v : String) : String =
     if (capturable.exists(_ == v)) rename(v + "'")
        else v
   def check(sig : Signature, dom : Context, cod : Context) : Boolean = {
     dom.vars.length == maps.length &&
     dom.vars.zip(maps).forall({case (v, (w, t)) => v == w && t.check(sig, cod)}) 
   }
   def +(v : String, t : Term) = Substitution(maps ::: List((v,t)))     
   def apply(v : String) : Term = maps.reverse.find(p => v == p._1).get._2
   def apply(t : Term) : Term =
     t match {
       case Var(v) => apply(v)
       case Func(f,args) => Func(f,args.map(apply))
   }      
   def apply(F : Form) : Form =
     F match {
       case Pred(p,args) => Pred(p,args.map(apply))
       case Truth => Truth
       case Falsity => Falsity
       case Conjunction(l,r) => Conjunction(apply(l),apply(r))
       case Disjunction(l,r) => Disjunction(apply(l),apply(r))
       case Implication(l,r) => Implication(apply(l),apply(r))
       case Negation(n) => Negation(apply(n))
       case Universal(x,F) => Universal(x,(this + (x,Var(rename(x)))).apply(F))
       case Existential(x,F) => Existential(x,(this + (x,Var(rename(x)))).apply(F))
   }
}

abstract class Term {
  def check(sig : Signature, con : Context) : Boolean
  def subs(con : Context, v : String, t : Term) : Term =
     (con.ident + (v, t)).apply(this)
  def freeVars : List[String]
}
case class Var(name : String) extends Term {
  def check(sig : Signature, con : Context) = con.hasVar(name)
  def freeVars = List(name)
}
case class Func(name : String, args : List[Term]) extends Term {
  def check(sig : Signature, con : Context) = sig.hasFunc(name,args.length)
  def freeVars = args.flatMap(_.freeVars).removeDuplicates
}
 
abstract class Form {
  def check(sig : Signature, con : Context) : Boolean
  // substitute v with t
  def subs(con : Context, v : String, t : Term) : Form =
    (con.ident + (v, t)).apply(this)
}

case object Truth extends Form {
  def check(sig: Signature, con : Context) = true
}
case object Falsity extends Form {
  def check(sig: Signature, con : Context) = true
}
case class Conjunction(left : Form, right : Form) extends Form {
  def check(sig : Signature, con : Context) : Boolean = left.check(sig, con) && right.check(sig, con) 
}
case class Disjunction(left : Form, right : Form) extends Form {
  def check(sig : Signature, con : Context) : Boolean = left.check(sig, con) && right.check(sig, con) 
}
case class Negation(arg : Form) extends Form {
 def check(sig : Signature, con : Context) : Boolean = arg.check(sig, con)
}
case class Implication(left : Form, right : Form) extends Form {
  def check(sig : Signature, con : Context) : Boolean = left.check(sig, con) && right.check(sig, con) 
}
case class Universal(x : String, F : Form) extends Form {
 def check(sig : Signature, con : Context) : Boolean = F.check(sig,con + x)
}
case class Equality(left : Term, right : Term) extends Form {
 def check(sig : Signature, con : Context) : Boolean = left.check(sig,con) && right.check(sig,con)
}
case class Existential(x : String, F : Form) extends Form {
 def check(sig : Signature, con : Context) : Boolean = F.check(sig,con + x)
}
case class Pred(name : String, args : List[Term]) extends Form {
  def check(sig : Signature, con : Context) : Boolean = sig.hasPred(name,args.length)
}

class Theory(sig : Signature, axioms : List[Form]) {
  def check : Boolean = sig.check && axioms.forall(_.check(sig, EmptyCon))
}
