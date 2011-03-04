package fol

/** exception that is thrown when interpreting a function or predicate symbol applied to the wrong number of arguments */
case object IllegalNumberOfArguments extends java.lang.Throwable
/** exception that is thrown when interpreting a function or predicate symbol that is not in the signature */
case object IllegalSymbol extends java.lang.Throwable

/** the universe of a model
 * @param U a scala type big enough to contain the universe
 */
abstract class Universe[U] {
   /** the universe is obtained as the quotient of U by this equivalence relation */
   def equal(u: U, v: U) : Boolean
   /** an oracle for a universal predicate because this is undecidable for infinite models */
   def forall(p: U => Boolean) : Boolean
}

/**
 * finite universes implemented as a quotient of the integers
 * @param n the size of the universe
 */
case class FiniteUniverse(n : Int) extends Universe[Int] {
   //a list of integers representing the elements of the universe
   private val elements = List.range(0,n-1)
   /** equality is defined modulo n */
   def equal(u : Int, v : Int) = (u % n) == (v % n)
   /** the oracle can be implemented by evaluating p for all elements of the universe */
   def forall(p: Int => Boolean) = elements.forall(p)
}

/** an assignment into a model whose universe is based on U */
case class Assignment[U](l : List[(String, U)]) {
  def apply(name : String) = l.reverse.find(_._1 == name).get._2
  def +(v : String, u : U) = Assignment(l ::: List((v,u)))
  def check(sig : Signature, con : Context, mod : Model[U]) : Boolean = {
     l.length == con.vars.length &&
     con.vars.zip(l).forall({case (v, (w, u)) => v == w})
  }
}

/**
 * a model
 * @param U a type of which the universe is a quotient 
 */
abstract class Model[U] {
  /** the universe */
  val univ : Universe[U]
  /** the interpretation of function symbols
   * it is not possible in Scala to return a function of the proper arity
   * instead, any list of arguments must be permitted
   */
  def interpretFunc(f: String) : List[U] => U
  /** the interpretation of predicate symbols
   * it is not possible in Scala to return a function of the proper arity
   * instead, any list of arguments must be permitted
   */
  def interpretPred(p: String) : List[U] => Boolean
  /**
   * checks whether this is a model of a certain signature
   * this only checks that there are some interpretations for at least the symbols of sig
   */
  def check(sig : Signature) : Boolean = {
     try {
       sig.funcsyms.foreach(p => interpretFunc(p._1))
       sig.predsyms.foreach(p => interpretPred(p._1))
       true
     }
     catch {
       case IllegalSymbol => false
     }
  }
  def eval(t : Term, ass : Assignment[U]) : U = {
     t match {
       case Var(n) => ass(n)
       case Func(f,args) => interpretFunc(f) (args.map(eval(_,ass)))
     }
  }
  def eval(F : Form, ass : Assignment[U]) : Boolean = {
    F match {
      case Pred(p, args) => interpretPred(p) (args.map(eval(_,ass)))
      case Equality(s,t) => univ.equal(eval(s, ass), eval(t,ass))
      case Truth => true
      case Falsity => false
      case Conjunction(f,g) => eval(f,ass) & eval(g,ass) 
      case Disjunction(f,g) => eval(f,ass) | eval(g,ass)
      case Implication(f,g) => if (eval(f,ass)) eval(g,ass) else true
      case Negation(f) => ! eval(f,ass) 
      case Universal(v, g) =>
        val p = (u: U) => eval(g, ass + (v,u))
        univ.forall(p)
      case Existential(v, g) =>
        val p = (u: U) => ! eval(g, ass + (v,u))
        ! univ.forall(p)
    }
  }
}

/**
 * if the signature is fixed, we can do better, here a class for models of the Monoid signature 
 */
abstract class MonoidModel[U] extends Model[U] {
  val unit : U
  val comp : (U,U) => U
  def interpretFunc(n : String) = n match {
    case "unit" => (l : List[U]) => l match {
      case Nil => unit
      case _ => throw IllegalNumberOfArguments
    }
    case "comp" => (l : List[U]) => l match {
      case List(a,b) => comp(a,b)
      case _ => throw IllegalNumberOfArguments
    } 
    case _ => throw IllegalSymbol
  }
  def interpretPred(n : String) = throw IllegalSymbol
}

/**
 * Z modulo 3 as a MonoidModel
 */
class ZmoduloThree extends MonoidModel[Int] {
  val univ = FiniteUniverse(3)
  val unit = 0
  val comp = (x : Int, y : Int) => (x + y) % 3
}