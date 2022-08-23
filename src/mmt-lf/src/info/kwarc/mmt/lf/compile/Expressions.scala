package info.kwarc.mmt.lf.compile

package object extras {
   type FuncName = String
   type ConsName = String
}

/** expressions of a simple functional language */
sealed abstract class EXP {
   /** equality predicate: e === e' */
   def ===(right: EXP) = EQUAL(this, right)
   /** addition: a ++ b */
   def ++(right: EXP) = PLUS(this, right)
   /** multiplication: a ** b */
   def **(right: EXP) = TIMES(this, right)
   /** concatenation of strings: s + s' */
   def +(e: EXP) = STRINGCONCAT(this, e)

   /** type of lists: a.list */
   def list = LIST(this)
   /** length of a list: l.length */
   def length = LENGTH(this)
   /** accessing elements of lists: l __ n */
   def at(index: EXP) = AT(this, index)
   /** concatenation of lists: l ::: l' */
   def :::(l: EXP) = CONCAT(l, this)
   /** map over a list: l map f */
   def map(f: String) = MAP(this, ID(f))
   //def map(f: EXP => EXP) = MAP(this, f(ID(s)))

   /** product types: A1 * ... * An */
   def *(e: EXP) = this match {
      case PROD(l) => PROD(l ::: List(e))
      case t => PROD(List(t,e))
   }
   /** tuples: a1 | ... | an */
   def |(e: EXP) = this match {
      case TUPLE(l) => TUPLE(l ::: List(e))
      case t => TUPLE(List(t, e))
   }
   /** projections: a __ n */
   def __(i: Int) = PROJ(this, i)

   /** accessing fields of a record: r __ n */
   def __(f: ID) = SELECT(this,f.name)

   /** variable declarations: n :: A */
   def ::(n: String) = ARG(n, this)
   /** field declarations: n ::: A */
   def :::(n: String) = FIELD(n, this)
   /** function types: return <-- (x1 :: A1, ... xn :: An) */
   def <--(args: ARG*) = FUNCTYPE(args.toList,this)

   /** pattern matching: a Match (pattern1 |> case1, ..., patternN |> caseN) */
   def Match(cases: CASE*) = MATCH(this, cases.toList)
}
/** identifiers
 * the empty identifier ID("") is reserved: it refers to the current declaration, which is useful in recursive ADT and FUNCTION definitions
 */
case class ID(name: String) extends EXP {
   /** application to arguments: id(args) */
   def apply(args: EXP*) = APPLY(name, args : _*)
   /** construction of a record: rectype(f1 ::: v1, ... fn ::: vn) */
   def apply(fs: FIELD*) = ARECORD(name, fs.toList)
   /** case in a pattern match: id |> body */
   def ==>(body: EXP) = CASE(this, body)
   def prepend(s : String) = s + name
   def o(that: ID) = ID(name + "." + that.name)
}

/** function application */
case class APPLY(fun: String, args: EXP*) extends EXP {
   /** case in a pattern match: id(args) |> body */
   def ==>(body: EXP) = CASE(this, body)
}

/** if then else */
case class IF(cond: EXP, thn: EXP, els: EXP) extends EXP
/** pattern matching */
case class MATCH(arg: EXP, cases: List[CASE]) extends EXP
/** throwing of an exception */
case class ERROR(name: String, msg: EXP) extends EXP

/** equality */
case class EQUAL(left: EXP, right: EXP) extends EXP

/** conjunction */
case class AND(left: EXP, right: EXP) extends EXP
/** disjunction */
case class OR(left: EXP, right: EXP) extends EXP

/** type of integers */
case object INTS extends EXP
/** type of booleans */
case object BOOLS extends EXP
/** integer literals */
case class INT(value: Int) extends EXP
/** addition */
case class PLUS(left: EXP, right: EXP) extends EXP
/** multiplication */
case class TIMES(left: EXP, right: EXP) extends EXP
/** type of strings */
case object STRINGS extends EXP
/** string literals */
case class STRING(value: String) extends EXP
/** concatenation of strings */
case class STRINGCONCAT(left: EXP, right: EXP) extends EXP

/** type of lists */
case class LIST(tp: EXP) extends EXP
/** a list */
case class ALIST(elems: List[EXP]) extends EXP
/** length of a list */
case class LENGTH(l: EXP) extends EXP
/** element of a list at a given index */
case class AT(l: EXP, index: EXP) extends EXP
/** concatenation of lists */
case class CONCAT(left: EXP, right: EXP) extends EXP
/** map over a list */
case class MAP(l: EXP, fun: ID) extends EXP
/** function composition */
//case class COMPOSE(fun1: ID, fun2: ID) extends ID()

/** type of options */
case class OPTION(tp: EXP) extends EXP
/** a defined option */
case class SOME(elem: EXP) extends EXP
/** the empty option */
case object NONE extends EXP
/** unsafe access of the optional element (.get) */
case class UNOPTION(sm: EXP) extends EXP

/** product type */
case class PROD(tps: List[EXP]) extends EXP
/** tuple */
case class TUPLE(tps: List[EXP]) extends EXP
/** projection out of a product */
case class PROJ(exp: EXP, proj: Int) extends EXP

/** record value */
case class ARECORD(tp: String, fields: List[FIELD]) extends EXP
/** selection from a record */
case class SELECT(record: EXP, field: String) extends EXP

/** auxiliary class for records types and values */
case class FIELD(name: String, value: EXP)
/** auxiliary class for cases in a MATCH */
case class CASE(pattern: EXP, body: EXP)
/** auxiliary class for arguments of a FUNCTION */
case class ARG(name: String, tp: EXP)
/** auxiliary class for constructors of an ADT */
case class CONS(name: String, args: List[EXP] = Nil)
/** auxiliary class for constructors of an ADT as
 *  f of ("x" :: "type", ...)
 */
case class CONSHEAD(name: String) {
   def of(args: EXP*) = CONS(name, args.toList)
}

/** An incomplete type checker for expressions */
object EXP {
   def infer(e: EXP)(implicit context: Context) : Type = e match {
      case ID(n) => context(n) match {
         case Some(t) => t
         case None => throw SyntaxError("undeclared identifier: " + e)
      }
      case APPLY(n, args @ _*) => infer(ID(n)) match {
        case FunctionalType(argtypes, ret) =>
           if (args.length != argtypes.length)
              throw SyntaxError("wrong number of arguments: " + e)
           (args zip argtypes) foreach {case (a,t) => check(a, t)}
           BuiltinType(ret)
        case BuiltinType(e) if args.isEmpty => BuiltinType(e)
        case _ => throw SyntaxError("not typable: " + e)
      }
      case IF(c,t,e) =>
         check(c, BOOLS)
         val List(tT, eT) = List(t,e) map {infer(_)}
         if (tT == eT) tT else
           throw SyntaxError("types of then and else branch not equal: " + e)
      case ERROR(_,_) => ErrorType
      case EQUAL(x,y) =>
         (infer(x), infer(y)) match {
           case (BuiltinType(a), BuiltinType(b)) if a == b => BuiltinType(BOOLS)
           case _ => throw SyntaxError("ill-formed comparison: " + e)
         }
      case AND(l,r) =>
        check(l,BOOLS)
        check(r,BOOLS)
        BuiltinType(BOOLS)
      case OR(l,r) =>
        check(l,BOOLS)
        check(r,BOOLS)
        BuiltinType(BOOLS)
      case BOOLS => KindOfTypes
      case INTS => KindOfTypes
      case INT(_) => BuiltinType(INTS)
      case PLUS(x,y) =>
         check(x, INTS)
         check(y, INTS)
         BuiltinType(INTS)
      case TIMES(x,y) =>
         check(x, INTS)
         check(y, INTS)
         BuiltinType(INTS)
      case STRINGS => KindOfTypes
      case STRING(_) => KindOfTypes
      case STRINGCONCAT(x,y) =>
         check(x, STRINGS)
         check(y, STRINGS)
         BuiltinType(STRINGS)
      case LIST(a) =>
         checkType(a)
         KindOfTypes
      case ALIST(es) =>
         val ts = es map infer
         ts match {
            case Nil => throw SyntaxError("cannot infer type of empty list: " + e)
            case BuiltinType(hd) :: tl =>
               if (tl forall {_ == BuiltinType(hd)}) BuiltinType(LIST(hd))
               else throw SyntaxError("different types in list: " + e)
            case _ => throw SyntaxError("not a well-formed list: " + e)
         }
      case LENGTH(l) => infer(l) match {
         case BuiltinType(LIST(_)) => BuiltinType(INTS)
         case t => throw SyntaxError("found: " + t + ", expected: list")
      }
      case AT(l, index) =>
         check(index, INTS)
         infer(l) match {
            case BuiltinType(LIST(t)) => BuiltinType(t)
            case t => throw SyntaxError("found: " + t + ", expected: list")
         }
      case CONCAT(l, m) =>
         val a = infer(l) match {
            case BuiltinType(LIST(t)) => t
            case t => throw SyntaxError("found: " + t + ", expected: list")
         }
         val b = infer(m) match {
            case BuiltinType(LIST(t)) => t
            case t => throw SyntaxError("found: " + t + ", expected: list")
         }
         if (a == b) BuiltinType(LIST(a))
         else throw SyntaxError("concatenation of lists of different types: " + e)
      case MAP(l, fun) =>
         val a = infer(l) match {
            case BuiltinType(LIST(t)) => t
            case t => throw SyntaxError("found: " + t + ", expected: list")
         }
         infer(fun) match {
            case FunctionalType(List(a), b) => BuiltinType(LIST(b))
            case t => throw SyntaxError("found: " + t + ", expected: function " + a + " -> ?")
         }
      case PROD(as) =>
         as foreach checkType
         KindOfTypes
      case TUPLE(es) =>
         val ts: List[EXP] = es map(infer) map {
            case BuiltinType(t) => t
            case t => throw SyntaxError("not legal in a product type: " + t)
         }
         BuiltinType(PROD(ts))
      case PROJ(e,n) => infer(e) match {
         case BuiltinType(PROD(ts)) if (n < ts.length) => BuiltinType(ts(n))
         case _ => throw SyntaxError("projection " + n + " not applicable to " + e)
      }
      case SELECT(e, f) => infer(APPLY(f, e))
      case ARECORD(r, fields) =>
         //TODO
         BuiltinType(ID(r))
      case OPTION(a) =>
        checkType(a)
        KindOfTypes
      case NONE =>
        throw SyntaxError("cannot infer type of NONE")
      case SOME(x) =>
        infer(x) match {
          case BuiltinType(a) => BuiltinType(OPTION(a))
          case a => throw SyntaxError("not legal in an option type: " + a)
        }
      case UNOPTION(x) =>
        infer(x) match {
          case BuiltinType(OPTION(a)) => BuiltinType(a)
          case a => throw SyntaxError("not an option type: " + a)
        }
      case MATCH(e, cases) =>
         //TODO
         null
   }
   def checkType(tp: EXP)(implicit context: Context): Unit = {tp match {
      case INTS =>
      case BOOLS =>
      case STRINGS =>
      case LIST(a) => checkType(a)
      case PROD(as) => as foreach checkType
      case ID(n) => context(n) match {
        case Some(KindOfTypes) =>
        case _ => throw SyntaxError("ill-formed type: " + tp)
      }
      case _ => throw SyntaxError("ill-formed type: " + tp)
   }}
   def check(e: EXP, tp: EXP)(implicit context: Context): Unit = {
      //the type of the empty list cannot be inferred, so special treatment needed
      if (e == ALIST(Nil)) {tp match {
        case LIST(_) => return
        case t => throw SyntaxError("found " + t + ", expected list ?")
      }}
      //all other cases
      infer(e) match {
         case BuiltinType(t) => if (t != tp) throw SyntaxError("ill-typed, found: " + t + ", expected: " + tp)
         case _ => throw SyntaxError("not a typable term " + e)
      }
   }
}

/** useful implicit conversions */
object EXPConversions {
   implicit def stringToID(s: String) : ID = ID(s)
   implicit def intToINT(i: Int) : INT = INT(i)
   /** integer literals: as in Scala */
   implicit def catRefToID(c: CatRef) : ID = ID(c.target)
   /** lists with given elements: as in Scala */
   implicit def listToLIST(l: List[EXP]) : EXP = ALIST(l)
   /** pairs: as in Scala */
   implicit def tupleToTuple2(t: (EXP,EXP)) : EXP = TUPLE(List(t._1,t._2))

   /** constructors in an ADT as "cons" of (args) */
   implicit def stringToCONSHEAD(n: String) : CONSHEAD = CONSHEAD(n)
   /** declarations as "name" keyword decl */
   implicit def stringToDECLHEAD(n: String) : DECLHEAD = DECLHEAD(n)
   /** shortcut for the special identifier that refers to the current declaration */
   val current = ID("")
}
