package info.kwarc.mmt.lf

/** a type (family) with constructors; possible type arguments are erased */
case class Category(name: String, constructors: List[Constructor])
/** a reference to a type (family); possible type arguments are erased */
case class CatRef(target: String) {
   override def toString = target
}

/** constructor for a Category */ 
abstract class Constructor
/** a connective declared in a logic with some argument types
 * we assume that the LF types of all connectives are of the form a1 -> ... an -> c where ai, c are atomic
 */
case class Connective(name: String, arguments: List[CatRef]) extends Constructor
/** a binder with some argument types and a typed bound variable
 * we assume that the LF types of all binders are of the form a1 -> ... an -> (b -> s) -> c where ai, b, s, c are atomic
 */
case class Binder(name: String, arguments: List[CatRef], bound: (CatRef, List[Int]), scope: CatRef) extends Constructor
/** a (Connective-like) constructor declared in a pattern body with some argument types */
case class ConstantSymbol(pattern: String, name: String, arguments: List[CatRef]) extends Constructor
/** a bound variable */
case object VariableSymbol extends Constructor

/** a pragmatic declaration with some argument types */
case class Declaration(pattern: String, arguments: List[CatRef])

/** a logic syntax consists of some categories (one of which is the category of formulas) and declarations */
case class LogicSyntax(cats: List[Category], form: CatRef, decls: List[Declaration])

// covering only binary function and predicate symbols to avoid sequences
object SFOL {
   val asort  = ConstantSymbol("sort", "s", Nil)
   val sorts  = Category("S", List(asort))
   
   val funapp  = ConstantSymbol("fun", "f", List(CatRef("tm"), CatRef("tm")))
   val termvar = VariableSymbol
   val terms   = Category("tm", List(funapp, termvar)) 

   val and     = Connective("AND", List(CatRef("o"), CatRef("o")))
   val forall  = Binder("FORALL", List(CatRef("S")), (CatRef("tm"), List(1)), CatRef("o"))
   val predapp = ConstantSymbol("pred", "p", List(CatRef("tm"), CatRef("tm")))
   val forms   = Category("o", List(and, forall, predapp))

   val sort  = Declaration("sort", Nil)
   val fun   = Declaration("fun", List(CatRef("S"), CatRef("S"), CatRef("S")))
   val pred  = Declaration("pred", List(CatRef("S"), CatRef("S")))

   val Syn = LogicSyntax(List(sorts, terms, forms), CatRef("o"), List(sort,fun,pred))
}

/** generates a LogicSyntax for a given LF theory */
class CodeGenerator {
  
}

/** an example backend that transforms a LogicSyntax L into the SML code that represents and validates L-theories */
object SMLEmitter {
   object lf {
      def lam(v: String, t: String, s: String) = "lf.lam(" + v + ", " + t + ", " + s + ")"
      def app(f: String, args: List[String]) = "lf.app(lf.con(" + f + "), " + args.mkString("[", ", ", "]") + ")"
      def qapp(i: String, f: String, args: List[String]) = "lf.app(lf.qcon(" + i + ",\"" + f + "\"), " + args.mkString("[", ", ", "]") + ")"
      def variable(i: Int) = "lf.var(" + i + ")"
      def instance(p: String, args: List[String]) = "lf.instance(" + p + ", " + args.mkString("[", ", ", "]") + ")"
   }
   def argsToString(args: List[CatRef]) = args.mkString(" of ", " * ", "")
   def argsToStringWithID(args: List[CatRef]) = args.foldLeft(" of id")(_ + " * " + _)
   def apply(log: LogicSyntax) {
      //auxiliary types for ids for better readability
      println("type id = string")
      println("type var = int")
      println()
      
      // the categories
      val cats = log.cats map {case Category(c, cons) =>
         val cases = cons map {
            case Connective(n, args) => n + argsToString(args)
            case Binder(n, args, (bound,_), scope) => n + argsToString(args) + " * var * " + scope
            case ConstantSymbol(p, n, args) => p + "_" + n + argsToStringWithID(args)
            case VariableSymbol => c + "_var of var"
         }
         // all constructors as ... | c of a1 * ... an | ... 
         c + " = " + cases.mkString("","\n | ", "") 
      }
      // all categories as datatype ... and ... and 
      println(cats.mkString("datatype ", "\n\nand ", "\n"))
      
      // the declarations
      val decls = log.decls map {case Declaration(p, args) =>
        // datatype p = p of a1 ... an
         "datatype " + p + " = " + p + argsToStringWithID(args)
      }
      // all declaration types
      println(decls.mkString("", "\n\n", "\n"))
      // the labeled union type of all declaration types
      val ofdecls = log.decls map {case Declaration(p, _) =>
         p + "_decl of " + p
      }
      println("datatype decl = " + ofdecls.mkString("", " | ", "\n"))

      // the type of signatures
      println("type sign = decl list")
      // the type of theories
      println("type theo = sign * (" + log.form + " list)")
      println()
      
      def varlist(args: List[CatRef]) = (args.zipWithIndex map {case (c,i) => "x" + i})
      def recvarlist(args: List[CatRef]) = (args.zipWithIndex map {case (c,i) => c + "_to_lf(x" + i + ")"})
      def bracket(l: List[String]) = l.mkString("(", ", ", ")")
      // the functions that map expressions to LF
      val tolf = log.cats map {case Category(c, cons) =>
         val cases = cons map {
            case Connective(n, args) => n + bracket(varlist(args)) + " => " + lf.app(n,recvarlist(args))
            case Binder(n, args, (bound, inds), scope) =>
               val lambda = lf.lam("v", lf.app(bound.toString, inds.map(lf.variable(_))), scope + "_to_lf(s)")
               n + bracket(varlist(args) ::: List("v", "s")) + " => " + lf.app(n, recvarlist(args) ::: List(lambda))
            case ConstantSymbol(p, n, args) => p + "_" + n + bracket("i" :: varlist(args)) + " => " + lf.qapp("i", n, recvarlist(args))
            case VariableSymbol => c + "_var(i) => lf.var(i)"
         }
         c + "_to_lf(x: " + c + ") : lf.exp = case x\n" + cases.mkString(" of ", "\n  | ", "\n")
      }
      println(tolf.mkString("fun ","\nand ", ""))
      
      // a function that maps declarations to LF instance declarations
      val decltolfcases = log.decls map {case Declaration(p, args) =>
         p + "_decl" + bracket(varlist(args)) + " => " + lf.instance(p, recvarlist(args)) 
      }
      println("fun decl_to_lf(d: decl) = case d\n" + decltolfcases.mkString(" of ", "\n  | ", "\n"))

      // functions that map signatures and theories to LF signatures
      println("fun sign_to_lf(sg: sign) : lf.sign = List.map decl_to_lf sg")
      println("fun axiom_to_lf(ax: " + log.form + ") : lf.decl = lf.decl(\"_\", " + log.form + "_to_lf(ax))")
      println("fun theo_to_lf((sg, axs): theo) : lf.sign = sign_to_lf(sg) @ (List.map axiom_to_lf axs)")
   }
}