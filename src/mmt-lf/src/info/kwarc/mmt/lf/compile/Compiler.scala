package info.kwarc.mmt.lf.compile

/** an example backend that transforms a LogicSyntax L into the SML code that represents and validates L-theories */
class Compiler(fl: FuncLang) {
   import EXPConversions._
   object lf {
      def lam(v: String, t: EXP, s: EXP) = APPLY("lf.lam", v, t, s)
      def app(f: String, args: List[EXP]) = APPLY("lf.app", f,  ALIST(args))
      def qapp(i: String, f: String, args: List[EXP]) = APPLY("lf.qapp", i, f, ALIST(args))
      def variable(name: String) = APPLY("lf.var", name)
      def decl(name: String, tp: EXP) = APPLY("lf.decl", name, tp)
      def instance(name: String, pattern: String, args: List[EXP]) = APPLY("lf.instance", name, pattern, ALIST(args))
   }
   def argsToEXP(args: List[CatRef]) : List[EXP] = args.map((x: CatRef) => ID(x.target))
   def argsToStringWithID(args: List[CatRef]) = args.foldLeft(" of id")(_ + " * " + _)
   def decl(d: DECL) {println(fl.decl(d))}
   def apply(log: LogicSyntax) {
      //auxiliary types for ids for better readability
      decl (TYPEDEF("id", ID("string")))
      decl (TYPEDEF("var", ID("int")))
      println()
      
      // the categories
      val cats = log.cats map {case Category(c, cons) =>
         val cases = cons map {
            case Connective(n, args) => CONS(n, argsToEXP(args))
            case Binder(n, args, (bound,_), scope) => CONS(n, argsToEXP(args) ::: List(ID("var"), ID(scope.target)))
            case ConstantSymbol(p, n, args) => CONS(p + "_" + n, ID("id") :: argsToEXP(args))
            case VariableSymbol => CONS(c + "_var", List(ID("var")))
         }
         ADT(c, cases)
      }
      // all categories as datatype ... and ... and 
      decl(ADTRec(cats))
      
      // the declarations
      val decls = log.decls map {case Declaration(p, args) =>
        // datatype p = p of a1 ... an
         ADT(p, List(CONS(p, ID("id") :: argsToEXP(args))))
      }
      // all declaration types
      decls foreach {d => decl(d); println()}
      // the labeled union type of all declaration types
      val ofdecls = log.decls map {case Declaration(p, _) =>
         CONS(p + "_decl", List("p"))
      }
      decl(ADT("decl", ofdecls))

      // the type of signatures
      decl(TYPEDEF("sign", LIST("decl")))
      // the type of theories
      decl(TYPEDEF("theo", PROD(List("sign", LIST(log.form)))))
      println()
      
      decl(EXCEPTION("error"))
      // the functions that map parse trees to expressions
      val frompt = log.cats map {case Category(c, cons) =>
         val appcase = CASE(APPLY("parse.app", "n", "args"),
           cons.foldLeft[EXP](ERROR("error", "unknown identifier: n")) {
              case (rest, Connective(con,cats)) =>
                 IF("n" === con,
                   IF(LENGTH("args") === cats.length,
                       APPLY(con, cats.zipWithIndex map {case (CatRef(a), i) =>
                          APPLY(c + "_from_pt", AT("args", i))
                       } : _*),
                       ERROR("error", "bad number of arguments, expected " + cats.length)
                     ),
                   rest
                 )
              case (rest, _) => rest
           })
         val varcase = if (cons.exists(_ == VariableSymbol))
            List(CASE(APPLY("parse.var", "n"), APPLY(c + "_var", "n")))
         else Nil
         val cases = MATCH("x", appcase :: varcase)
         FUNCTION(c + "_from_pt", List(ARG("x", "parse.tree")), c, cases)
      }
      decl(FUNCTIONRec(frompt))
      
      def varlist(args: List[CatRef]) : List[EXP] = (args.zipWithIndex map {case (c,i) => ID("x" + i)})
      def recvarlist(args: List[CatRef]) : List[EXP] = (args.zipWithIndex map {case (c,i) => APPLY(c + "_to_lf", "x" + i)})
      def bracket(l: List[String]) = l.mkString("(", ", ", ")")
      // the functions that map expressions to LF
      val tolf = log.cats map {case Category(c, cons) =>
         val cases = cons map {
            case Connective(n, args) => CASE(APPLY(n, varlist(args) : _*), lf.app(n,recvarlist(args)))
            case Binder(n, args, (bound, inds), scope) =>
               val lambda = lf.lam("v", lf.app(bound.target, inds.map(i => lf.variable("x" + i))), APPLY(scope + "_to_lf", "s"))
               CASE(APPLY(n, varlist(args) ::: List(ID("v"), ID("s")) : _*), lf.app(n, recvarlist(args) ::: List(lambda)))
            case ConstantSymbol(p, n, args) => CASE(APPLY(p + "_" + n, ID("i") :: varlist(args) : _*), lf.qapp("i", n, recvarlist(args)))
            case VariableSymbol => CASE(APPLY(c + "_var", "n"), lf.variable("n"))
         }
         FUNCTION(c + "_to_lf", List(ARG("x", c)), "lf.exp", MATCH("x", cases))
      }
      decl(FUNCTIONRec(tolf))
      
      // a function that maps declarations to LF instance declarations
      val decltolfcases = log.decls map {case Declaration(p, args) =>
         CASE(APPLY(p + "_decl", ID("n") :: varlist(args) : _*), lf.instance(p, "n", recvarlist(args))) 
      }
      decl(FUNCTION("decl_to_lf", List(ARG("d", "decl")), "lf.decl", MATCH("d", decltolfcases)))

      // functions that map signatures and theories to LF signatures
      decl(FUNCTION("sign_to_lf", List(ARG("sg", "sign")), "lf.sign", MAP("sg", "decl_to_lf"))) 
      decl(FUNCTION("axiom_to_lf", List(ARG("ax", log.form)), "lf.decl", lf.decl("_", APPLY(log.form + "_to_lf", "ax"))))
      decl(FUNCTION("theo_to_lf", List(ARG("th", "theo")), "lf.sign", 
          CONCAT(APPLY("sign_to_lf", PROJ("th", 1)), MAP(PROJ("th", 2), "axiom_to_lf"))
      ))
   }
}