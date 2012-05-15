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
      def instance(name: String, pattern: String, args: List[EXP]) = APPLY("lf.instance", name, STRING(pattern), ALIST(args))
   }
   def argsToEXP(args: List[CatRef]) : List[EXP] = args.map((x: CatRef) => ID(x.target))
   def argsToStringWithID(args: List[CatRef]) = args.foldLeft(" of id")(_ + " * " + _)
   def decl(d: DECL) {println(fl.decl(d))}
   def apply(log: LogicSyntax) {
      //auxiliary types for ids for better readability
      decl (TYPEDEF("id", ID("string")))
      decl (TYPEDEF("var", ID("int")))
      
      // the categories
      val cats = log.cats map {case Category(c, cons) =>
         val cases = cons map {
            case Connective(n, args) => CONS(n, argsToEXP(args))
            case Binder(n, argOpt, bound, scope) =>
               val arg : List[EXP] = argOpt match {case Some(a) => List(a) case None => Nil}
               CONS(n, arg ::: List(ID("var"), (scope:EXP)))
            case ConstantSymbol(p, n, args) => CONS(p + "_" + n, ID("id") :: argsToEXP(args))
            case VariableSymbol => CONS(c + "_var", List(ID("var")))
         }
         ADT(c, cases)
      }
      // all categories as datatype ... and ... and 
      cats foreach decl
      
      // the declarations
      val decls = log.decls map {case Declaration(p, args) =>
        // datatype p = p of a1 ... an
         ADT(p, List(CONS(p, ID("id") :: argsToEXP(args))))
      }
      // all declaration types
      decls foreach {d => decl(d); println()}
      // the labeled union type of all declaration types
      val ofdecls = log.decls map {case Declaration(p, _) =>
         CONS(p + "_decl", List(p))
      }
      decl(ADT("decl", ofdecls))

      // the type of signatures
      decl(TYPEDEF("sign", LIST("decl")))
      // the type of theories
      decl(RECORD("theo", List(FIELD("sign", "sign"), FIELD("axioms", LIST(log.form)))))
      
      decl(EXCEPTION("error"))
      // the functions that map parse trees to expressions
      
      def parse(c: CatRef, a: EXP) = APPLY(c.target + "_from_pt", a)
      def qualIDFirst(e: EXP) = APPLY("parse.qualIDSplitFirst", e)   //parse.qualIDSplit("instance_name") = ("instance","name")
      def qualIDSecond(e: EXP) = APPLY("parse.qualIDSplitSecond", e)
      val frompt = log.cats map {case Category(c, cons) =>
         val appcase = CASE(APPLY("parse.app", "n", "args"),
           cons.foldLeft[EXP](ERROR("error", STRINGCONCAT(STRING("illegal identifier: "), ID("n")))) {
              case (rest, Connective(con,cats)) =>
                 IF("n" === STRING(con),
                   IF(LENGTH("args") === INT(cats.length),
                       APPLY(con, cats.zipWithIndex map {case (r, i) =>
                          parse(r, AT("args", i))
                       } : _*),
                       ERROR("error", STRING("bad number of arguments, expected " + cats.length))
                     ),
                   rest
                 )
              //NOTE: The following case assumes that non-logical constructors for the same category have different names, even though they this is not necessarily the case if they are declared in different patterns
              //This is typical in practice though.
              case (rest, ConstantSymbol(pat, con,cats)) =>
                 IF(qualIDSecond("n") === STRING(con),
                   IF(LENGTH("args") === INT(cats.length),
                       APPLY(pat + "_" + con, qualIDFirst("n") :: (cats.zipWithIndex map {case (r, i) =>
                          parse(r, AT("args", i))
                       }) : _*),
                       ERROR("error", STRING("bad number of arguments, expected " + cats.length))
                     ),
                   rest
                 )
                 
              case (rest, _) => rest
           }
         )
         val bindcase = CASE(APPLY("parse.bind", "n", "v", "s"),
            cons.foldLeft[EXP](ERROR("error", STRINGCONCAT(STRING("illegal identifier: "), ID("n")))) {
               case (rest, Binder(name, None, bound, scope)) =>
                  IF("n" === STRING(name),
                     APPLY(name, "v", parse(scope, "s")),
                     rest
                  )
               case (rest, _) => rest
           }
         )
         val tbindcase = CASE(APPLY("parse.tbind", "n", "a", "v", "s"),
            cons.foldLeft[EXP](ERROR("error", STRINGCONCAT(STRING("illegal identifier: "), ID("n")))) {
               case (rest, Binder(name, Some(a), bound, scope)) =>
                  IF("n" === STRING(name),
                     APPLY(name, parse(a, "a"), "v", parse(scope, "s")),
                     rest
                  )
               case (rest, _) => rest
           }
         )
         val varcase = CASE(APPLY("parse.var", "n"),
             if (cons.exists(_ == VariableSymbol)) APPLY(c + "_var", "n")
             else ERROR("error", STRING("variables not allowed here"))
         )
         val cases = MATCH("x", List(appcase, bindcase, tbindcase, varcase))
         FUNCTION(c + "_from_pt", List(ARG("x", "parse.tree")), c, cases)
      }
      decl(FUNCTIONRec(frompt))
      
      // a function that parses declarations
      val declfrompt = log.decls.foldLeft[EXP](ERROR("error", STRING("illegal pattern"))) {
         case (rest, Declaration(p, args)) =>
            IF("p" === STRING(p),
              IF(LENGTH("args") === INT(args.length),
                 APPLY(p + "_decl", ID("i") :: (args.zipWithIndex map {case (r, i) =>
                          parse(r, AT("args", i))
                       }) : _*),
                 ERROR("error", STRING("bad number of arguments, expected " + args.length))
              ),
              rest
           )
      }
      decl(FUNCTION("decl_from_pt", List(ARG("d", "parse.decl")), "decl",
              MATCH("d", List(CASE(APPLY("instance", ID("i"), ID("p"), ID("args")), declfrompt)))))
      // functions that parse signatures and theories
      decl(FUNCTION("sign_from_pt", List(ARG("sg", "parse.sign")), "sign", MAP("sg", "decl_from_pt")))
      decl(FUNCTION("axiom_from_pt", List(ARG("ax", "parse.tree")), log.form, parse(log.form, "ax")))
      decl(FUNCTION("theo_from_pt", List(ARG("th", "parse.theo")), "theo", 
          ARECORD("theo", List(FIELD("sign", APPLY("sign_from_pt", SELECT("th", "sign"))), FIELD("axioms", MAP(SELECT("th", "axioms"), "axiom_to_lf"))))
      ))

      def tolf(c: CatRef, e: EXP) = APPLY(c.target + "_to_lf", e)
      def varlist(args: List[CatRef]) : List[EXP] = (args.zipWithIndex map {case (c,i) => ID("x" + i)})
      def recvarlist(args: List[CatRef]) : List[EXP] = args.zipWithIndex map {case (c,i) => tolf(c, "x" + i)}
      // the functions that map expressions to LF
      val tolffuncs = log.cats map {case Category(c, cons) =>
         val cases = cons map {
            case Connective(n, args) => CASE(APPLY(n, varlist(args) : _*), lf.app(n,recvarlist(args)))
            case Binder(n, Some(a), bound, scope) =>
               val lambda = lf.lam("v", lf.app(bound.target, List(tolf(a, "a"))), tolf(scope, "s"))
               CASE(APPLY(n, ID("a"), ID("v"), ID("s")),
                    lf.app(n, List(tolf(a,"a"), lambda)))
            case Binder(n, None, bound, scope) =>
               val lambda = lf.lam("v", bound.target, tolf(scope, "s"))
               CASE(APPLY(n, ID("v"), ID("s")),
                    lf.app(n, List(lambda)))
            case ConstantSymbol(p, n, args) => CASE(APPLY(p + "_" + n, ID("i") :: varlist(args) : _*), lf.qapp("i", n, recvarlist(args)))
            case VariableSymbol => CASE(APPLY(c + "_var", "n"), lf.variable("n"))
         }
         FUNCTION(c + "_to_lf", List(ARG("x", c)), "lf.exp", MATCH("x", cases))
      }
      decl(FUNCTIONRec(tolffuncs))
      
      // a function that maps declarations to LF instance declarations
      val decltolfcases = log.decls map {case Declaration(p, args) =>
         CASE(APPLY(p + "_decl", ID("n") :: varlist(args) : _*), lf.instance(p, "n", recvarlist(args))) 
      }
      decl(FUNCTION("decl_to_lf", List(ARG("d", "decl")), "lf.decl", MATCH("d", decltolfcases)))

      // functions that map signatures and theories to LF signatures
      decl(FUNCTION("sign_to_lf", List(ARG("sg", "sign")), "lf.sign", MAP("sg", "decl_to_lf"))) 
      decl(FUNCTION("axiom_to_lf", List(ARG("ax", log.form)), "lf.decl", lf.decl("_", tolf(log.form, "ax"))))
      decl(FUNCTION("theo_to_lf", List(ARG("th", "theo")), "lf.sign", 
          CONCAT(APPLY("sign_to_lf", SELECT("th", "sign")), MAP(SELECT("th", "axioms"), "axiom_to_lf"))
      ))
   }
}