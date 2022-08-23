/**
 * Concrete syntax compiler
 * Takes an abstract syntax (LogicSyntax)
 * Produces concrete pseudo-code (a list of declarations)
 *
 * created by Florian Rabe
 *
 * modifications: added a list of labels (strings) that is constructed along with the declarations
 * modified by Aivaras:
 * a.jakubauskas@jacobs-university.de
 */
package info.kwarc.mmt.lf.compile

object Compiler {
  def apply(fl: FuncLang[String], log: LogicSyntax): Unit = {
    val cp = new Compiler(log)
    cp.get foreach {
       d => println(fl.decl(d))
    }
  }
}

/** an example backend that transforms a LogicSyntax L into the SML code that represents and validates L-theories */
class Compiler(log: LogicSyntax) extends Program {
   import EXPConversions._
   object lf {
      val lam = ID("lf.lam")
      val app = ID("lf.app")
      val qapp = ID("lf.qapp")
      val variable = ID("lf.var")
      val decl = ID("lf.decl")
      val instance = ID("lf.instance")
   }
   def argsToEXP(args: List[CatRef]) : List[EXP] = args.map((x: CatRef) => ID(x.target))
   def argsToStringWithID(args: List[CatRef]) = args.foldLeft(" of id")(_ + " * " + _)

   //auxiliary types for ids for better readability
   val declare(id) = "id" typedef ID("string")
   val declare(vrb) = "var" typedef ID("int")


   // the categories
   val cats = log.cats map {case Category(c, cons) =>
      val cases = cons map {
         case Connective(n,Nil) => CONS(n,Nil)
         case Connective(n, args) => CONS(n, argsToEXP(args))
         case Binder(n, argOpt, bound, scope) =>
            val arg : List[EXP] = argOpt match {case Some(a) => List(a) case None => Nil}
            CONS(n, arg ::: List(vrb, (scope:EXP)))
         case ConstantSymbol(p, n, args) => CONS(p + "_" + n, id :: argsToEXP(args))
         case VariableSymbol => CONS(c + "_var", List(vrb))
      }
      val declare(_*) = (c adt (cases :_*)).derive(List("Show","Typeable","Eq"))
   }

   // the declarations
   val decls = log.decls map {case Declaration(p, args) =>
     // datatype p = p of a1 ... an
      val declare(_*) = (p adt (p of ((id :: argsToEXP(args) ) : _*) )).derive(List("Show","Typeable","Eq"))
   }
   // the labeled union type of all declaration types
   val ofdecls = log.decls map {case Declaration(p, _) =>
      CONS(p + "_decl", List(p))
   }
   val declare(decl, _*) = ("decl" adt(ofdecls : _*)).derive(List("Show","Typeable","Eq"))
   addTag("basic")
   // the type of signatures
   val declare(sigs) = ("sigs" typedef LIST(decl)).derive("Show","Typeable")

   // the type of theories
   val declare(theo, sign, axioms) =
     "theo" record ("sign" ::: sigs, "axioms" ::: LIST(log.form))
   addTag("sig")

   //FIXME no support for more complex expressions in ADT arguments
   // cannot declare, matching error
//   val declare(basic_spec) = "basic_spec" adt List(LIST(decl))
//   val declare(basic_spec) = "basic_spec" typedef LIST(decl)
//   addTag("basic")
   // declare morphism
   val declare(morphism,source,target) = "morphism" record ("source" ::: sigs, "target" ::: sigs)
   addTag("mor")
   // symbols
   val declare(symb,sname) = ("symb" record ("sname" ::: id)).derive(List("Show","Typeable"))
   addTag("basic")

   val declare(error) = "error" exception

   addTag("basic")

   // parse tree
   val declare(tree, varr, app, bind, tbind) = "tree" adt (
       CONS("variable",List(id)),
       CONS("application",List(id,LIST(ID("")))),
       CONS("bind",List(id,id,ID(""))),
       CONS("tbind",List(id,id,ID(""),ID("")))
   )
   addTag("tree")

   // the functions that map parse trees to expressions

   def parse(c: CatRef, a: EXP) = APPLY("fromJust", APPLY(c.target + "_from_pt", a))
//   def qualIDFirst(e: EXP) = APPLY("Generic.qualIDSplitFirst", e)   //parse.qualIDSplit("instance_name") = ("instance","name")
//   def qualIDSecond(e: EXP) = APPLY("Generic.qualIDSplitSecond", e)
   val parsefuncs = log.cats map {case Category(c, cons) =>
     /** matches Application(n, None, args) */
      val appcase = cons.foldLeft[EXP](ERROR("error", STRINGCONCAT(STRING("illegal identifier: "), ID("n")))) {
        case (rest, Connective(con,cats)) =>
          IF("n" === STRING(con),
             IF(ID("args").length === cats.length,
                  SOME( // returns Some(exp)
                            APPLY(upc(con), cats.zipWithIndex map {case (r, i) =>
                            parse(r, AT("args", i))
                            } : _*)
                  ),
//                 ERROR("error", STRING("bad number of arguments, expected " + cats.length)) // returns None
                NONE
               ),
             rest
           )
        case (rest, _) => rest
      }
      /** matches Application(n, Some(Pair(p,i)), args) */
      val instappcase = cons.foldLeft[EXP](ERROR("error", STRINGCONCAT(STRING("illegal identifier: "), ID("n")))) {
        case (rest, ConstantSymbol(pat,con,cats)) =>
           IF(AND("n" === STRING(con), "pat" === STRING(pat)),
             IF(ID("args").length === cats.length,
                 SOME(
                       APPLY(upc(pat) + "_" + con, ID("inst") :: (cats.zipWithIndex map {case (r, i) =>
                       parse(r, AT("args", i))
                       }) : _*)
                 ),
//                 ERROR("error", STRING("bad number of arguments, expected " + cats.length))
                   NONE
               ),
             rest
           )
        case (rest, _) => rest
      }

      val bindcase = cons.foldLeft[EXP](ERROR("error", STRINGCONCAT(STRING("illegal identifier: "), ID("n")))) {
         case (rest, Binder(name, None, bound, scope)) =>
            IF("n" === STRING(name),
               APPLY(name, "v", parse(scope, "s")),
               rest
            )
         case (rest, _) => rest
      }

      val tbindcase = cons.foldLeft[EXP](ERROR("error", STRINGCONCAT(STRING("illegal identifier: "), ID("n")))) {
            case (rest, Binder(name, Some(a), bound, scope)) =>
               IF("n" === STRING(name),
                  APPLY(name, parse(a, "a"), "v", parse(scope, "s")),
                  rest
               )
            case (rest, _) => rest
        }
      val varcase = if (cons.exists(_ == VariableSymbol)) APPLY(c + "_var", "n")
          else ERROR("error", STRING("variables not allowed here"))

      // worst case - return None
//      val errcase : EXP = NONE

      val declare(_) = c + "_from_pt" function OPTION(c) <-- ("x" :: "Generic.Tree") == {
        "x" Match (
            ID("Generic.Application")("n", NONE, "args") ==> appcase,
            ID("Generic.Application")("n", SOME(TUPLE(List("pat", "inst"))), "args") ==> instappcase,
            ID("Generic.Bind")("_n", "_v", "_s") ==> bindcase,
            ID("Generic.Tbind")("_n", "_a", "_v", "_s") ==> tbindcase,
            ID("Generic.Variable")("_n") ==> varcase//,
//            ID("_") ==> errcase
        )
      }
   }

   // a function that parses declarations
   val declfrompt = log.decls.foldLeft[EXP](ERROR("error", STRING("illegal pattern"))) {
      case (rest, Declaration(p, args)) =>
         IF("p" === STRING(p),
           IF(ID("args").length === args.length,
              SOME(ID(p + "_decl")(
                  APPLY(p,
                        ((ID("i") ::
                           (args.zipWithIndex map {case (r, i) => parse(r, AT("args", i)) })
                         ) : _* )
                  )
                 )
              ),
              ERROR("error", STRING("bad number of arguments, expected " + args.length))
           ),
           rest
        )
   }
   val declare(decl_from_pt) = "decl_from_pt" function OPTION(decl) <-- ("d" :: "Generic.Decl") =|= {case d =>
       d Match (
          ID("Generic.Decl")("i", "p", "args") ==> declfrompt
       )
   }

   // functions that parse signatures and theories
   val declare(sign_from_pt) = "sign_from_pt" function sigs <-- ("(Generic.Sign sg)" :: "Generic.Sign") =|= {case sg =>
      sigs(MAP(ID("sg"), ID("fromJust") o decl_from_pt ))
   }

//   val declare(sign_from_pt) = "sign_from_pt" function "Sigs" <-- ("sg" :: "Generic.Sign") =|= {case sg =>
//     sg Match (
//         ID("Generic.Sign")("sg") ==> ERROR("","")
//     )
//   }

//   val declare(sign_from_pt) =

   val declare(axiom_from_pt) = "axiom_from_pt" function log.form <-- ("ax" :: "Generic.Tree") =|= {case ax =>
      parse(log.form, ax)
   }
   val declare(theo_from_pt) = "theo_from_pt" function theo <-- ("th" :: "Generic.Theo") =|= {case th =>
       theo("sign" ::: sign_from_pt(th __ sign.prepend("Generic.")), "axioms" ::: MAP(th __ axioms.prepend("Generic."), axiom_from_pt))
   }
/*
   def tolf(c: CatRef, e: EXP) = APPLY(c.target + "_to_lf", e)
   def varlist(args: List[CatRef]) : List[EXP] = (args.zipWithIndex map {case (c,i) => ID("x" + i)})
   def recvarlist(args: List[CatRef]) : List[EXP] = args.zipWithIndex map {case (c,i) => tolf(c, "x" + i)}

   // the functions that map expressions to LF
   val tolffuncs = log.cats map {case Category(c, cons) =>
      val declare(_) =
        c + "_to_lf" function "LF.EXP" <-- ("x" :: c) == {"x" Match( cons map {
          case Connective(n, Nil) => CASE(APPLY(n, varlist(Nil) : _*), lf.app(n,recvarlist(Nil)))
          case Connective(n, args) => CASE(APPLY(n, varlist(args) : _*), lf.app(n,recvarlist(args)))
          case Binder(n, Some(a), bound, scope) =>
            val lambda = lf.lam("v", lf.app(bound.target, List(tolf(a, "a"))), tolf(scope, "s"))
            ID(n)("a","v","s") ==> lf.app(n, List(tolf(a,"a"), lambda))
          case Binder(n, None, bound, scope) =>
            ID(n)("v","s") ==> lf.app(n, List(lf.lam("v", bound.target, tolf(scope, "s"))))
          case ConstantSymbol(p, n, args) =>
            ID(p + "_" + n)("i", varlist(args)) ==> lf.qapp("i", n, recvarlist(args))
          case VariableSymbol =>
            ID(c + "_var")("n") ==> lf.variable("n")
        }  :  _*) }
   }//TODO add case _ => error any new var name matched here

   // a function that maps declarations to LF instance declarations
   val declare(decl_to_lf) =
     "decl_to_lf" function "LF.BASIC_ITEM" <-- ("d" :: decl) == {"d" Match (
        log.decls map {case Declaration(p, args) => {
           ID(p + "_decl")("n", varlist(args)) ==> lf.instance(p, "n", recvarlist(args)) }
        } :_*
     )}


   // functions that map signatures and theories to LF signatures
   val declare(sign_to_lf) = "sign_to_lf" function "LF.Sign" <-- ("sg" :: sigs) == {
      MAP("sg", decl_to_lf)
   }
   val declare(axiom_to_lf) = "axiom_to_lf" function "LF.BASIC_ITEM" <-- ("ax" :: log.form) == {
      lf.decl("_", tolf(log.form, "ax"))
   }
   val declare(theo_to_lf) = "theo_to_lf" function "LF.Sign" <-- ("th" :: theo) =|= {case th =>
       sign_to_lf(th __ sign) ::: MAP(th __ axioms, axiom_to_lf)
   }
   */
   addTag("funs")

   private def upc(string : String) : String = string.head.toUpper + string.substring(1)

   private def getCons(ex : EXP) : EXP = {
     ID("")
   }
}
