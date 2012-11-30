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
  def apply(fl: FuncLang[String], log: LogicSyntax) {
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
         case Connective(n, args) => CONS(n, argsToEXP(args))
         case Binder(n, argOpt, bound, scope) =>
            val arg : List[EXP] = argOpt match {case Some(a) => List(a) case None => Nil}
            CONS(n, arg ::: List(vrb, (scope:EXP)))
         case ConstantSymbol(p, n, args) => CONS(p + "_" + n, id :: argsToEXP(args))
         case VariableSymbol => CONS(c + "_var", List(vrb))
         case Constructor0(n) => CONS(n, Nil)
      }
      val declare(_*) = c adt (cases :_*)
   }
   
   // the declarations
   val decls = log.decls map {case Declaration(p, args) =>
     // datatype p = p of a1 ... an
      val declare(_*) = p adt (p of ((id :: argsToEXP(args) ) : _*) )   
   }
   // the labeled union type of all declaration types
   val ofdecls = log.decls map {case Declaration(p, _) =>
      CONS(p + "_decl", List(p))
   }
   val declare(decl, _*) = "decl" adt(ofdecls : _*)
   addTag("basic")
   // the type of signatures
   val declare(sigs) = "sigs" typedef LIST(decl)
   
   // the type of theories
   val declare(theo, sign, axioms) =
     "theo" record ("sign" ::: sigs, "axioms" ::: LIST(log.form))  
   addTag("sig")
   
   val declare(basic_spec) = "basic_spec" typedef LIST(decl) 
   addTag("basic")
   // declare morphism
   val declare(morphism,source,target) = "morphism" record ("source" ::: sigs, "target" ::: sigs)
   addTag("mor")
   // symbols
   val declare(symb,sname) = "symb" record ("sname" ::: id)
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
   
   def parse(c: CatRef, a: EXP) = APPLY(c.target + "_from_pt", a)
   def qualIDFirst(e: EXP) = APPLY("parse.qualIDSplitFirst", e)   //parse.qualIDSplit("instance_name") = ("instance","name")
   def qualIDSecond(e: EXP) = APPLY("parse.qualIDSplitSecond", e)
   //TODO generate 'case _ => error' at the end
   val parsefuncs = log.cats map {case Category(c, cons) =>
      val appcase = cons.foldLeft[EXP](ERROR("error", STRINGCONCAT(STRING("illegal identifier: "), ID("n")))) {
        case (rest, Connective(con,cats)) =>
           IF("n" === STRING(con),
             IF(ID("args").length === cats.length,
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
             IF(ID("args").length === cats.length,
                 APPLY(pat + "_" + con, qualIDFirst("n") :: (cats.zipWithIndex map {case (r, i) =>
                    parse(r, AT("args", i))
                 }) : _*),
                 ERROR("error", STRING("bad number of arguments, expected " + cats.length))
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

      val declare(_) = c + "_from_pt" function c <-- ("x" :: "parse.tree") == {
        "x" Match (
            ID("parse.app")("n", "args") ==> appcase,
            ID("parse.bind")("n", "v", "s") ==> bindcase,
            ID("parse.tbind")("n", "a", "v", "s") ==> tbindcase,
            ID("parse.var")("n") ==> varcase
        )
      }
   }
   
   // a function that parses declarations
   val declfrompt = log.decls.foldLeft[EXP](ERROR("error", STRING("illegal pattern"))) {
      case (rest, Declaration(p, args)) =>
         IF("p" === STRING(p),
           IF(ID("args").length === args.length,
              ID(p + "_decl")("i", args.zipWithIndex map {case (r, i) =>
                       parse(r, AT("args", i))
                 }),
              ERROR("error", STRING("bad number of arguments, expected " + args.length))
           ),
           rest
        )      
   }
   val declare(decl_from_pt) = "decl_from_pt" function decl <-- ("d" :: "parse.decl") =|= {case d =>
       d Match (
          ID("instance")("i", "p", "args") ==> declfrompt
       )
   }
   
   // functions that parse signatures and theories
   val declare(sign_from_pt) = "sign_from_pt" function sigs <-- ("sg" :: "parse.sign") =|= {case sg =>
      MAP(sg, decl_from_pt)
   }
   
   val declare(axiom_from_pt) = "axiom_from_pt" function log.form <-- ("ax" :: "parse.tree") =|= {case ax =>
      parse(log.form, ax)
   }
   val declare(theo_from_pt) = "theo_from_pt" function theo <-- ("th" :: "parse.theo") =|= {case th => 
       theo("sign" ::: sign_from_pt(th __ sign), "axioms" ::: MAP(th __ axioms, axiom_from_pt))
   }

   def tolf(c: CatRef, e: EXP) = APPLY(c.target + "_to_lf", e)
   def varlist(args: List[CatRef]) : List[EXP] = (args.zipWithIndex map {case (c,i) => ID("x" + i)})
   def recvarlist(args: List[CatRef]) : List[EXP] = args.zipWithIndex map {case (c,i) => tolf(c, "x" + i)}
   
   // the functions that map expressions to LF
   val tolffuncs = log.cats map {case Category(c, cons) =>
      val declare(_) =
        c + "_to_lf" function "lf.exp" <-- ("x" :: c) == {"x" Match( cons map {
          case Connective(n, args) => CASE(APPLY(n, varlist(args) : _*), lf.app(n,recvarlist(args)))
          case Binder(n, Some(a), bound, scope) =>
            val lambda = lf.lam("v", lf.app(bound.target, List(tolf(a, "a"))), tolf(scope, "s"))
            ID(n)("a","v","s") ==> lf.app(n, List(tolf(a,"a"), lambda))
          case Binder(n, None, bound, scope) =>
            ID(n)("v","s") ==> lf.app(n, List(lf.lam("v", bound.target, tolf(scope, "s"))))
          case ConstantSymbol(p, n, args) =>
            ID(p + "_" + n)("i", varlist(args)) ==> lf.qapp("i", n, recvarlist(args))
          case Constructor0(n) => CASE(APPLY(n, varlist(Nil) : _*), lf.app(n,recvarlist(Nil)))  
          case VariableSymbol =>
            ID(c + "_var")("n") ==> lf.variable("n")
        }  :  _*) }
   }//TODO add case _ => error any new var name matched here
   
   // a function that maps declarations to LF instance declarations
   val declare(decl_to_lf) =
     "decl_to_lf" function "lf.decl" <-- ("d" :: decl) == {"d" Match (
        log.decls map {case Declaration(p, args) => {
           ID(p + "_decl")("n", varlist(args)) ==> lf.instance(p, "n", recvarlist(args)) } 
        } :_*
     )}
   

   // functions that map signatures and theories to LF signatures
   val declare(sign_to_lf) = "sign_to_lf" function "lf.sign" <-- ("sg" :: sigs) == {
      MAP("sg", decl_to_lf) 
   }
   val declare(axiom_to_lf) = "axiom_to_lf" function "lf.decl" <-- ("ax" :: log.form) == {
      lf.decl("_", tolf(log.form, "ax"))
   }
   val declare(theo_to_lf) = "theo_to_lf" function "lf.sign" <-- ("th" :: theo) =|= {case th => 
       sign_to_lf(th __ sign) ::: MAP(th __ axioms, axiom_to_lf)
   }
   addTag("funs")
   
}