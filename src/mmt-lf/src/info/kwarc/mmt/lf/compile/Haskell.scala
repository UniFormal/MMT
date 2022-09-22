/**
 * Concrete syntax translator to a specific (Haskell) programming language
 * takes pseudo-language declarations, produces Haskell declarations
 *
 * created by Florian Rabe
 *
 * modified by Aivaras:
 * a.jakubauskas@jacobs-university.de
 */
package info.kwarc.mmt.lf.compile

case class Unsupported(msg: String) extends java.lang.Throwable

/** Haskell as an implementation of a FuncLang */
object Haskell extends FuncLang[String] {
   private var current : String = null

   def exp(e: EXP) : String = e match {
     case EQUAL(left,right) => "(" + exp(left) + " == " + exp(right) + ")"
     case AND(left,right) => "(" + exp(left) + " && " + exp(right) + ")"
     case OR(left,right) => "(" + exp(left) + " || " + exp(right) + ")"
     case INTS => "Integer"
     case INT(value) => value.toString
     case PLUS(x,y) => "(" + exp(x) + " + " + exp(y) + ")"
     case TIMES(x,y) => "(" + exp(x) + " * " + exp(y) + ")"
     case BOOLS => "Bool"
     case STRINGS => "String"
     case STRING(value) => "\"" + value + "\""
     case STRINGCONCAT(left, right) => "(" + exp(left) + " + " + exp(right) + ")"
     case ID(name) => if (name == "") {
       ids.find(_._1 == current) match {
         //TODO replace with fixConflict(current,fixMap)
         case Some((_,b)) => if (b) fix(upc(current)) else fix(current)
         case None => current
       }
     }
     else { if (! name.contains("."))
        ids.find(_._1 == name) match {
         case Some((_,b)) => if (b) fix(upc(name)) else fix(name)
         case None => name
       } // ID must contain function composition
        else {
         "(" + name + ")"
       }
     }
     case APPLY(fun, args @ _*) => "(" + fixConflict(fixNative(upperORlower(fun)),fixMap) + {
       if (! args.isEmpty) args.map(exp).mkString(" ", " ", " ")
       else ""
     } + ")"
     case IF(cond, thn, els) => "if " + exp(cond) +
                             idtrn + "then " + exp(thn) +
                             idtn + "else " + exp(els) +
                          left
     case MATCH(arg, cases) => "case " + exp(arg) + " of" + idtrn + cases.map(cas).mkString(idt, idtrn, idtln + left)
//     case ERROR(e, msg) => "error " + exp(STRINGCONCAT(STRING(e + ": "), msg))
     case ERROR(e, msg) => "Nothing"
     case LIST(tp) => "[" + exp(tp) + "]"
//     case ALIST(elems) => if (elems.length > 1) elems.map(exp).mkString("[", ",", "]") else elems.map(exp).mkString("")
     case ALIST(elems) => elems.map(exp).mkString("[", ",", "]")
     case OPTION(tp) => s"Maybe(${exp(tp)})"
     case SOME(elem) => s"(Just(${exp(elem)}))"
     case NONE => "Nothing"
     case UNOPTION(sm) => s"(fromJust(${exp(sm)})" // requires 'import Data.Maybe'
     case LENGTH(list) => "length(" + exp(list) + ")"
     case AT(list, index) => "(" + exp(list) + " !! " + exp(index) + ")"
     case CONCAT(left, right) => "(" + exp(left) + " ++ " + exp(right) + ")"
     case MAP(list, fun) => "(map " + exp(fun) + " " + exp(list) + ")"
//     case COMPOSE(fun1, fun2) => "(" + exp(fun2) + "." + exp(fun1) + ")"
     case PROD(tps) => s"(${tps.map(exp).mkString(",")})"
     case TUPLE(elems) => s"(${elems.map(exp).mkString(",")})"
     case PROJ(e, i) => s"${exp(e)}($i)"
     case ARECORD(tp, fields) => fix(upc(tp)) + fields.map {case FIELD(n,v) => n + " = " + exp(v)}.mkString("{", ",", "}")
     case SELECT(rec, field) => "(" + field + " " + exp(rec) + ")"
   }
   def cons(c: CONS) = fix(upc(c.name)) + " " + c.args.map(exp).mkString("", " ", "")
   def arg(a: ARG) = a.name + ": " + exp(a.tp)
   private def ADTaux(a: ADT) = {
      current = a.name
      fixNative(upc(a.name)) + " = " + a.constructors.map(cons).mkString("", " | ", "\n")
   }
   private def FUNCTIONaux(f: FUNCTION) = {
      current = f.name
      f.name + f.args.map(arg).mkString("(",",",")") + " : " + exp(f.ret) + " = " + exp(f.body) + "\n"
   }
   def decl(d: DECL) = d match {
     case a : ADT => "data " + ADTaux(a) + {
       if (!a.typeClasses.isEmpty) " deriving " + a.typeClasses.mkString("( ", ", ", ")\n") else ""
     }
     case ADTRec(adts) => adts.map(decl).mkString("", "", "")
     case a : TYPEDEF => "type " + upc(a.name) + " = " + exp(a.df) + {
       if (!a.typeClasses.isEmpty) " deriving " + a.typeClasses.mkString("( ", ", ", ")") else ""
     } + "\n"
     case f: FUNCTION => {
//       println(f.args.map(a => exp(a.tp) + f.ret))
        f.name + " :: " + f.args.map(a => exp(a.tp)).mkString("", " -> ", " -> ") +
           exp(f.ret) + "\n" +
        f.name + " " + f.args.map(_.name).mkString("", " ", " = ") + exp(f.body) + "\n"
     }
     case FUNCTIONRec(fs) => fs.map(decl).mkString("", "\n", "")
     case a : RECORD => "data " + upc(a.name) + " = " + upc(a.name) + a.fields.map {case FIELD(n,v) => n + " :: " + exp(v)}.mkString("{", ",", "}") + {
       if (!a.typeClasses.isEmpty) " deriving " + a.typeClasses.mkString("( ", ", ", ")\n") else ""
     }
     case EXCEPTION(e) => ""
     case COMMENT(s) => "-- " + s 
   }
   private var ids: List[(String,Boolean)] = Nil
   def reset: Unit = {ids = Nil}
   override def prog(ds: List[DECL]) : List[String] = {
//      val q =
        ds map {d =>
//        reset
        ids :::= declName(d)
//        val q = ids
        decl(d)
      }
//      q
   }
   private def declName(d : DECL) : List[(String,Boolean)] = d match {
     case TYPEDEF(a,b) => (a,true) :: declName(b)
     case d : ADT => (d.name,true) :: (d.constructors map declName flatten) ++ d.typeClasses.map((_, true))
     case d : FUNCTION => List((d.name,false)) // function names start with lowercase
     case d : RECORD => List((d.name,true))
     case d : EXCEPTION => List((d.name,true))
     case d : ADTRec => d.adts map declName flatten
     case d : FUNCTIONRec => d.funs map declName flatten
     case _ => List()
   }
   private def declName(c : CONS) : List[(String,Boolean)] = c match {
     case CONS(a,b) => (a,true) :: (b map declName flatten)
     case _ => List()
   }
   private def declName(e : EXP) : List[(String,Boolean)] = e match {
     case ID(a) => List((a,true)) // constructors start with capital in haskell
     case _ => List()
   }
   private def typeOrData(d : DECL) : Boolean = d match {
     case d : TYPEDEF => true
     case d : ADT => true
     case _  => false
   }
   private def upperORlower(s : String) : String =
     if (s == "") {
       ids.find(_._1 == current) match {
         //TODO replace with fixConflict(current,fixMap)
         case Some((_,b)) => if (b) fixNative(upc(current)) else fixNative(current)
         case None => current
       }
     }
     else {
       ids.find(_._1 == s) match {
         case Some((_,b)) => if (b) fixNative(upc(s)) else fixNative(s)
         case None => s
       }
     }
   def cas(c: CASE) : String = exp(c.pattern) + " -> " + exp(c.body)
   // auxiliary function: upc(string) = String
   def upc(string : String) : String = s"${string.head.toUpper}${string.substring(1)}"
   // haskell native datatypes and checker
   private val haskellTypes : List[String] = List("Integer","Bool")//,"String","Int")
   private def haskellType(name : String) : Boolean = haskellTypes.exists( name == )
   /** resolves conflicts with Haskell native types by mapping strings that occur in IDs to replacement strings
    *  if the key is not found, i.e. there is no conflict, it's just an identity map
    */
   private def fixConflict(s : String, m : scala.collection.mutable.HashMap[String,String]) : String = {
      m.applyOrElse(s, {(x : String) => x})
   }
   def fix(s : String) : String = {
     fixNative(fixConflict(s,fixMap))
   }
   // maps the possible conflicts to resolutions
   private val fixMap = scala.collection.mutable.HashMap[String,String](
       "true" -> "AS_BASIC.True",
//       "True" -> "AS_BASIC.True",
       "false" -> "AS_BASIC.False"//,
//       "False" -> "AS_BASIC.False",
//       "<Decl>" -> "Generic.Decl"//,
//       "Bool" -> "AS_BASIC.Form",
//       "Sigs" -> "PLpatt.Sigs.Sigs"
       )
   private val haskellNative : List[String] = List(
       "Bool",
       "True",
       "False"
       )
   private def fixNative(n : String) : String = if (haskellNative.exists({x => x == n})) s"$n'" else n
   //TODO this could be improved
//   private def checkNativeTypeConflict(name : String) : String = if (upc(name) == "Bool" || upc(name) == "Boolean") "Form" else  if (haskellType(name)) name + "'" else name
   // indentation state tracker, represents the level of indentation
   private var indent : Int = 0
   // helper function for creation of indentations
   private def idt : String = { " " * (indent * 2)}
   private def idt(n : Int) : String = { " " * (n * 2)}
   private def idt(s : String) : String = { s + idt}
   private def idt(c : Char) : String = { s"$c$idt"}
   private def idtn : String = { idt('\n')}
   private def right : String = { indent = indent + 1; ""}
   private def left : String = { if (indent > 0) indent = indent - 1; ""}
   private def idtr : String = { right; idt}
   private def idtrn : String = { right; idtn}
   private def idtl : String = { left; idt}
   private def idtln : String = { left; idtn}
   private def idtReset : String = { indent = 0; ""}
}
