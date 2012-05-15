package info.kwarc.mmt.lf.compile

case class Unsupported(msg: String) extends java.lang.Throwable 

/** Haskell as an implementation of a FuncLang */
object Haskell extends FuncLang {
   def exp(e: EXP) : String = e match {
     case EQUAL(left,right) => "(" + exp(left) + " == " + exp(right) + ")"
     case INT(value) => value.toString
     case STRING(value) => "\"" + value + "\""
     case STRINGCONCAT(left, right) => "(" + exp(left) + " + " + exp(right) + ")"
     case ID(name) => name
     case APPLY(fun, args @ _*) => fun + args.map(exp).mkString("(", ",", ")")
     case IF(cond, thn, els) => "(if " + exp(cond) + " then " + exp(thn) + " else " + exp(els) + ")"  
     case MATCH(arg, cases) => "case " + exp(arg) + "\n" + cases.map(cas).mkString("  of ", "\n   ", "\n")
     case ERROR(e, msg) => "error " + exp(STRINGCONCAT(STRING(e + ": "), msg))
     case LIST(tp) => "[" + exp(tp) + "]"
     case ALIST(elems) => elems.map(exp).mkString("[", ",", "]")
     case LENGTH(list) => "length(" + exp(list) + ")"
     case AT(list, index) => "(" + exp(list) + " !! " + exp(index) + ")"
     case CONCAT(left, right) => "(" + exp(left) + " ++ " + exp(right) + ")"
     case MAP(list, fun) => "(map " + exp(fun) + " " + exp(list) + ")"
     case PROD(tps) => throw Unsupported("product types")
     case TUPLE(es) => throw Unsupported("product types")
     case PROJ(e, i) => throw Unsupported("product types")
     case ARECORD(tp, fields) => tp + fields.map {case FIELD(n,v) => n + " = " + exp(v)}.mkString("{", ",", "}")
     case SELECT(rec, field) => "(" + field + " " + exp(rec) + ")"  
   }
   def cons(c: CONS) = c.name + " " + c.args.map(exp).mkString("", " ", "")
   def arg(a: ARG) = a.name + ": " + exp(a.tp) 
   private def ADTaux(a: ADT) = a.name + " = " + a.constructors.map(cons).mkString("", " | ", "\n")
   private def FUNCTIONaux(f: FUNCTION) = f.name + f.args.map(arg).mkString("(",",",")") + " : " + exp(f.ret) + " = " + exp(f.body) + "\n"
   def decl(d: DECL) = d match {
     case a : ADT => "data " + ADTaux(a)
     case ADTRec(adts) => adts.map(decl).mkString("", "", "")
     case TYPEDEF(name, df) => "type " + name + " = " + exp(df) + "\n"
     case FUNCTION(name, args, ret, body) =>
        name + " :: " + args.map(a => exp(a.tp)).mkString("", " -> ", " -> ") + exp(ret) + "\n" +
        name + " " + args.map(_.name).mkString("", " ", " = ") + exp(body) + "\n"
     case FUNCTIONRec(fs) => fs.map(decl).mkString("", "\n", "")
     case RECORD(name, fields) => "data " + name + " = " + name + fields.map {case FIELD(n,v) => n + " :: " + exp(v)}.mkString("{", ",", "}")
     case EXCEPTION(e) => ""
   }
   def cas(c: CASE) : String = exp(c.pattern) + " -> " + exp(c.body)
}
