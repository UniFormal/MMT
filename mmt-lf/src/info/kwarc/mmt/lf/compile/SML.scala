package info.kwarc.mmt.lf.compile

/** SML as an implementation of a FuncLang */
object SML extends FuncLang[String] {
   private var current : String = null
   def exp(e: EXP) : String = e match {
     case EQUAL(left,right) => "(" + exp(left) + " = " + exp(right) + ")"
     case INTS => "int"
     case INT(value) => value.toString
     case PLUS(x,y) => "(" + exp(x) + " + " + exp(y) + ")"
     case TIMES(x,y) => "(" + exp(x) + " * " + exp(y) + ")"
     case BOOLS => "bool"
     case STRINGS => "string"
     case STRING(value) => "\"" + value + "\""
     case STRINGCONCAT(left, right) => "(" + exp(left) + " ^ " + exp(right) + ")"
     case ID(name) => if (name == "") current else name
     case APPLY(fun, args @ _*) => fun + args.map(exp).mkString("(", ",", ")")
     case IF(cond, thn, els) => "(if " + exp(cond) + " then " + exp(thn) + " else " + exp(els) + ")"  
     case MATCH(arg, cases) => "case " + exp(arg) + "\n" + cases.map(cas).mkString("  of ", "\n   | ", "\n")
     case ERROR(e, msg) => "raise (" + e + " " + exp(msg) + ")"
     case LIST(tp) => "(" + exp(tp) + " list)"
     case ALIST(elems) => elems.map(exp).mkString("[", ",", "]")
     case LENGTH(list) => "List.length(" + exp(list) + ")"
     case AT(list, index) => "List.nth(" + exp(list) + "," + exp(index) + ")"
     case CONCAT(left, right) => "(" + exp(left) + " @ " + exp(right) + ")"
     case MAP(list, fun) => "(List.map " + exp(list) + " " + exp(fun) + ")"
     case PROD(tps) => tps.map(exp).mkString("(", " * ", ")")
     case TUPLE(es) => es.map(exp).mkString("(", ", ", ")")
     case PROJ(e, i) => "(#" + i.toString + " (" + exp(e) + "))"
     case ARECORD(tp, fields) => tp + fields.map {case FIELD(n,v) => "#" + n + " = " + exp(v)}.mkString("{", ",", "}")
     case SELECT(rec, field) => "(#" + field + " " + exp(rec) + ")"  
   }
   def cons(c: CONS) = c.name + " of " + c.args.map(exp).mkString("", " * ", "")
   def arg(a: ARG) = a.name + ": " + exp(a.tp) 
   private def ADTaux(a: ADT) = {
      current = a.name
      a.name + " = " + a.constructors.map(cons).mkString("", " | ", "\n")
   }
   private def FUNCTIONaux(f: FUNCTION) = {
      current = f.name
      f.name + f.args.map(arg).mkString("(",",",")") + " : " + exp(f.ret) + " = " + exp(f.body) + "\n"
   }
   def decl(d: DECL) = d match {
     case a : ADT => "datatype " + ADTaux(a)
     case ADTRec(adts) => adts.map(ADTaux).mkString("datatype ", "     and ", "\n")
     case TYPEDEF(name, df) => "type " + name + " = " + exp(df) + "\n"
     case f: FUNCTION => "fun " + FUNCTIONaux(f)
     case FUNCTIONRec(fs) => fs.map(FUNCTIONaux).mkString("fun ", "and ", "\n")
     case EXCEPTION(e) => "exn " + e + " of string\n"
     case RECORD(name, fields) => "datatype " + name + " = " + name + " of " + fields.map {case FIELD(n,v) => n + " : " + exp(v)}.mkString("{", ",", "}")
   }
   def cas(c: CASE) : String = exp(c.pattern) + " => " + exp(c.body)
}
