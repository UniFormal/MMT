package info.kwarc.mmt.lf.compile

case class Unsupported(msg: String) extends java.lang.Throwable 

/** Haskell as an implementation of a FuncLang */
object Haskell extends FuncLang[String] {
   private var current : String = null
   def exp(e: EXP) : String = e match {
     case EQUAL(left,right) => "(" + exp(left) + " == " + exp(right) + ")"
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
         case Some((_,b)) => if (b) checkNativeTypeConflict(upc(current)) else checkNativeTypeConflict(current)
         case None => current
       } 
     }
     else {
       ids.find(_._1 == name) match {
         case Some((_,b)) => if (b) checkNativeTypeConflict(upc(name)) else checkNativeTypeConflict(name)
         case None => name
       }
     }
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
   def cons(c: CONS) = upc(c.name) + " " + c.args.map(exp).mkString("", " ", "")
   def arg(a: ARG) = a.name + ": " + exp(a.tp) 
   private def ADTaux(a: ADT) = {
      current = a.name
      checkNativeTypeConflict(upc(a.name)) + " = " + a.constructors.map(cons).mkString("", " | ", "\n")
   }
   private def FUNCTIONaux(f: FUNCTION) = {
      current = f.name
      f.name + f.args.map(arg).mkString("(",",",")") + " : " + exp(f.ret) + " = " + exp(f.body) + "\n"
   }
   def decl(d: DECL) = d match {
     case a : ADT => "data " + ADTaux(a)
     case ADTRec(adts) => adts.map(decl).mkString("", "", "")
     case TYPEDEF(name, df) => "type " + upc(name) + " = " + exp(df) + "\n"
     case f: FUNCTION =>
        f.name + " :: " + f.args.map(a => exp(a.tp)).mkString("", " -> ", " -> ") + exp(f.ret) + "\n" +
        f.name + " " + f.args.map(_.name).mkString("", " ", " = ") + exp(f.body) + "\n"
     case FUNCTIONRec(fs) => fs.map(decl).mkString("", "\n", "")
     case RECORD(name, fields) => "data " + upc(name) + " = " + upc(name) + fields.map {case FIELD(n,v) => n + " :: " + exp(v)}.mkString("{", ",", "}")
     case EXCEPTION(e) => ""
   }
   private var ids: List[(String,Boolean)] = Nil
   def reset {ids = Nil}
   override def prog(ds: List[DECL]) : List[String] = {
      val q = ds map {d =>
//        reset
        ids :::= declName(d)
        val q = ids
        decl(d)
      }
//      println(ids)
      q
   }
   private def declName(d : DECL) : List[(String,Boolean)] = d match {
     case TYPEDEF(a,b) => (a,true) :: declName(b)
     case ADT(a,b) => (a,true) :: (b map declName flatten)
//    	 val q = b map declName 
//     }
//     ( )foldRight(List())(:::)
     case d : FUNCTION => List((d.name,false)) // function names start with small letters
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
   def cas(c: CASE) : String = exp(c.pattern) + " -> " + exp(c.body)
   // auxiliary function: upc(string) = String
   private def upc(string : String) : String = string.head.toUpperCase + string.substring(1)
   // haskell native datatypes and checker
   private val haskellTypes : List[String] = List("Integer","Bool")//,"String","Int")
   private def haskellType(name : String) : Boolean = haskellTypes.exists( name == )
   //TODO this could be improved
   private def checkNativeTypeConflict(name : String) : String = if (upc(name) == "Bool" || upc(name) == "Boolean") "Form" else  if (haskellType(name)) name + "'" else name
}
