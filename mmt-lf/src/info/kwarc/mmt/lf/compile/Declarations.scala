package info.kwarc.mmt.lf.compile

/** declarations of a simple functional language */
sealed abstract class DECL
/** abstract data types: name -> id%%(args) | ... | id(args)
 *  name and id's ADT and CONS respectively
 *  but using implicit conversions in Program, they can also be strings
 *  anonymous record types are not permitted */
case class ADT(name: String, constructors: List[CONS] = Nil) extends DECL
/** a group of mutually recursive abstract data types */
case class ADTRec(adts: List[ADT]) extends DECL
/** type definition */
case class TYPEDEF(name: String, df: EXP) extends DECL
/** function definition */
case class FUNCTION(name: String)(val args: ARG*)(val ret: EXP)(val body: EXP) extends DECL
/** a group of mutually recursive functions */
case class FUNCTIONRec(funs: List[FUNCTION]) extends DECL
/** record type declaration: RECORD(n) ^ c1 :: A1 ^ ... ^ cN :: AN  
 * anonymous record types are not permitted */
case class RECORD(name: String, fields: List[FIELD] = Nil) extends DECL
/** exception declaration; all exception take a single string argument */
case class EXCEPTION(name: String) extends DECL

/** auxiliary class for anonymous functions */
case class FUNCVALUE(args: List[ARG], ret: EXP, body: EXP)
/** auxiliary class for function types */
case class FUNCTYPE(args: List[ARG], ret: EXP) {
   /** anonymous functions: return <-- (args) == body */
   def ==(body: EXP) = FUNCVALUE(args, ret, body)
   /** anonymous functions with 1 argument, binding a Scala variable:
    * return <-- (args) =|= {case x => body}
    * args must have length 1
    */
   def =|=(body: EXP => EXP) = args match {
      case List(ARG(x, _)) => FUNCVALUE(args, ret, body(ID(x)))
      case _ => throw SyntaxError("wrong number of arguments in function declaration")
   }
   /** anonymous functions with 2 arguments, binding Scala variables:
    * return <-- (args) =||= {case (x,y) => body}
    * args must have length 2
    */
   def =||=(body: (EXP, EXP) => EXP) = args match {
      case List(ARG(x, _), ARG(y,_)) => FUNCVALUE(args, ret, body(ID(x), ID(y)))
      case _ => throw SyntaxError("wrong number of arguments in function declaration")
   }
}

/** An incomplete type checker for expressions */
object DECL {
   /** @param d the declaration to be checked
    *  @param context the previous declarations
    *  @return the declarations introduced by this one 
    */
   def check(d: DECL)(implicit context: Context): Context = {d match {
     case TYPEDEF(n,tp) =>
        EXP.checkType(tp)
        Context(List((n, KindOfTypes)))
     case ADT(n, cons) =>
        val condecls = cons map {case CONS(c, args) =>
           (c, FunctionalType(args, ID(n)))
        }
        Context((n, KindOfTypes) :: condecls)
     case RECORD(n, fields) =>
        val sels = fields map {case FIELD(f, v) =>
           EXP.checkType(v)
           (f, FunctionalType(List(ID(n)), v))
        }
        Context((n, KindOfTypes) :: sels)
     case f: FUNCTION =>
        val argdecls = f.args.toList map {case ARG(n,t) =>
           EXP.checkType(t) //assuming no dependencies on previous arguments
           (n,BuiltinType(t))
        }
        EXP.checkType(f.ret)
        val funtype = FunctionalType(f.args.toList.map(_.tp), f.ret)
        val fundecl = (f.name, funtype)
        EXP.check(f.body, f.ret)(context ++ Context(fundecl :: ("", funtype) :: argdecls))
        Context(List(fundecl))
     case EXCEPTION(n) =>
       Context(List((n, ErrorType)))
   }}
}

/** auxiliary class for constructing DECL's as
 *    name keyword arguments
 *  @param name the name of the declaration
 *  works with EXPConversions.stringToDECLHEAD 
 */
case class DECLHEAD(name: String) {
   /** type definitions as 
    *  name typedef type
    */
   def typedef(e: EXP) = TYPEDEF(name, e)
   /** adt definitions as 
    *  name adt (constructors)
    */
   def adt(cs: CONS*) = ADT(name, cs.toList)
   /** record type definitions as 
    *  name record (fields)
    */
   def record(fs: FIELD*) = RECORD(name, fs.toList)
   /** function definitions as 
    *  name function returntype <-- (arguments) == body
    *  uses FUNCTYPE and FUNCVALUE
    *  also possible:
    *  name function returntype <-- (arguments) =|= case x => body
    *  name function returntype <-- (arguments) =||= case (x,y) => body
    */
   def function(f: FUNCVALUE) = FUNCTION(name)(f.args : _*)(f.ret)(f.body)
   /** exception declarations as 
    *  name exception
    */
   def exception() = EXCEPTION(name)
}