package info.kwarc.mmt.lf.compile

/** expressions of a simple functional language */
sealed abstract class EXP {
   def ==>(body: EXP) = CASE(this, body)
   def ===(right: EXP) = EQUAL(this, right)
   def INDEX(index: EXP) = AT(this, index)
}
/** equality */
case class EQUAL(left: EXP, right: EXP) extends EXP
/** integer literals */
case class INT(value: Int) extends EXP
/** string literals */
case class STRING(value: String) extends EXP
/** concatenation of strings */
case class STRINGCONCAT(left: EXP, right: EXP) extends EXP
/** identifiers */
case class ID(name: String) extends EXP
/** function application */
case class APPLY(fun: String, args: EXP*) extends EXP
/** if then else */
case class IF(cond: EXP, thn: EXP, els: EXP) extends EXP
/** pattern matching */
case class MATCH(arg: EXP, cases: List[CASE]) extends EXP
/** throwing of an exception */
case class ERROR(name: String, msg: EXP) extends EXP
/** type of lists */
case class LIST(tp: EXP) extends EXP
/** a list */
case class ALIST(elems: List[EXP]) extends EXP
/** length of a list */
case class LENGTH(list: EXP) extends EXP
/** element of a list at a given index */
case class AT(list: EXP, index: EXP) extends EXP
/** concatenation of lists */
case class CONCAT(left: EXP, right: EXP) extends EXP
/** map over a list */
case class MAP(list: EXP, fun: ID) extends EXP
/** product type */
case class PROD(tps: List[EXP]) extends EXP
/** tuple */
case class TUPLE(tps: List[EXP]) extends EXP
/** projection out of a product */
case class PROJ(exp: EXP, proj: Int) extends EXP
/** record value */
case class ARECORD(tp: String, fields: List[FIELD]) extends EXP
/** selection from a record */
case class SELECT(record: EXP, field: String) extends EXP

/** auxiliary class for records types and values */
case class FIELD(name: String, value: EXP)
/** auxiliary class for cases in a MATCH */
case class CASE(pattern: EXP, body: EXP)
/** auxiliary class for arguments of a FUNCTION */
case class ARG(name: String, tp: EXP)
/** auxiliary class for constructors of an ADT */
case class CONS(name: String, args: List[EXP])

/** declarations of a simple functional language */
sealed abstract class DECL
/** abstract data type */
case class ADT(name: String, constructors: List[CONS]) extends DECL
/** a group of mutually recursive abstract data types */
case class ADTRec(adts: List[ADT]) extends DECL
/** type definition */
case class TYPEDEF(name: String, df: EXP) extends DECL
/** function definition */
case class FUNCTION(name: String, args: List[ARG], ret: EXP, body: EXP) extends DECL
/** a group of mutually recursive functions */
case class FUNCTIONRec(funs: List[FUNCTION]) extends DECL
/** record type declaration (only named record types are permitted) */
case class RECORD(name: String, fields: List[FIELD]) extends DECL
/** exception declaration; all exception take a single string argument */
case class EXCEPTION(name: String) extends DECL

/** useful implicit converions */
object EXPConversions {
   implicit def stringToID(s: String) = ID(s)
   implicit def catRefToID(c: CatRef) = ID(c.target)
   implicit def intToINT(i: Int) = INT(i)
}

/** a simple functional language represented as transformation functions from declarations and expressions to strings */
abstract class FuncLang {
   def exp(e: EXP) : String
   def decl(d: DECL) : String
}