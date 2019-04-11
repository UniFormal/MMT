package info.kwarc.mmt.sql

import info.kwarc.mmt.api._
import frontend._
import modules._
import symbols._
import objects._
import uom._
import valuebases._

import info.kwarc.mmt.lf._

import SQLSyntax._

/**
 * 
 */
class SQLBridge(controller: Controller, rules: RuleSet, commProps: List[CommutingProperty]) extends Logger {
   private val coder = new SQLCoder(rules)
   
   val report = controller.report
   val logPrefix = "sql"
   
   def theoryToTable(t: Theory): Table = {
     val cols = t.getConstants flatMap {
       case c: Constant => constantToColumn(c).toList
     }
     Table(t.path, cols)
   }
   
   def constantToColumn(c: Constant): Option[Column] = {
     val codecTerm = Codecs.codecAnnotator.get(c).getOrElse(return None) // no codec given
     val context = Context(c.home.toMPath)
     val mathType = c.tp.getOrElse {
        log("no mathematical type given " + c.name)
        log("ignoring constant " + c.name)
        return None
     }
     val u = LocalName("") / "dbtype"
     val expType = (Context(VarDecl(u)), Codecs.codec(mathType, OMV(u)))
     // |- codecTerm : Codecs.codec(mathType, dbtype), where dbtype is to be inferred
     val codecCheck = checking.Solver.check(controller, Stack(context), codecTerm, Some(expType), Some(rules))
     val (codecTermChecked, codecType) = codecCheck match {
       case utils.Left(r) => r
       case utils.Right(s) =>
         log("cannot infer type of codec expression " + codecTerm)
         log("ignoring constant " + c.name)
         s.logState(logPrefix)
         return None
     }
     val dbtype = codecType match {
       case Codecs.codec(_,tp) => SQLBridge.termToType(tp)
       case a =>
         log("codec term has bad type: " + codecTerm + " : " + a)
         log("ignoring constant " + c.name)
         return None
     }

     val isNullable = false  //TODO
     val isForeignKey = SchemaLang.foreignKey.get(c)
     val isOpaque = SchemaLang.opaque.get(c)
     val isHidden = SchemaLang.hidden.get(c)
     val collection = if (SchemaLang.collection.get(c)) {
       Some(CollectionInfo(c.path,c.metadata))
     } else 
       None
     val col = Column(c.path, mathType, codecTermChecked, dbtype, isForeignKey, isOpaque, !isHidden, collection)
     Some(col)
   }
      
   private def defaultCodec(rt: RealizedType): Term = rt.synType match {
     case OMS(MathData.bool)   => OMS(Codecs.BoolIdent)
     case OMS(MathData.int)    => OMS(Codecs.IntIdent)
     case OMS(MathData.string) => OMS(Codecs.StringIdent)
     case OMS(MathData.uuid)   => OMS(Codecs.UUIDIdent)
   }
  
  /**
    *  converts a term over a schema theory into the corresponding SQL expression and the codec that allows decoding it
    */
  // this does not work yet
   def termToExpr(table: Table, tm: Term, expCodec: Option[Term]): (Expr,Term) = {
     tm match {
       case OMS(p) =>
         table.columns.find(_.path == p) match {
           case Some(col) =>
             expCodec match {
               case Some(cod) if col.codec != cod => throw CodecMismatch(col, cod)
               case _ =>
             }
             (ColumnRef(col.name), col.codec)
           case None =>
             termToExpr(table, OMA(tm,Nil), expCodec) // TODO check if this works
         }
       case l: OMLIT =>
         val cod = expCodec.getOrElse(defaultCodec(l.rt))
         val codec = coder.buildCodec(cod)
         val tmE = codec.encode(l)
         (tmE, cod)
       case OMA(OMS(p), args) =>
         val context = Context(table.path)
         val matcher = new Matcher(controller,rules)
         val candidates = commProps.filter(cp => cp.mathOp == p && cp.arity == args.length)
         candidates.foreach {cp =>
           val (params,argsToEncode) = args.splitAt(cp.mathParams.length)
           /* TODO recursive call should alternate with matching
              if the free variables in an inCodec have already been solved, they are passed as the expected codec
              if an expected codec is passed, it is matched against the outCodec */ 
           val (argsE, argsC) = args.map {a => termToExpr(table, a, None)}.unzip
           val res = matcher(context, cp.context) {eq =>
             ((params zip cp.mathParams) forall {case (a,b) => eq(a,b)}) &&
             ((argsC zip cp.inCodecs)    forall {case (a,b) => eq(a,b)})
           }
           res match {
             case MatchSuccess(subs,_) =>
               val tmE = cp.dbOp(argsE:_*)
               val tmC = cp.outCodec ^ subs
               return (tmE, tmC)
             case _ =>
           }
         }
         throw CannotTranslate(tm)
     }
   }
   
   def termToFilter(table: Table, tm: Term): Filter = {
     // determine type and codec for each subterm, then translate according to commutativity annotations 
     val (e,_) = termToExpr(table, tm, None)
     Filter(e)
   }
}

object SQLBridge {
  import DbData._
  val basetypes = List(bool -> BoolType, int -> IntType, string -> StringType, uuid -> UUIDType)
  /** converts a term over the theory DbData into the corresponding SQL type */
  def termToType(tp: Term): Type[_] = tp match {
     case OMS(p) => utils.listmap(basetypes,p).get
     case DbData.array(tp) => ArrayType(termToType(tp))
  }
  /** inverse of termToType */
  def typeToTerm(tp: Type[_]): Term = tp match {
    case b: BaseType[_] => OMS(utils.invlistmap(basetypes, b).get)
    case ArrayType(t) => array(typeToTerm(t))
  }
  
  /** MPath of example schema */ 
  val example = SchemaLang._base ? "Example"
  /** convert a theory to a table */
  def test(thyP: MPath) = {
    val controller = Controller.make(true, true, List("MMT/urtheories", "MMT/LFX","ODK/DiscreteZoo"))
    try {
    val rules = RuleSet.collectRules(controller, Context(thyP))
    val bridge = new SQLBridge(controller, rules, Nil)
    controller.handleLine("log+ " + bridge.logPrefix)
    val thy = controller.globalLookup.getAs(classOf[Theory], thyP)
    val table = bridge.theoryToTable(thy)
    table
    } catch {
      case e:Error => println(e.toStringLong)
    }
  }
}

case class CannotTranslate(tm: Term) extends Throwable
case class CodecMismatch(col: Column, expectedCodec: Term) extends Throwable