package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import frontend._
import modules._
import symbols._
import objects._
import checking._
import uom._

/** realize an LF-type/function as a [[SemanticType]] or [[SemanticOperator]] */
object Realize extends ParametricRule {

   /** finds the semantic type that was previously declared to realized a syntactic type */
   private def getSemanticType(controller: Controller, home: Term, synTp: Term): SemanticType = {
     controller.globalLookup.forDeclarationsInScope(home) {
       case (_,m,rc: RuleConstant) => rc.df match {
         // TODO translate rt along m 
         case Some(rt: RealizedType) if rt.synType == synTp => return rt.semType
         case _ =>
       }
       case _ =>
     }
     throw ParseError("no realized type known: " + synTp)
   }

   //TODO checks are called even when an .omdoc file is read
   def apply(controller: Controller, home: Term, args: List[Term]) = {
     if (args.length != 2) throw ParseError("two arguments expected")
     val List(syn,sem) = args
     val mp = sem match {
       case OMMOD(mp) => mp
       case _ =>  throw ParseError("semantic element must be identifier")
     }
     val obj = controller.backend.loadObjectO(mp).getOrElse {
       throw ParseError("semantic object not found")
     }
     obj match {
       case st: SemanticType =>
         val cont = home match {
           case ComplexTheory(c) => c
           case _ => Context.empty //TODO
         }
         val synC = Solver.check(controller, Stack(cont), syn)
         synC match {
           case Right(solver) => throw ParseError("type must be LF-type: " + syn)
           case Left((synR,_)) => RealizedType(synR, st)
         }
       case semOp: SemanticOperator =>
         val synP = syn match {
           case OMS(p) => p
           case _ => throw ParseError("realized operator must be an identifier")
         }
         val synTp = controller.globalLookup.getO(synP) match {
           case Some(c: Constant) => c.tpC.getAnalyzedIfFullyChecked.getOrElse {
             throw ParseError("type not present or not fully checked")
           }
           case _ => throw ParseError("realized operator must be a constant")
         }
         val FunType(from, to) = synTp
         if (from.exists(_._1.isDefined))
           throw ParseError("can only handle simple function types")
         val args = from.map(_._2)
         if (args.length != semOp.arity)
           throw ParseError("semantic operator has wrong arity")
         val expSynOpTp = SynOpType(args,to)
         def gST(tp: Term) = getSemanticType(controller, home, tp)
         val expSemOpTp = SemOpType(args map gST, gST(to))
         if (!semOp.getTypes.contains(expSemOpTp))
           throw ParseError("semantic operator has wrong type")
         RealizedOperator(synP, expSynOpTp, semOp, expSemOpTp)
       case _ => throw ParseError("objects exists but is not a semantic type or operator")
     }
   }
}
