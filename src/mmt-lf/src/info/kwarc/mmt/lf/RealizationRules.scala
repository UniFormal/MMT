package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import modules._
import symbols._
import objects._
import checking._
import uom._

abstract class RealizationRuleFeature extends StructuralFeature("realize") {
  
   //def unnamedDeclarations: Option[List[DeclarationComponent] => LocalName] = None
   
   def expectedComponents = List(">" -> TypeComponent)
  
   def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {
     val tp = dd.getComponent(TypeComponent).getOrElse {
       env.errorCont(InvalidElement(dd, "no type give"))
       return
     }
     tp match {
       case OMPMOD(this.mpath, List(syn,sem)) =>
         sem match {
           case OMMOD(mp) =>
             val obj = controller.backend.loadObject(mp)
             obj match {
               case st: SemanticType =>
                 // check if syn is atomic?
                 val cont = Context(dd.parent.toMPath)
                 val j = Typing(Stack(Context.empty), syn, OMS(Typed.ktype))
                 val cr = env.objectChecker(CheckingUnit(None, cont, Context.empty, j), env.rules)(env.ce)
                 if (cr.solved) {
                   RealizedType(cr.term, st)
                 } else {
                   throw LocalError("type did not check")
                 }
               case semOp: SemanticOperator =>
                 val synP = syn match {
                   case OMS(p) => p
                   case _ => throw LocalError("realized operator must be an identifier")
                 }
                 val synTp = controller.globalLookup.getO(synP) match {
                   case Some(c: Constant) => c.tpC.getAnalyzedIfFullyChecked getOrElse {
                     throw LocalError("type not present or not fully checked")
                   }
                   case _ => throw LocalError("realized operator must be a constant")
                 }
                 val FunType(from, to) = synTp
                 if (from.exists(_._1.isDefined))
                   throw LocalError("can only handle simple function types")
                 val args = from.map(_._2)
                 if (args.length != semOp.arity)
                   throw LocalError("semantic operator has wrong arity")
                 val expSynOpTp = SynOpType(args,to)
                 val argExp = ???// get RealizedType(synA, semA) from dd.parent if that exists
                 val toExp = ???// get RealizedType(to, semOp.ret from dd.parent if that exists
                 val expSemOpTp = SemOpType(argExp, toExp)
                 if (!semOp.getTypes.contains(expSemOpTp))
                   throw LocalError("semantic operator has wrong type")
                 RealizedOperator(synP, expSynOpTp, semOp, expSemOpTp)
               case _ => throw LocalError("objects exists but is not a semantic type or operator")
             }
           case _ => throw LocalError("ill-formed")
         }
     }
   }

   def elaborate(parent: DeclaredModule, dd: DerivedDeclaration): Elaboration
}