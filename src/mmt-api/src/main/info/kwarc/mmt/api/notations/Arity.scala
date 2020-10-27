package info.kwarc.mmt.api.notations

import info.kwarc.mmt.api._
import objects._

/**
 * the arity of a symbol describes what kind of objects can be formed from it
 *
 * @param subargs arguments before the context
 * @param variables the variable positions in order
 * @param arguments arguments after the context
 *
 * special cases and analogs in the OpenMath role system for a symbol s:
 *  no arguments, variables: constant, s
 *  some arguments, no variables: application OMA(s, args)
 *  some arguments, some variables: binder, OMBINDC(s, vars, args)
 *  as above but exactly 1 argument: usual binders, generalized binders as suggested by Davenport, Kohlhase, MKM 2010
 */
case class Arity(subargs: List[ArgumentComponent],
                 variables: List[VariableComponent],
                 arguments: List[ArgumentComponent]) {
   def components = subargs ::: variables ::: arguments
   def length = components.length
   def isConstant    = subargs.isEmpty && arguments.isEmpty && variables.isEmpty
   def isApplication = subargs.isEmpty && variables.isEmpty && (! arguments.isEmpty)
   def isPlainBinder = subargs.isEmpty && (! variables.isEmpty) && arguments.length == 1
   def numSeqSubs = subargs.count(_.isInstanceOf[SeqArg])
   def numSeqArgs = arguments.count(_.isInstanceOf[SeqArg])
   def numSeqVars = variables.count {
      case Var(_,_, Some(_),_) => true
      case _ => false
   }
   def numNormalSubs = subargs.count {
      case _: Arg => true
      case ImplicitArg(_,_) => true
      case _ => false
   }
   def numNormalArgs = arguments.count {
      case _: Arg => true
      case ImplicitArg(_,_) => true
      case _ => false
   }
   def numNormalVars = variables.count {
      case Var(_,_, None,_) => true
      case Var(_,_, Some(_),_) => false
      case _ => true
   }
   def firstVarNumberIfAny = subargs.lastOption match {
      case Some(a) => a.number+1
      case None => 1
   }
   def firstArgNumberIfAny = (subargs:::variables).lastOption match {
      case Some(a) => a.number+1
      case None => 1
   }
   // distributes available components to numTotal normal/sequence components where the positions of the sequences are given by seqs
   // returns: number of arguments per sequence and cutoff below which sequences get one extra argument
   private def distribute(available: Int, numTotal: Int, seqs: List[Int]): (Int,Int) = {
     val numSeqs = seqs.length
     if (numSeqs == 0) return (0,0)
     val numSingle = numTotal - numSeqs
     val availableForSeqs = available - numSingle
     //the number of arguments that every sequence argument gets
     val perSeq = availableForSeqs / numSeqs
     //the first sequence that does not get an extra argument
     val cutoff = seqs.apply(availableForSeqs % numSeqs)
     (perSeq,cutoff)
   }
   /**
    * distributes all arguments evenly to the sequence arguments
    *
    * @param numArgs the total number of arguments
    *
    * if there is more than 1 sequence arguments, the available arguments are evenly distributed over the sequences
    * remaining arguments are distributed in order of content position
    */
   private def distributeArgs(argComps: List[ArgumentComponent], numArgs: Int) : (Int, Int) = {
      val seqArgPositions = argComps.flatMap {
         case sa: SeqArg => List(sa.number)
         case _ => Nil
      }
      distribute(numArgs, arguments.length, seqArgPositions)
   }
   /**
    * like distributeArgs but for the variables
    */
   private def distributeVars(numVars: Int): (Int, Int) = {
      val seqVarPositions = variables.flatMap {
         case Var(n,_,Some(_),_) => List(n)
         case _ => Nil
      }
      distribute(numVars, variables.length, seqVarPositions)
   }
   /** maps component positions to position in flattened notation, by including the arguments of the preceding sequences */
   private def remapFun(perSeqSub: Int, seqSubCutOff: Int, perSeqVar: Int, seqVarCutOff: Int,
                        perSeqArg: Int, seqArgCutOff: Int)(p: Int): Int = {
         var i = p
         subargs foreach {
            case sa:SeqArg if sa.number < p =>
               i += perSeqSub - 1
               if (sa.number < seqSubCutOff) i += 1
            case _ =>
         }
         variables foreach {
            case Var(n,_,Some(_),_) if n < p =>
               i += perSeqVar - 1
               if (n < seqVarCutOff) i += 1
            case _ =>
         }
         arguments foreach {
            case sa:SeqArg if sa.number < p =>
               i += perSeqArg - 1
               if (sa.number < seqArgCutOff) i += 1
            case _ =>
         }
         i
      }

   /**
    * groups a list of arguments into sequences according to distributeArgs
    * @param args the arguments to group
    * @param useSubArgs arguments occur before/after bound variables
    */
   def groupArgs(args: List[Term], useSubArgs: Boolean) : List[List[Term]] = {
      val comps = if (useSubArgs) subargs else arguments 
      var remain = args
      var result : List[List[Term]] = Nil
      val (perSeq,cutoff) = distributeArgs(comps, remain.length)
      comps foreach {
         case _:Arg | _ :ImplicitArg =>
            result ::= List(remain.head)
            remain = remain.tail
         case sa:SeqArg =>
            val len = if (sa.number < cutoff) perSeq+1 else perSeq
            result ::= remain.take(len)
            remain = remain.drop(len)
      }
      result.reverse
   }
   /**
    * groups a list of variable declarations into sequences according to distributeVars
    */
   def groupVars(cont: Context) : List[Context] = {
      var remain = cont.variables
      var result : List[Context] = Nil
      val (perSeq,cutoff) = distributeVars(remain.length)
      variables foreach {
         case v: Var if ! v.isSequence =>
            result ::= Context(remain.head)
            remain = remain.tail
         case v: Var if v.isSequence =>
            val len = if (v.number < cutoff) perSeq+1 else perSeq
            result ::= Context(remain.take(len) :_*)
            remain = remain.drop(len)
      }
      result.reverse
   }
  /**
    * flattens all sequence arguments/variables of a notation according to a complex term
    *
    * @param markers the list of markers to flatten
    * @param subs number of arguments before variables
    * @param vars number of variables
    * @param args number of arguments after variables
    *
    * if there is more than 1 sequence arguments, the available arguments are evenly distributed over the sequences
    * remaining arguments are distributed in order of content position
    *
    * multiple sequence variables are treated accordingly
    * it is assumed there are no sequences in the scopes
    *
    * pre: canHandle(vars, args) == true
    */
   def flatten(markers: List[Marker], subs: Int, vars: Int, args: Int) : List[Marker] = {
      val (perSeqSub, seqSubCutOff) = distributeArgs(subargs, args)
      val (perSeqVar, seqVarCutOff) = distributeVars(vars)
      val (perSeqArg, seqArgCutOff) = distributeArgs(arguments, args)
      val remap = remapFun(perSeqSub, seqSubCutOff, perSeqVar, seqVarCutOff, perSeqArg, seqArgCutOff) _
      def flattenOne(m: Marker): List[Marker] = m match {
         case am: Arg =>
            List(am * remap)
         case ia: ImplicitArg =>
            List(ia * remap)
         case sa:SeqArg =>
            val length = if (sa.number < seqArgCutOff) perSeqArg+1 else perSeqArg
            val first = remap(sa.number)
            utils.insertSep((0 until length).toList.map(i => sa.makeCorrespondingArg(first + i, remap)), sa.sep)
         case v: Var => v.sep match {
           case None => List(v * remap)
           case Some(sep) =>
             val length = if (v.number < seqVarCutOff) perSeqVar+1 else perSeqVar
             val first = remap(v.number)
             utils.insertSep((0 until length).toList.map(i => v.makeCorrespondingSingleVar(first+i,remap)), sep)
         }
         case d: Delimiter =>
            List(d)
         case p: PresentationMarker => List(p flatMap flattenOne)
         case w: WordMarker => List(w)
      }
      markers flatMap flattenOne
   }
   /**
    *  @param args the number of total arguments
    *  @return the list of remapped implicit arguments
    */
   def flatImplicitArguments(args: Int) : List[ImplicitArg] = {
      val (perSeqSub, seqSubCutOff) = distributeArgs(subargs, args)
      val (perSeqVar, seqVarCutOff) = distributeVars(0)
      val (perSeqArg, seqArgCutOff) = distributeArgs(arguments, args)
      arguments flatMap {
         case ImplicitArg(n,p) =>
            val remap = remapFun(perSeqSub, seqSubCutOff, perSeqVar, seqVarCutOff, perSeqArg, seqArgCutOff) _
            List(ImplicitArg(remap(n), p*remap))
         case _ => Nil
      }
   }
}

object Arity {
   import CommonMarkerProperties.noProps
   def constant = Arity(Nil,Nil, Nil)
   def plainApplication = Arity(Nil, Nil, List(SimpSeqArg(1,Delim(""), noProps)))
   def plainBinder = Arity(Nil, List(Var(1,false,Some(Delim("")), noProps)), List(SimpArg(2, noProps)))
}

