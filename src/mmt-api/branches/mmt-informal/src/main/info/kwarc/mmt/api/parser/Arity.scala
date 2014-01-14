package info.kwarc.mmt.api.parser

import info.kwarc.mmt.api._
import objects._

/**
 * the arity of a symbol describes what kind of objects can be formed from it
 * 
 * special cases and analogs in the OpenMath role system for a symbol s:
 *  no arguments, variables, scopes: constant, s
 *  some arguments, no variables, scopes: application OMA(s, args)
 *  no arguments, some variables: binder, OMBINDC(s, vars, scopes)
 *  some arguments, some variables: application+binder as in OMBINDC(OMA(s, args), vars, scopes), as suggested by Kohlhase, Rabe, MiCS 2012
 *  as above but exactly 1 scope: usual binders, generalized binders as suggested by Davenport, Kohlhase, MKM 2010
 */
case class Arity(variables: List[VariableComponent],
                 arguments: List[ArgumentComponent], attribution: Boolean) {
   def components = variables ::: arguments
   def length = components.length
   def isConstant    = (! attribution) &&    arguments.isEmpty  &&    variables.isEmpty
   def isApplication = (! attribution) && (! arguments.isEmpty) &&    variables.isEmpty
   def isAttribution = attribution && variables.isEmpty
   def isPlainAttribution = isAttribution && arguments.length == 1
   def isPlainBinder = (! attribution) && (! variables.isEmpty) && arguments.length == 1
   def numSeqArgs = arguments.count(_.isInstanceOf[SeqArg])
   def numSeqVars = variables.count {
      case Var(_,_, Some(_)) => true
      case _ => false
   }
   def numNormalArgs = arguments.count {
      case Arg(n) => true
      case ImplicitArg(_) => true
      case _ => false
   }
   def numNormalVars = variables.count {
      case Var(_,_, None) => true
      case Var(_,_, Some(_)) => false
      case _ => true
   }
   // distributes available components to numTotal normal/sequence components where the positions of the sequences are given by seqs
   // returns: number of arguments per sequence and cutoff below which sequences get one extra argument
   private def distribute(available: Int, numTotal: Int, seqs: List[Int]):(Int,Int) = {
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
   private def distributeArgs(numArgs: Int) : (Int, Int) = {
      val seqArgPositions = arguments.flatMap {
         case SeqArg(n,_) if n > 0 => List(n)
         case _ => Nil
      }
      distribute(numArgs, arguments.length, seqArgPositions)
   }
   /** 
    * like distributeArgs but for the variables
    */
   private def distributeVars(numVars: Int): (Int, Int) = {
      val seqVarPositions = variables.flatMap {
         case Var(n,_,Some(_)) => List(n)
         case _ => Nil
      }
      distribute(numVars, variables.length, seqVarPositions)
   }
   /** maps component positions to position in flattened notation, by including the arguments of the preceding sequences */
   private def remapFun(perSeqArg: Int, seqArgCutOff: Int, perSeqVar: Int, seqVarCutOff: Int)(p: Int): Int = {
         var i = p.abs
         components foreach {
            case SeqArg(n,_) if n < p =>
               i += perSeqArg - 1
               if (n < seqArgCutOff) i += 1
            case Var(n,_,Some(_)) if n < p => 
               i += perSeqVar - 1
               if (n < seqVarCutOff) i += 1
            case _ =>
         }
         i
      }

   /**
    * groups a list of arguments into sequences according to distributeArgs
    */
   def groupArgs(args: List[Term]) : List[List[Term]] = {
      var remain = args
      var result : List[List[Term]] = Nil
      val (perSeq,cutoff) = distributeArgs(remain.length)
      arguments foreach {
         case _:Arg | _ :ImplicitArg =>
            result ::= List(remain.head)
            remain = remain.tail
         case SeqArg(n,_) =>
            val len = if (n < cutoff) perSeq+1 else perSeq
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
   /** @return true if ComplexTerm(name, args, vars, scs) has enough components for this arity and provides for an attribution if necessary */
   def canHandle(vars: Int, args: Int, att: Boolean) = {
      numNormalArgs <= args &&
      numNormalVars <= vars &&
      (! att || attribution)
   }
  /**
    * flattens all sequence arguments/variables of a notation according to a complex term
    * 
    * @param markers the list of markers to flatten
    * @param args number of arguments
    * @param vars number of variables
    * @param scs number of scopes
    * 
    * if there is more than 1 sequence arguments, the available arguments are evenly distributed over the sequences
    * remaining arguments are distributed in order of content position
    * 
    * multiple sequence variables are treated accordingly
    * it is assumed there are no sequences in the scopes
    * 
    * pre: canHandle(vars, args) == true
    */
   def flatten(markers: List[Marker], vars: Int, args: Int, attrib: Boolean) : List[Marker] = {
      val (perSeqArg, seqArgCutOff) = distributeArgs(args)
      val (perSeqVar, seqVarCutOff) = distributeVars(vars)
      val remap = remapFun(perSeqArg, seqArgCutOff, perSeqVar, seqVarCutOff) _
      def flattenOne(m: Marker): List[Marker] = m match {
         case Arg(n) =>
            List(Arg(remap(n)))
         case SeqArg(n, sep) =>
            val length = if (n < seqArgCutOff) perSeqArg+1 else perSeqArg
            val first = remap(n)
            if (length == 0) Nil
            else Range(1,length).toList.flatMap(i => List(Arg(first+i-1), sep)) ::: List(Arg(first+length-1))
         case ImplicitArg(n) =>
            List(ImplicitArg(remap(n)))
         case Var(n, tpd, None) =>
            List(Var(remap(n), tpd, None))
         case v @ Var(n, tpd, Some(sep)) =>
            val length = if (n < seqVarCutOff) perSeqVar+1 else perSeqVar
            val first = remap(n)
            if (length == 0) Nil
            else Range(1,length).toList.flatMap(i => List(Var(first+i-1, tpd, None), sep)) ::: List(Var(first+length-1, tpd, None))
         case AttributedObject => if (attrib) List(AttributedObject) else Nil
         case d: Delimiter =>
            List(d)
         case p: PresentationMarker => List(p flatMap flattenOne)
      }
      markers flatMap flattenOne
   }
   /** 
    *  @param args the number of total arguments
    *  @return the list of remapped implicit arguments
    */
   def flatImplicitArguments(args: Int) : List[ImplicitArg] = {
      val (perSeqArg, seqArgCutOff) = distributeArgs(args)
      val (perSeqVar, seqVarCutOff) = distributeVars(0)
      arguments flatMap {
         case ImplicitArg(n) => List(ImplicitArg(remapFun(perSeqArg, seqArgCutOff, perSeqVar, seqVarCutOff)(n)))
         case _ => Nil
      }
   }
}

object Arity {
   def constant = Arity(Nil,Nil,false)
   def plainApplication = Arity(Nil, List(SeqArg(1,Delim(""))), false)
   def plainBinder = Arity(List(Var(1,false,Some(Delim("")))), List(Arg(2)), false)
   def attribution = Arity(Nil, List(Arg(1)),true)
}

