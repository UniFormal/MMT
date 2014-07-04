package info.kwarc.mmt.api.notations

import info.kwarc.mmt.api._
import objects._

/**
 * the arity of a symbol describes what kind of objects can be formed from it
 * 
 * @param variables the variable positions in order
 * @param arguments the argument positions in order
 * @param attribution true if it can attribute an object
 * 
 * special cases and analogs in the OpenMath role system for a symbol s:
 *  no arguments, variables: constant, s
 *  some arguments, no variables: application OMA(s, args)
 *  some arguments, some variables: binder, OMBINDC(s, vars, args)
 *  as above but exactly 1 argument: usual binders, generalized binders as suggested by Davenport, Kohlhase, MKM 2010
 */
case class Arity(subargs: List[ScopeComponent],
                 variables: List[VariableComponent],
                 arguments: List[ArgumentComponent], attribution: Boolean) {
   def components = variables ::: arguments
   def length = components.length
   def isConstant    = (! attribution) &&    arguments.isEmpty  &&    variables.isEmpty
   def isApplication = (! attribution) && (subargs.isEmpty) && variables.isEmpty && (! arguments.isEmpty)
   def isPlainBinder = (! attribution) && (subargs.isEmpty) && (! variables.isEmpty) && arguments.length == 1
   def numSeqSubs = subargs.count(_.isInstanceOf[SeqArg])
   def numSeqArgs = arguments.count(_.isInstanceOf[SeqArg])
   def numSeqVars = variables.count {
      case Var(_,_, Some(_),_) => true
      case _ => false
   }
   def numNormalSubs = subargs.count {
      case Subs(n,_) => true
      case _ => false
   }
   def numNormalArgs = arguments.count {
      case Arg(n,_) => true
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
         case SeqArg(n,_,_) if n > 0 => List(n)
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
   private def remapFun(perSeqArg: Int, seqArgCutOff: Int, perSeqVar: Int, seqVarCutOff: Int)(p: Int): Int = {
         var i = p.abs
         components foreach {
            case SeqArg(n,_,_) if n < p =>
               i += perSeqArg - 1
               if (n < seqArgCutOff) i += 1
            case Var(n,_,Some(_),_) if n < p => 
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
         case SeqArg(n,_,_) =>
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
   /** @return true if ComplexTerm(name, subs, vars, args) has enough components for this arity and provides for an attribution if necessary */
   def canHandle(subs: Int, vars: Int, args: Int, att: Boolean) = {
     
      (numNormalSubs == subs || (numNormalSubs < subs && numSeqSubs >= 1)) &&
      (numNormalVars == vars || (numNormalVars < vars && numSeqVars >= 1)) &&
      (numNormalArgs == args || (numNormalArgs < args && numSeqArgs >= 1)) &&
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
         case Arg(n,p) =>
            List(Arg(remap(n),p))
         case SeqArg(n, sep,p) =>
            val length = if (n < seqArgCutOff) perSeqArg+1 else perSeqArg
            val first = remap(n)
            if (length == 0) Nil
            else Range(1,length).toList.flatMap(i => List(Arg(first+i-1,p), sep)) ::: List(Arg(first+length-1,p))
         case ImplicitArg(n,p) =>
            List(ImplicitArg(remap(n),p))
         case Var(n, tpd, None, p) =>
            List(Var(remap(n), tpd, None, p))
         case Subs(n, p) => 
            List(Subs(remap(n), p))
         case v @ Var(n, tpd, Some(sep), p) =>
            val length = if (n < seqVarCutOff) perSeqVar+1 else perSeqVar
            val first = remap(n)
            if (length == 0) Nil
            else Range(1,length).toList.flatMap(i => List(Var(first+i-1, tpd, None, p), sep)) ::: List(Var(first+length-1, tpd, None, p))
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
         case ImplicitArg(n, _) => List(ImplicitArg(remapFun(perSeqArg, seqArgCutOff, perSeqVar, seqVarCutOff)(n)))
         case _ => Nil
      }
   }
}

object Arity {
   def constant = Arity(Nil,Nil, Nil,false)
   def plainApplication = Arity(Nil, Nil, List(SeqArg(1,Delim(""))), false)
   def plainBinder = Arity(Nil, List(Var(1,false,Some(Delim("")))), List(Arg(2)), false)
   def attribution = Arity(Nil, Nil, List(Arg(1)),true)
}

