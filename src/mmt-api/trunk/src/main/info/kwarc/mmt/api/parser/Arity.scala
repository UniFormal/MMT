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
case class Arity(arguments: List[ArgumentComponent], variables: List[VariableComponent], scopes: List[ScopeComponent]) {
   def components = arguments ::: variables ::: scopes
   def length = components.length
   def isConstant    =    arguments.isEmpty  &&    variables.isEmpty  && scopes.isEmpty
   def isApplication = (! arguments.isEmpty) &&    variables.isEmpty  && scopes.isEmpty
   def isBinder      =    arguments.isEmpty  && (! variables.isEmpty)
   def isPlainBinder =    arguments.isEmpty  && (! variables.isEmpty) && scopes.length == 1
   def isApplBinder  = (! arguments.isEmpty) && (! variables.isEmpty)
   def isPlainApplBinder  = (! arguments.isEmpty) && (! variables.isEmpty) && scopes.length == 1
   def numSeqArgs = arguments.count(_.isInstanceOf[SeqArg])
   def numSeqVars = variables.count {
      case Var(_,_, Some(_)) => true
      case _ => false
   }
   def numNormalArgs = arguments.count {
      case Arg(n) => n > 0
      case ImplicitArg(_) => true
      case _ => false
   }
   def numNormalVars = variables.count {
      case Var(_,_, Some(_)) => false
      case _ => true
   }
   def numNormalScopes = arguments.count {
      case Arg(n) => n < 0
      case _ => false
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
   def distributeArgs(numArgs: Int) : (Int, Int) = {
      val seqArgPositions = arguments.flatMap {
         case SeqArg(n,_) if n > 0 => List(n)
         case _ => Nil
      }
      distribute(numArgs, arguments.length, seqArgPositions)
   }
   /** 
    * like distributeArgs but for the variables
    */
   def distributeVars(numVars: Int): (Int, Int) = {
      val seqVarPositions = variables.flatMap {
         case Var(n,_,Some(_)) => List(n)
         case _ => Nil
      }
      distribute(numVars, variables.length, seqVarPositions)
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
}

object Arity {
   def constant = Arity(Nil,Nil,Nil)
   def plainApplication = Arity(List(SeqArg(1,Delim(""))), Nil, Nil)
   def plainBinder = Arity(Nil, List(Var(1,false,Some(Delim("")))), List(Arg(-2)))
}

