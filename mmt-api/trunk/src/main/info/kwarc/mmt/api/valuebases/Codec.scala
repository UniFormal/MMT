package info.kwarc.mmt.api.valuebases

import info.kwarc.mmt.api._
import objects._

case object CodecNotApplicable extends java.lang.Throwable

/**
 * a term that should be coded
 * @param tm the term
 * @param tp its type
 */
case class Codable(tm: Term, tp: Term)

/**
 * a term representation that can be decoded
 * @param rep the code
 * @param tp its expected type
 */
case class Decodable[Rep](rep: Rep, tp: Term)

/**
 * encodes/decodes terms of a certain type
 * @param tp the type
 */
abstract class Codec[Rep](val tp: Term) {
   def encode(c: Term): Rep
   def decode(r: Rep): Term
}

/**
 * encodes/decodes terms whose type is of the form T = OMA(OMS(tp), pars)
 * @param tp the type operator
 */
abstract class CodecOperator[Rep](val tp: GlobalName) {
   
   val numberOfTypeParameters: Int
   
   /**
    * @param c one codec for each type parameter, i.e., cs.length == this.numberOfTypeParameters
    * @return a codec for OMA(OMS(tp), cs.map(_.tp)) 
    */
   def apply(cs: Codec[Rep]*): Codec[Rep]
   
   // obsolete after here
   /**
    * constructs terms of this type after decoding their components
    * @param pars the parameters of T
    * @param args the decoded components (obtained by decoding the result of decode)
    * @return the decoded term 
    */
   def construct(pars: List[Term], args: List[Term]): Term
   
   /**
    * tries to destruct terms of this type for coding
    * @param pars the parameters of T
    * @param c the term of type T
    * @return if successful, the list of components that make up the term 
    */
   def destruct(pars: List[Term], c: Codable): Option[List[Codable]]

   /**
    * encodes terms of type T
    * @param pars the parameters of T
    * @param args the encoded components (obtained by encoding the result of destruct)
    * @return the encoded term
    */
   def encode(pars: List[Term], args: List[Rep]): Rep
   /**
    * decodes terms of type T
    * @param pars the parameters of T
    * @param d the representation to decode
    * @return the components that must be decoded further 
    */
   def decode(pars: List[Term], d: Decodable[Rep]): List[Decodable[Rep]]
}

/**
 * encodes/decodes terms using a set of [[Codec]]s and [[CodecOperator]]s 
 */
class Coder[Rep](codecs: List[Codec[Rep]], operators: List[CodecOperator[Rep]]) {
   
   def buildCodec(tp: Term): Codec[Rep] = tp match {
      case OMA(OMS(op), pars) =>
         val cop = operators.find(_.tp == op).getOrElse {
            throw GeneralError("no codec found")
         }
         if (pars.length != cop.numberOfTypeParameters)
            throw CodecNotApplicable
         val parsC = pars map buildCodec
         val tpC = cop(parsC:_*)
         if (tpC.tp != tp)
            throw CodecNotApplicable
         tpC
   }
   
   // obsolete after here
   def encode(c: Codable): Rep = c.tp match {
      case OMA(OMS(op), pars) =>
         val cop = operators.find(_.tp == op).getOrElse {
            throw GeneralError("no codec found")
         }
         val args = cop.destruct(pars, c).getOrElse {
            throw GeneralError("codec not applicable")
         }
         val argsE = args map encode
         cop.encode(pars, argsE)
      case OMS(p) =>
         val cod = codecs.find(_.tp == OMS(p)).getOrElse {
            throw GeneralError("no codec found")
         }
         cod.encode(c.tm)
      case _ =>
         throw GeneralError("no codec found")
   }
   
   def decode(d: Decodable[Rep]): Term = d.tp match {
      case OMA(OMS(op), pars) =>
         val cop = operators.find(_.tp == op).getOrElse {
            throw GeneralError("no codec found")
         }
         val args = cop.decode(pars, d)
         val argsD = args map decode
         cop.construct(pars, argsD)
      case OMS(p) =>
         val cod = codecs.find(_.tp == OMS(p)).getOrElse {
            throw GeneralError("no codec found")
         }
         cod.decode(d.rep)
      case _ =>
         throw GeneralError("no codec found")
   }
}

/** lifts an embedding between representation types to a functor on codecs */
abstract class Embedding[From,To] {
   def embed(f: From): To
   def extract(t: To): Option[From]
   
   def apply(c: Codec[From]): Codec[To] = new Codec[To](c.tp) {
      def encode(t: Term) = embed(c.encode(t))
      def decode(rT: To) = extract(rT) match {
         case Some(rF) => c.decode(rF)
         case None => throw CodecNotApplicable
      }
   }
}

/**
 * codes a list
 */
abstract class ListCodec[Rep](tp: GlobalName, nil: GlobalName, cons: GlobalName) extends CodecOperator[Rep](tp) {self =>
   
   val numberOfTypeParameters = 1
   
   def encode(reps: List[Rep]): Rep
   def decode(rep: Rep): Option[List[Rep]]
   
   def destruct(tm: Term): List[Term] = tm match {
      case OMS(nil) => Nil
      case OMA(OMS(cons), List(hd, tl)) => hd :: destruct(tl)
   }
   def construct(tms: List[Term]): Term = tms.foldRight[Term](OMS(nil)){case (sofar, next) => cons(next, sofar)}
   
   def apply(cs: Codec[Rep]*) = {
      val c = cs.head
      new Codec[Rep](tp(c.tp)) {
         def encode(t: Term) = self.encode(self.destruct(t) map c.encode)
         def decode(r: Rep) = self.construct(self.decode(r).get map c.decode)
      }
   }

   // obsolete after here
   
   def getParam(pars: List[Term]) = pars match {
      case List(p) => p
      case _ => throw CodecNotApplicable
   }
   def getParam(d: Decodable[Rep]) = d.tp match {
      case OMA(OMS(tp), List(p)) => p
      case _ => throw CodecNotApplicable
   }
   
   def construct(pars: List[Term], args: List[Term]): Term = {
      args.foldRight[Term](OMS(nil)){case (sofar, next) => cons(next, sofar)}
   }
   def destruct(pars: List[Term], c: Codable): Option[List[Codable]] = {
      val p = getParam(pars)
      c.tm match {
         case OMS(nil) =>
            Some(Nil)
         case OMA(OMS(cons), List(hd, tl)) =>
            val tlCOpt = destruct(pars, Codable(tl, c.tp))
            tlCOpt map {tlC =>
               Codable(hd, p) :: tlC
            }
      }
   }
   def encode(pars: List[Term], args: List[Rep]): Rep = encode(args)
   def decode(pars: List[Term], d: Decodable[Rep]): List[Decodable[Rep]] = {
      val p = getParam(d)
      decode(d.rep).getOrElse(throw CodecNotApplicable).map {r => Decodable(r, p)}
   }
}

/**
 * codes a record, not correct yet
 */
abstract class RecordCodec[Rep](tp: GlobalName, tm: GlobalName, keys: List[GlobalName]) extends CodecOperator[Rep](tp) {self =>
   
   val numberOfTypeParameters = keys.length
   
   def encode(reps: List[(GlobalName,Rep)]): Rep
   def decode(rep: Rep): Option[List[(GlobalName,Rep)]]
   
   def destruct(tm: Term): List[(GlobalName,Term)] = tm match {
      case OMA(OMS(tm), fields) =>
         fields.map {
            case OMA(OMS(k), List(v)) => (k,v)
         }
   }
   def construct(fields: List[(GlobalName,Term)]): Term = {
      val fieldTerms = fields map {case (k,v) => k(v)}
      tm(fieldTerms)
   }
   
   def apply(cs: Codec[Rep]*) = {
      val fields = (keys zip cs) map {case (k,c) => k(c.tp)}
      new Codec[Rep](tp(fields)) {
         def encode(t: Term) = self.encode((self.destruct(t) zip cs) map {case ((k,v),c) => (k, c.encode(v))})
         def decode(r: Rep) = self.construct((self.decode(r).get zip cs) map {case ((k,v),c) => (k, c.decode(v))})
      }
   }
}