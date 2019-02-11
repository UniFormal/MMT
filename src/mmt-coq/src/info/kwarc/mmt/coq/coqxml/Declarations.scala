package info.kwarc.mmt.coq.coqxml

import info.kwarc.mmt.api.{GlobalName, LocalName, MPath}
import info.kwarc.mmt.api.objects.{OMS, OMV, Term, VarDecl}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.coq._

import scala.collection.mutable

trait CoqEntry

// ---------------------------------------------------------------------------

trait theorystructure extends CoqEntry

// as \in Axiom | Declaration
case class AXIOM(uri:URI,as:String, components : List[CoqEntry]) extends theorystructure
// as \in Definition | InteractiveDefinition | Inductive | CoInductive | Record
case class DEFINITION(uri : URI,as:String, components : List[CoqEntry]) extends theorystructure
// as \in Theorem | Lemma | Corollary | Fact | Remark
case class THEOREM(uri:URI,as:String, components : List[CoqEntry]) extends theorystructure
// as \in Assumption | Hypothesis | LocalDefinition | LocalFact
case class VARIABLE(uri:URI,as:String, components : List[CoqEntry]) extends theorystructure

case class SECTION(uri:URI,statements:List[theorystructure]) extends theorystructure

// --------------------------------------------------------------------------

// uri: ends with con => constant=DEFINITION|DECLARATION, ends with var => VARIABLE, ends with ind => INDUCTIVE|COINDUCTIVE
//                          ^ may have two xml files (type/statement)

case class InnerTypes(of : URI,_types : List[TYPE]) extends CoqEntry

case class TYPE(of: String,tp : term) extends CoqEntry

// Types and Statements:
case class ConstantType(name : String, params:List[String], id : String, _type : term) extends CoqEntry
//                                      ^ ?-separated list of names (=variables)

// Definiens / Proof:
case class ConstantBody(_for : URI, params: List[String], id : String, body : term) extends CoqEntry
case class Variable(name : String, params: List[String], id : String, body : Option[body], _type : _type) extends CoqEntry
case class InductiveDefinition(noParams : Int,params:List[String],id:String,tps:List[InductiveType]) extends CoqEntry
//                                ^ number of uniform parameters        ^ non-empty

case class InductiveType(name:String,inductive:Boolean,id:String,arity:arity,constructors:List[Constructor]) extends CoqEntry
//                                       ^ or coinductive
case class body(tm : term) extends CoqEntry
case class arity(tm : term) extends CoqEntry
case class Constructor(name:String,_type:term) extends CoqEntry

// ------------------------------------------------------------------------

class TranslationState(theories : Map[URI,MPath], symbols : Map[URI,GlobalName]) {
  private var _vars : List[Option[String]] = Nil
  def addVar = _vars ::= None
  def solveVar(i : Int, name : String): LocalName = {
    val j = i-1
    if (i > _vars.length) {
      ???
    }
    _vars(j) match {
      case Some(`name`) => LocalName(name)
      case Some(o) =>
        LocalName(o) // TODO check this!
      case None =>
        val pre = _vars.take(j)
        val post = _vars.drop(j+1)
        _vars = pre ::: Some(name) :: post
        LocalName(name)
    }
  }
  def getVar = {
    val ret :: tail = _vars
    _vars = tail
    ret
  }

  private var implicits : List[Option[Term]] = Nil

  def newImplicit(tp : Option[Term] = None) = {
    val ret = LocalName("") / LocalName("i") / implicits.length.toString
    implicits::=tp
    OMV(ret)
  }

  private var varnames : Int = 0
  def pickFresh : LocalName = {
    varnames+=1
    LocalName("MMT_internal_" + varnames.toString)
  }

  def toMPath(uri : URI) = theories.getOrElse(uri,???) // TODO
  def toGlobalName(uri : URI) = symbols.getOrElse(uri,Coq.fail) // TODO

}

trait term extends CoqEntry {
  def toOMDoc(theories : Map[URI,MPath], symbols : Map[URI,GlobalName]) : Term = {
    // TODO implicit arguments
    recOMDoc(new TranslationState(theories,symbols))
  }
  private[coqxml] def recOMDoc(implicit variables : TranslationState) : Term
}
trait objectOccurence extends term

case class SORT(value : String, id : String) extends term {
  def recOMDoc(implicit variables : TranslationState) : Term = Coq.makeSort(value)
}

case class LAMBDA(sort : String, decls:List[decl] ,target:target) extends term {
  def recOMDoc(implicit variables : TranslationState) : Term = {
    val tps = decls.map{d =>
      val ret = (d.id,d._vartype.recOMDoc)
      variables.addVar
      ret
    }
    val ret = target.tm.recOMDoc
    val vars = tps.reverse.map(d => (LocalName(variables.getVar.getOrElse(OMV.anonymous.toString)),d._2))
    // println("VARIABLES: " + vars.map(_._1.toString))
    CoqLambda(sort,vars,ret)
  }
}

case class LETIN(sort : String, defs:List[_def] ,target:target) extends term {
  def recOMDoc(implicit variables : TranslationState) : Term = {
    val dfs = defs.map{d =>
      val ret = (d.id,d._vardef.recOMDoc)
      variables.addVar
      ret
    }
    val body = target.tm.recOMDoc
    val vars = dfs.reverse.map(d => (variables.getVar.getOrElse(OMV.anonymous.toString),d._2))
    vars.foldLeft(body)((b,p) => Let(LocalName(p._1),p._2,b))
  }

}
case class PROD(_type : String, decls:List[decl] ,target:target) extends term {
  def recOMDoc(implicit variables : TranslationState) : Term = {
    val tps = decls.map{d =>
      val ret = (d.id,d._vartype.recOMDoc)
      variables.addVar
      ret
    }
    val body = target.tm.recOMDoc
    val vars = tps.reverse.map(d => (variables.getVar.getOrElse(OMV.anonymous.toString),d._2))
    // println("VARIABLES: " + vars.map(_._1.toString))
    CoqPROD(_type,vars,body)
  }
}
case class CAST(id : String, sort : String, tm : term, _type : _type) extends term {
  def recOMDoc(implicit variables : TranslationState) : Term = tm.recOMDoc // TODO
}
case class APPLY(id : String, sort : String, tms : List[term]) extends term {
  def recOMDoc(implicit variables : TranslationState) : Term = {
    val f :: args = tms.map(_.recOMDoc)
    CoqApply(sort,f,args)
  }
}
case class VAR(uri : URI, id : String, sort : String) extends term with objectOccurence {
  def recOMDoc(implicit variables : TranslationState) : Term = OMS(variables.toGlobalName(uri))
}// OMS (because sections)
case class CONST(uri : URI, id : String, sort : String) extends term with objectOccurence {
  def recOMDoc(implicit variables : TranslationState) : Term = OMS(variables.toGlobalName(uri))
}// OMS
case class MUTIND(uri : URI, noType : Int, id : String) extends term with objectOccurence {
  def recOMDoc(implicit variables : TranslationState) : Term = {
    val gn = variables.toGlobalName(uri)
    OMS(gn.module ? (gn.name.toString + noType.toString))
  }
} // OMS
//                                ^  starts from 0, index in list of mututally recursive types
case class MUTCONSTRUCT(uri : URI, noType : Int, noConstr : Int, id : String, sort : String) extends term with objectOccurence {
  def recOMDoc(implicit variables : TranslationState) : Term = OMS(Coq.fail) // TODO
}// OMS
//                                     ^  from 0      ^ starts from 1, index in list of constructors
case class FIX(noFun : Int, id : String, sort : String, fixFunctions : List[FixFunction]) extends term {
  def recOMDoc(implicit variables : TranslationState) : Term = ???
}
//              ^ from 0 n                                  ^ no-empty
case class FixFunction(name : String, id : String, recIndex: Int,_type : _type, body : body) extends CoqEntry {
  def recOMDoc(implicit variables : TranslationState) : Term = ???
}
//                                                      ^ index of decreasing argument, from 0 (proof for termination)
case class COFIX(noFun : Int, id : String, sort : String, cofixFunctions : List[CofixFunction]) extends term {
  def recOMDoc(implicit variables : TranslationState) : Term = ???
}
//              ^ from 0 n                                  ^ no-empty
case class CofixFunction(id : String, name : String, _type : _type, body : body) extends CoqEntry {
  def recOMDoc(implicit variables : TranslationState) : Term = ???
}
case class MUTCASE(uriType: URI, noType : Int, id : String, sort : String, patternsType : patternsType,
//                     ^  inductive type to match on                                ^ lambda abstracted return type (over both indices and mathcing value)
                   inductiveTerm : inductiveTerm, patterns : List[pattern]) extends term {
  def recOMDoc(implicit variables : TranslationState) : Term = OMS(Coq.fail) // TODO
}
//                    ^  the thing I'm matching      ^ lambda-abstracted cases
case class instantiate(id: String, oo:objectOccurence,args:List[arg]) extends term {
  // println("Args: " + args)
  def recOMDoc(implicit variables : TranslationState) : Term = Sub(args(1).arg.recOMDoc,oo.recOMDoc,args.head.arg.recOMDoc)
    // TODO check that this is correct
}
//                                                             ^ non-empty
case class REL(value : Int, binder : String, id : String, idref:String,sort : String) extends term {
  def recOMDoc(implicit variables : TranslationState) : Term = {
    OMV(variables.solveVar(value,binder))
  }
}// OMV
//               ^ deBruijn-index(from 1) ^ (ideally) the name ^ id of binder

case class arg(relUri : URI, arg : term) extends CoqEntry // explicit substitution
case class patternsType(tm : term) extends CoqEntry
case class inductiveTerm(tm : term) extends CoqEntry
case class pattern(tm : term) extends CoqEntry
case class _type(tm : term) extends CoqEntry
case class target(tm : term) extends CoqEntry
case class decl(id: String, _type : String /* sort , binder : String */, _vartype : term) extends CoqEntry
case class _def(id: String, sort : String , binder : String, _vardef : term) extends CoqEntry