package info.kwarc.mmt.coq.coqxml

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.coq._
import info.kwarc.mmt.lf.{ApplySpine, Lambda}

trait CoqEntry

trait CoqXml extends CoqEntry
trait theoryexpr extends CoqEntry

case class TopXML(e : CoqEntry) extends CoqXml
case class TypesXML(e : CoqEntry) extends CoqXml
case class BodyXML(e : CoqEntry) extends CoqXml
case class ExprXML(e : theoryexpr) extends CoqXml
case class SupXML(e : supertypes) extends CoqXml
case class Constraints(e : List[CoqEntry]) extends CoqXml
case class supertypes(ls : List[CoqEntry]) extends CoqXml


// ---------------------------------------------------------------------------

trait theorystructure extends CoqEntry

case class Requirement(uri : URI) extends theorystructure

object constantlike {
  def unapply(ts:theorystructure) : Option[(URI,String,List[CoqEntry])] = ts match {
    case AXIOM(uri,as,components) => Some((uri,as,components))
    case DEFINITION(uri,as,components) => Some((uri,as,components))
    case THEOREM(uri,as,components) => Some((uri,as,components))
    case CHILD(uri,components) => Some((uri,"",components))
    case _ => None
  }
}

// as \in Axiom | Declaration
case class AXIOM(uri:URI,as:String, components : List[CoqEntry]) extends theorystructure {
}
// as \in Definition | InteractiveDefinition | Inductive | CoInductive | Record
case class DEFINITION(uri : URI,as:String, components : List[CoqEntry]) extends theorystructure
// as \in Theorem | Lemma | Corollary | Fact | Remark
case class THEOREM(uri:URI,as:String, components : List[CoqEntry]) extends theorystructure
// as \in Assumption | Hypothesis | LocalDefinition | LocalFact
// We append _ to disambiguate the class files for VARIABLE and Variable on case-insensitive file systems
case class VARIABLE_(uri:URI,as:String, components : List[CoqEntry]) extends theorystructure

case class SECTION(uri:URI,statements:List[theorystructure]) extends theorystructure

case class MODULE(uri : URI, params : List[(String,List[CoqXml])], as:String,components:List[theorystructure],componentsImpl : List[theorystructure],attributes : List[CoqXml],attributesImpl:List[CoqXml]) extends theorystructure {
  as match {
    case "Module" =>
    case "ModuleType" =>
    case _ =>
      println(as)
      ???
  }
}
case class MODULEExpr(uri : URI, params : List[(String,List[CoqXml])], as:String,components:List[CoqXml],children : List[CHILD]) extends theorystructure {
  as match {
    case "AlgebraicModule" =>
    case "AlgebraicModuleType" =>
  }
}
case class CHILD(uri:URI,components : List[CoqEntry]) extends theorystructure

case class MREF(uri : URI) extends theoryexpr
case class FUNAPP(f : theoryexpr, args : List[theoryexpr]) extends theoryexpr
case class WITH(a : theoryexpr, d : theoryexpr) extends theoryexpr
case class THDEF(relUri : URI,df : term) extends theoryexpr
case class ABS(uri : URI,mod:theoryexpr)extends theoryexpr

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

case class TranslationState(controller:Controller, toMPath : URI => MPath, toGlobalName : URI => GlobalName, currentT : AbstractTheory) {
  val current = currentT.modulePath
  /*
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
  } */
}

trait term extends CoqEntry {
  def toOMDoc(s:TranslationState) : Term = {
    // TODO implicit arguments
    recOMDoc(s)
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
      // val ret = (d.id,d._vartype.recOMDoc)
      // variables.addVar
      // ret
      (d.binder,d._vartype.recOMDoc)
    }
    val ret = target.tm.recOMDoc
    val vars = tps.reverse.map(d => (LocalName(d._1),d._2))
    // println("VARIABLES: " + vars.map(_._1.toString))
    CoqLambda(sort,vars,ret)
  }
}

case class LETIN(sort : String, defs:List[_def] ,target:target) extends term {
  def recOMDoc(implicit variables : TranslationState) : Term = {
    val dfs = defs.map{d =>
      // val ret = (d.id,d._vardef.recOMDoc)
      // variables.addVar
      // ret
      (d.binder,d._vardef.recOMDoc)
    }
    val body = target.tm.recOMDoc
    val vars = dfs.reverse.map(d => (d._1,d._2))
    vars.foldLeft(body)((b,p) => Let(LocalName(p._1),p._2,b))
  }

}
case class PROD(_type : String, decls:List[decl] ,target:target) extends term {
  def recOMDoc(implicit variables : TranslationState) : Term = {
    val tps = decls.map{d =>
      // val ret = (d.id,d._vartype.recOMDoc)
      //variables.addVar
      // ret
      (d.binder,d._vartype.recOMDoc)
    }
    val body = target.tm.recOMDoc
    val vars = tps.reverse.map(d => (d._1,d._2))
    // println("VARIABLES: " + vars.map(_._1.toString))
    CoqPROD(_type,vars,body)
  }
}
case class CAST(id : String, sort : String, tm : term, _type : _type) extends term {
  def recOMDoc(implicit variables : TranslationState) : Term = {
    tm.recOMDoc // type casts are only for the coq type checker
  }
}
case class APPLY(id : String, sort : String, tms : List[term]) extends term {
  def recOMDoc(implicit variables : TranslationState) : Term = {
    val f :: args = tms.map(_.recOMDoc)
    CoqApply(sort,f,args)
  }
}
case class VAR(uri : URI, id : String, sort : String) extends term with objectOccurence {
  def recOMDoc(implicit variables : TranslationState) : Term = {
    OMS(variables.current ? uri.path.last.replace(".var",""))
  }
}// OMS (because sections)
case class CONST(uri : URI, id : String, sort : String) extends term with objectOccurence {
  def recOMDoc(implicit variables : TranslationState) : Term = OMS(variables.toGlobalName(uri))
}// OMS
case class MUTIND(uri : URI, noType : Int, id : String) extends term with objectOccurence {
  def recOMDoc(implicit variables : TranslationState) : Term = {
    val gn = variables.toGlobalName(uri)
    OMS(gn.module ? (gn.name.toString /* + "_" + noType.toString */)) // TODO
  }
} // OMS
//                                ^  starts from 0, index in list of mututally recursive types
case class MUTCONSTRUCT(uri : URI, noType : Int, noConstr : Int, id : String, sort : String) extends term with objectOccurence {
  def recOMDoc(implicit variables : TranslationState) : Term = {
    val gn = variables.toGlobalName(uri)
    OMS(gn.module ? (gn.name.toString + "_C_" + noConstr ))
  }
}// OMS
//                                     ^  from 0      ^ starts from 1, index in list of constructors
import objects.Conversions._
case class FIX(noFun : Int, id : String, sort : String, fixFunctions : List[FixFunction]) extends term {
  def recOMDoc(implicit variables : TranslationState) : Term = {
    val vars = fixFunctions.map(f => (LocalName(f.name),f._type.tm.recOMDoc,f.body.tm.recOMDoc))
    ApplySpine(OMS(Coq.fix),Lambda(vars.map(v => v._1%v._2),ApplySpine(OMS(Coq.fix),vars.map(_._3):_*))) // TODO ???
  }
}
//              ^ from 0 n                                  ^ no-empty
case class FixFunction(name : String, id : String, recIndex: Int,_type : _type, body : body) extends CoqEntry {
  def recOMDoc(implicit variables : TranslationState) : Term = {
   ??? // TODO ???
  }
}
//                                                      ^ index of decreasing argument, from 0 (proof for termination)
case class COFIX(noFun : Int, id : String, sort : String, cofixFunctions : List[CofixFunction]) extends term {
  def recOMDoc(implicit variables : TranslationState) : Term = {
    val vars = cofixFunctions.map(f => (LocalName(f.name),f._type.tm.recOMDoc,f.body.tm.recOMDoc))
    ApplySpine(OMS(Coq.cofix),Lambda(vars.map(v => v._1%v._2),ApplySpine(OMS(Coq.cofix),vars.map(_._3):_*))) // TODO ???
  }
}
//              ^ from 0 n                                  ^ no-empty
case class CofixFunction(id : String, name : String, _type : _type, body : body) extends CoqEntry {
  def recOMDoc(implicit variables : TranslationState) : Term = {
    ??? // TODO ???
  }
}
case class MUTCASE(uriType: URI, noType : Int, id : String, sort : String, patternsType : patternsType,
//                     ^  inductive type to match on                                ^ lambda abstracted return type (over both indices and mathcing value)
                   inductiveTerm : inductiveTerm, patterns : List[pattern]) extends term {
  def recOMDoc(implicit variables : TranslationState) : Term = {
    ApplySpine(OMS(Coq.ccase),patternsType.tm.recOMDoc :: inductiveTerm.tm.recOMDoc :: patterns.map(_.tm.recOMDoc):_*)
  } // TODO
}
//                    ^  the thing I'm matching      ^ lambda-abstracted cases
case class instantiate(id: String, oo:objectOccurence,args:List[arg]) extends term {
  // println("Args: " + args)
  def recOMDoc(implicit variables : TranslationState) : Term = {

    ApplySpine(oo.recOMDoc,args.map(_.arg.recOMDoc):_*)
  }
    // TODO check that this is correct
}
//                                                             ^ non-empty
case class REL(value : Int, binder : String, id : String, idref:String,sort : String) extends term {
  def recOMDoc(implicit variables : TranslationState) : Term = {
    OMV(binder)
  }
}// OMV
//               ^ deBruijn-index(from 1) ^ (ideally) the name ^ id of binder
case class PROJ(uri: URI, noType : Int, id : String, sort : String, tm : term) extends term {
  def recOMDoc(implicit variables : TranslationState) : Term = {
    ApplySpine(OMS(Coq.proj),tm.recOMDoc)
  } // TODO
}

case class arg(relUri : URI, arg : term) extends CoqEntry // explicit substitution
case class patternsType(tm : term) extends CoqEntry
case class inductiveTerm(tm : term) extends CoqEntry
case class pattern(tm : term) extends CoqEntry
case class _type(tm : term) extends CoqEntry
case class target(tm : term) extends CoqEntry
case class decl(id: String, _type : String /* sort , binder : String */, binder : String, _vartype : term) extends CoqEntry
case class _def(id: String, sort : String , binder : String, _vardef : term) extends CoqEntry