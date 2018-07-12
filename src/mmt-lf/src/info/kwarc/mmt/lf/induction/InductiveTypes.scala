package info.kwarc.mmt.lf.induction

import info.kwarc.mmt.api._
import objects._
import symbols._
import notations._
import checking._
import modules._
import frontend.Controller

import info.kwarc.mmt.lf._
import info.kwarc.mmt.lf.induction.InternalDeclarationUtil._

/** theories as a set of types of expressions */ 
class InductiveTypes extends StructuralFeature("inductive") with ParametricTheoryLike {

  /**
   * Checks the validity of the inductive type(s) to be constructed
   * @param dd the derived declaration from which the inductive type(s) are to be constructed
   */
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {
    //TODO: check for inhabitability
  }
  
  /**
   * Elaborates an declaration of one or multiple mutual inductive types into their declaration, 
   * as well as the corresponding no confusion and no junk axioms
   * Constructs a structure whose models are exactly the (not necessarily initial) models of the declared inductive types
   * @param parent The parent module of the declared inductive types
   * @param dd the derived declaration to be elaborated
   */
  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) = {
    log("Path of eq: "+Eq.path+"\nPath of neq: "+Neq.path+"\nPath of contradiction: "+Contra.path)
    implicit val parentTerm = parent.toTerm
    // to hold the result
    var elabDecls : List[Declaration] = Nil
    implicit var tmdecls : List[TermLevel]= Nil
    implicit var statdecls : List[StatementLevel]= Nil
    implicit var tpdecls : List[TypeLevel]= Nil
    try {
      val decls = dd.getDeclarations map {
      case c: Constant =>
        val tp = c.tp getOrElse {throw LocalError("missing type")}        
          val FunType(args, ret) = tp
          if (JudgmentTypes.isJudgment(ret)(controller.globalLookup)) {
            val statdecl= StatementLevel(c.path, args)
            statdecls ::= statdecl
            statdecl
          } else {
          ret match {
            case Univ(1) => {
              val tpdecl = TypeLevel(c.path, args)
              tpdecls ::= tpdecl 
              tpdecl
            }
            case Univ(x) if x != 1 => throw LocalError("unsupported universe")
            case r =>
              //TODO: Check r:type
              val tmdecl = TermLevel(c.path, args, r)
              tmdecls ::= tmdecl 
              tmdecl
            }
          }
        case _ => throw LocalError("unsupported declaration")
      }
      // the type and term constructors of the inductive types and the no confusion axioms for the term constructors
      var noConfDecls : List[Declaration] = Nil
      var remainingTmDecls = tmdecls
      decls foreach {d =>
        val tp = d
        val c = Constant(parentTerm, d.name, Nil, Some(tp), None, None)
        elabDecls ::= c
        d match {
          case tl @ TermLevel(loc, args, tm) =>
            noConfDecls ++= noConf(tl,remainingTmDecls)
            //don't generate the no conf declarations twice
            remainingTmDecls=remainingTmDecls.filterNot(_ == tl)
          case _ =>
        }
      }
      // get the no junk axioms and build the result
      elabDecls = elabDecls.reverse ::: noConfDecls ::: noJunk(decls, tpdecls, tmdecls, statdecls)
          elabDecls foreach {d =>
      println(controller.presenter.asString(d).replace("\n", ""))
      }
    }
    catch {
    case e : Error => 
     log("error in elaborate: ")
     log(e.getMessage)
    }
    new Elaboration {
      val elabs : List[Declaration] = Nil 
      def domain = elabDecls map {d => d.name}
      def getO(n: LocalName) = {
        elabDecls.find(_.name == n)
      }
    }
  }

  /**
    * Generate injectivity declaration for term constructor d
    * @param parent the parent declared module of the derived declaration to elaborate
    * @param d the term level for which to generate the injectivity axiom
    */
  private def injDecl(d : TermLevel)(implicit parent : OMID) : Declaration = {
    val (aCtx, aApplied) = d.argContext(Some("1"))
    val (bCtx, bApplied) = d.argContext(Some("2"))
    
    val argEq = (aCtx zip bCtx) map {case (a,b) => Eq(a.toTerm,b.toTerm)}
    val resEq = Eq(aApplied.toTerm, bApplied.toTerm)
    val body = Arrow(Arrow(argEq, Contra), Arrow(resEq, Contra))
    val inj = Pi(aCtx ++ bCtx,  body)
    
    Constant(parent, uniqueLN("injective_"+d.name.toString), Nil, Some(inj), None, None)
  }
  
  /**
  * Generate no confusion/injectivity declaration for term constructor d and all term constructors of the same return type
  * @param parent the parent declared module of the derived declaration to elaborate
  * @param d the term level declaration for which to generate the no confusion declarations
  * @param tmdecls all term level declarations
  */
  private def noConf(a : TermLevel, tmdecls: List[TermLevel])(implicit parent : OMID) : List[Declaration] = {
  var decls:List[Declaration] = Nil
  tmdecls.filter(_.ret == a.ret) foreach {b => 
    if (b == a) {
      if(a.args.length > 0)
        decls :+= injDecl(a)  
    } else {
        val name = uniqueLN("no_conf_" + a.name.toString + "_" + b.name.toString)
        val (aCtx, aApplied) = a.argContext(None)
        val (bCtx, bApplied) = b.argContext(None)
        val tp = Pi(aCtx ++ bCtx, Neq(aApplied.toTerm, bApplied.toTerm))
        decls :+= Constant(parent, name, Nil, Some(tp), None, None)
    }
  }
  decls
  }
  
  /**
  * Generate no junk declaration for all term-- and typelevel inductive declarations
  * @param parent the parent declared module of the derived declaration to elaborate
  * @param decls all declarations
  * @param tmdecls all term level declarations
  * @param tpdecls all type level declarations
  * @returns returns one no junk (morphism) declaration for each type level declaration
  * then generates all the corresponding no junk declarations for the termlevel constructors of each declared type
  */    
  private def noJunk(decls : List[InternalDeclaration], tpdecls: List[TypeLevel], tmdecls: List[TermLevel], statdecls: List[StatementLevel])(implicit parent : OMID) : List[Declaration] = {
    var derived_decls:List[Declaration] = Nil
    val (tp_induct_decls, mapConstr, mapTerm, chain) = getFullMorph(decls, tpdecls, controller)
    derived_decls ++=tp_induct_decls
    
    (tmdecls++statdecls).reverse foreach {decl : InternalDeclaration => 
          val (args, orig) = decl.argContext(None)
            
          //the result of applying m to the result of the constructor
          //redeclaring the type to preserve matches in mapTerm, despite more complicated type structure of orig
          val mappedOrig = mapTerm(VarDecl(orig.name, None, Some(decl.ret), orig.df, None))
          
          //the result of applying the image of the constructor (all types substituted) to the image of the arguments
          val mappedArgs : Term = ApplyGeneral(mapConstr(decl).toVarDecl.toTerm, args map mapTerm)
          
          //both results should be equal, if m is actually a homomorphism
          val assertion = Pi(chain.map (_.toVarDecl) ++ args, Eq(mappedOrig, mappedArgs))
          derived_decls :+= Constant(parent, uniqueLN("no_junk_"+decl.name), Nil, Some(assertion), None, None)
    }
    derived_decls
  } 
}

object InductiveRule extends StructuralFeatureRule(classOf[InductiveTypes], "inductive")