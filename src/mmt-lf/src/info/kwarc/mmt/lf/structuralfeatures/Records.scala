package info.kwarc.mmt.lf.structuralfeatures

import info.kwarc.mmt.api._
import objects._
import symbols._
import notations._
import checking._
import modules._
import frontend.Controller

import info.kwarc.mmt.lf._

/** theories as a set of types of expressions */ 
class Records extends StructuralFeature("record") with ParametricTheoryLike {

  /**
   * Checks the validity of the record to be constructed
   * @param dd the derived declaration from which the record is to be constructed
   */
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {
    //TODO: check for inhabitability
  }
  
  /**
   * Elaborates the declaration of a record into the derived declarations, 
   * as well as the corresponding no confusion and no junk axioms
   * Constructs a structure whose models are exactly the (not necessarily initial) models of the declared inductive types
   * @param parent The parent module of the declared inductive types
   * @param dd the derived declaration to be elaborated
   */
  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) = {
    val context = Type.getParameters(dd) 
    val name = LocalName(dd.path.last)
    implicit val parentTerm = OMID(parent.path / name)
    // to hold the result
    var elabDecls : List[Constant] = Nil
    implicit var tmdecls : List[TermLevel]= Nil
    implicit var statdecls : List[StatementLevel]= Nil
    implicit var tpdecls : List[TypeLevel] = Nil
    
    val hd = InternalDeclaration.fromConstant(Constant(parentTerm, InternalDeclarationUtil.uniqueLN("M"), Nil, Some(Univ(1)), None, None), controller)
    elabDecls :+= hd.toConstant
    
    var origDecls : List[InternalDeclaration] = Nil
    val decls : List[InternalDeclaration] = dd.getDeclarations map {
      case c: Constant =>
        val intDecl = InternalDeclaration.fromConstant(c, controller)
        var mapTypes : List[Sub] = Nil
        intDecl match {
          case TermLevel(p, args, ret, df, notC) => 
          	origDecls ::= intDecl
          	TermLevel(p, (Some(InternalDeclarationUtil.uniqueLN("m")), hd.ToOMS)::args, ret, df, notC) ^ mapTypes
          case d @ TypeLevel(p, args, df, notC) => 
          	origDecls ::= intDecl
          	val tpl = TypeLevel(p, (None, OMS(hd.toConstant.path))::args, df, notC) ^ mapTypes
          	val s : Sub = OMV(p.name) / tpl.applied(hd.toVarDecl); mapTypes ::=s
          	tpl
          case d @ StatementLevel(p, args, df, notC) => 
          	origDecls ::= intDecl
          	StatementLevel(p, (Some(InternalDeclarationUtil.uniqueLN("m")), hd.ToOMS)::args, df, notC) ^ mapTypes
          	
         }
      case _ => throw LocalError("unsupported declaration")
    }
    decls foreach {
       case d @ TermLevel(_, _, _, _, _) => tmdecls :+= d
       case d @ TypeLevel(_, _, _, _) => tpdecls :+= d
       case d @ StatementLevel(_, _, _, _) => statdecls :+= d
    }
    
    val make : InternalDeclaration = InternalDeclaration.fromConstant(Constant(parentTerm, InternalDeclarationUtil.uniqueLN("M"), Nil, Some(Pi(origDecls.map(_.toVarDecl), hd.ToOMS)), None, None), controller)
	  val applyMakeToArgs = (make.ret, make, {(arg:VarDecl, chain:Context, mapConstr: InternalDeclaration => InternalDeclaration) => OMV(make.name) % make.applied(origDecls.map(x => mapConstr(x).toVarDecl))})
    
    // copy all the declarations
    decls foreach {d => elabDecls ::= d.toConstant}
    
    //process chain
    def processChain: InternalDeclaration => InternalDeclaration = {d => d match {
      case TermLevel(p, args, ret, df, notC) => TermLevel(p, args.tail, ret, df, notC)
       case TypeLevel(p, args, df, notC) => TypeLevel(p, args.tail, df, notC)
       case StatementLevel(p, args, df, notC) => StatementLevel(p, args.tail, df, notC)
    }}
    
    // the no junk axioms
    elabDecls = elabDecls.reverse ++ InternalDeclaration.noJunks(decls, tpdecls, tmdecls, statdecls, context, Some(List(applyMakeToArgs)), processChain)
    
    val arg = InternalDeclarationUtil.newVar(InternalDeclarationUtil.uniqueLN("m"), hd.ret, None)
    val repTm = Pi(arg, InternalDeclarationUtil.Eq(hd.applyTo(decls.map(_.applied(arg))), arg.toTerm))
    val repr = OMV(InternalDeclarationUtil.uniqueLN("repr")) % repTm
    elabDecls :+= Constant.apply(parentTerm, repr.name, Nil, repr.tp, repr.df, None)
    
    elabDecls foreach {d =>
      println(InternalDeclarationUtil.present(d))
    }
    new Elaboration {
      val elabs : List[Declaration] = Nil 
      def domain = elabDecls map {d => d.name}
      def getO(n: LocalName) = {
        elabDecls.find(_.name == n)
      }
    }
  }
}

object RecordRule extends StructuralFeatureRule(classOf[Records], "record")