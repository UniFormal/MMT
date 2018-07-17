package info.kwarc.mmt.lf.induction

import info.kwarc.mmt.api._
import objects._
import symbols._
import notations._
import checking._
import modules._
import frontend.Controller

import info.kwarc.mmt.lf._

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
    val context = Type.getParameters(dd) 
//Type.getParameters(dd)//getInnerContext(dd)//Context.empty//Type.getParameters(dd)//this.getInnerContext(dd)//Type.getParameters(dd)//this.getInnerContext(dd)
    val name = LocalName(dd.path.last)
    implicit val parentTerm = OMID(parent.path / name)
    // to hold the result
    var elabDecls : List[Constant] = Nil
    implicit var tmdecls : List[TermLevel]= Nil
    implicit var statdecls : List[StatementLevel]= Nil
    implicit var tpdecls : List[TypeLevel]= Nil
    val decls = dd.getDeclarations map {
      case c: Constant =>
        val intDecl = InternalDeclaration.fromConstant(c, controller, (context++dd.getDeclarations filter {case _ : Constant => true
        case _ => false}) map({case c:Constant=>VarDecl.apply(c.name, None, c.tp, c.tp, None)}))
        intDecl match {
          case d @ TermLevel(_, _, _, _, _) => tmdecls :+= d; intDecl
          case d @ TypeLevel(_, _, _, _) => tpdecls :+= d; intDecl
          case d @ StatementLevel(_, _, _, _) => statdecls :+= d; intDecl 
         }
      case _ => throw LocalError("unsupported declaration")
    }

    // copy all the declarations
    decls foreach {d => elabDecls ::= d.toConstant}
    
    // the no confusion axioms for the data constructors
    elabDecls = elabDecls.reverse ::: TermLevel.noConfs(tmdecls, context)
    
    // the no junk axioms
    elabDecls ++= InternalDeclaration.noJunk(decls, tpdecls, tmdecls, statdecls, context)
    
    elabDecls foreach {d =>
      log(controller.presenter.asString(d).replaceAll("%2F", "/").replaceAll("\n", ""))
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

object InductiveRule extends StructuralFeatureRule(classOf[InductiveTypes], "inductive")
