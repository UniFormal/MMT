package info.kwarc.mmt.lf.structuralfeatures

import info.kwarc.mmt.api._
import objects._
import symbols._
import notations._
import checking._
import modules._
import frontend.Controller

import info.kwarc.mmt.lf._

import InternalDeclaration._
import InternalDeclarationUtil._

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
    
    val structure : TypeLevel = structureDeclaration
    elabDecls :+= structure.toConstant
    
    val origDecls : List[InternalDeclaration] = dd.getDeclarations map {
      case c: Constant => fromConstant(c, controller)
      case _ => throw LocalError("unsupported declaration")
    }
    val decls : List[InternalDeclaration] = toEliminationDecls(origDecls, structure)
    
    decls foreach {
       case d @ TermLevel(_, _, _, _, _) => tmdecls :+= d
       case d @ TypeLevel(_, _, _, _) => tpdecls :+= d
       case d @ StatementLevel(_, _, _, _) => statdecls :+= d
    }
    
    val make : TermLevel = introductionDeclaration(structure, origDecls)
    // copy all the declarations
    decls foreach {d => elabDecls ::= d.toConstant}
    
    // the no junk axioms
    elabDecls = elabDecls.reverse ++ noJunksEliminationDeclarations(decls, tpdecls, tmdecls, statdecls, context, make, origDecls)
    
    elabDecls :+= reprDeclaration(structure, decls).toConstant
    
    elabDecls foreach {d =>
      log(present(d))
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