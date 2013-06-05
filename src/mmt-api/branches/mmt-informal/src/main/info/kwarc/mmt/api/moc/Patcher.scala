package info.kwarc.mmt.api.moc

import info.kwarc.mmt.api._

import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.patterns._

object Patcher {
  /**
   * Applies a diff to a memory. Assumes diff is applicable!
   * @param diff the diff
   * @param mem  the memory
   */
  def patch(diff : Diff,  controller : Controller) = diff.changes foreach {ch => patchChange(ch, controller)}


  /**
   * Applies a change to a memory. Assumes change is applicable!
   * @param ch the change to be applied
   * @param mem the memory representing the theory graph
   */
  private def patchChange(ch : Change, controller : Controller) {
    ch match {
      case a : Add => controller.add(a.s)
      case d : Delete => controller.delete(d.s.path)
      /*
      case AddModule(m) => mem.content.add(m)
      case DeleteModule(m) => mem.content.delete(m.path)
      case AddDeclaration(d) => mem.content.add(d)
      case DeleteDeclaration(d) => mem.content.delete(d.path)
      */
      case UpdateComponent(path, comp, old, nw) =>
        val d = controller.memory.content.get(path)
        val dnew = updateComponent(d, comp, old, nw)
        controller.memory.content.update(dnew)
      case UpdateMetadata(path,key,old,nw) => 
        val md = controller.memory.content.get(path).metadata
        md.update(key, nw:_*)
      case PragmaticChange(name, diff, tp, mp, desc) => patch(diff, controller)
      
    }

  }
  
  private def copyDecls(old : DeclaredTheory, nw : DeclaredTheory) : DeclaredTheory = {
    old.components collect {
      case s : Symbol => nw.add(s)
    }
    nw
  }
  
  private def copyDecls(old : DeclaredView, nw : DeclaredView) : DeclaredView = {
    old.components collect {
      case s : Assignment => nw.add(s)
    }
    nw
  }
  
  def updateComponent(d : ContentElement, comp : DeclarationComponent,  old : Option[Obj], nw : Option[Obj]) : ContentElement = (d, comp, nw) match {

    /** Theories */
    case (t : DeclaredTheory, DomComponent, Some(OMMOD(p))) => copyDecls(t, new DeclaredTheory(t.parent, t.name, Some(p)))

    case (t : DeclaredTheory, DomComponent, None) => copyDecls(t, new DeclaredTheory(t.parent, t.name, None))
      
    case (t : DefinedTheory,  DefComponent, Some(df : Term)) => new DefinedTheory(t.parent, t.name, df)

    /** Views */
    case (v : DeclaredView, CodComponent, Some(to : Term)) => copyDecls(v, new DeclaredView(v.parent, v.name, v.from, to, v.isImplicit))
    case (v : DeclaredView, DomComponent, Some(from : Term)) => copyDecls(v, new DeclaredView(v.parent, v.name, from, v.to, v.isImplicit))
    case (v : DefinedView, CodComponent, Some(to : Term)) => new DefinedView(v.parent, v.name, v.from, to, v.df, v.isImplicit)
    case (v : DefinedView, DomComponent, Some(from : Term)) => new DefinedView(v.parent, v.name, from, v.to, v.df, v.isImplicit)
    case (v : DefinedView,  DefComponent, Some(df : Term)) => new DefinedView(v.parent, v.name, v.from, v.to, df, v.isImplicit)

    /** Constants */
    case (c : Constant, DefComponent, Some(s : Term)) => Constant(c.home, c.name, c.alias, c.tp, Some(s), c.rl, c.not)
    case (c : Constant, DefComponent, None) => Constant(c.home, c.name, c.alias, c.tp, None, c.rl, c.not)
    case (c : Constant, TypeComponent, Some(s : Term)) => Constant(c.home, c.name, c.alias, Some(s), c.df, c.rl, c.not)
    case (c : Constant, TypeComponent, None) => Constant(c.home, c.name, c.alias, None, c.df, c.rl, c.not)

    /** Patterns */
    case(p : Pattern,  ParamsComponent, Some(params : Context)) => new Pattern(p.home, p.name, params, p.body, p.not)
    case(p : Pattern,  PatternBodyComponent, Some(body : Context)) => new Pattern(p.home, p.name, p.params, body, p.not)

    /** Instances */
    case(i : Instance, TypeComponent, Some(OMA(OMID(p), matches))) if i.pattern == p => new Instance(i.home, i.name, i.pattern, matches)

    /** ConstantAssignments */
    case(c : ConstantAssignment, DefComponent, Some(target : Term)) => new ConstantAssignment(c.home, c.name, c.alias, Some(target))

    /** DefLinkAssignments */
    case(d : DefLinkAssignment, DefComponent, Some(target : Term)) => new DefLinkAssignment(d.home, d.name, d.from, target)

    case _ => throw UpdateError("Unexpected component update found while applying Diff.\n" +
                                "ContentElement = " + d.toString + "\n" +
                                "compName = " + comp +  "\n" +
                                "newComp = " + nw.toString)
 
  }
  

}


/*
object Patcher {
   def patch(diff : Diff, controller : Controller) : Document = {
	  val baseDoc = controller.getDocument(diff.old)
	  diff.changes.map(x => x match {
	    case c : ChangeModule => applyModuleChange(c, diff.cold, controller)
	    case c : ChangeDeclaration => applyDeclarationChange(c, diff.cold, controller)
	  })
	  
	  controller.getDocument(diff.nw)
	}
   
   def applyModuleChange(cm : ChangeModule, cold : Controller, controller : Controller) = {
	  cm match {
	    case AddModule(m,tp) => (tp,m) match {
	        case ("theory",d : DeclaredTheory) => 
	        	val nd = new DeclaredTheory(d.parent, d.name, d.meta)
	        	controller.add(nd)
	        	controller.add(MRef(m.path.parent ,m.path, true))
	        	_declarations(d).map(x => controller.add(x))
	      }
	      
	    case UpdateModule(path,tp, bodyChanges, componentChanges) => 
	      val m = applyComponentChanges(cold.get(path), tp, componentChanges)
	      controller.add(m)
	      m.path match {
	        case pth : MPath => controller.add(MRef(m.path.doc ,pth, true))
	        case _ => 
	      }
	      bodyChanges.map(applyDeclarationChange(_, cold, controller))
	    case DeleteModule(path, tp) => None
	    case RenameModule(path, tp, name) =>  
	      (tp,cold.get(path)) match {
	        case ("theory",d : DeclaredTheory) => 
	        	val nd = new DeclaredTheory(d.parent, name, d.meta)
	        	controller.add(nd)
	        	controller.add(MRef(nd.path.parent ,nd.path, true))
	        	_declarations(d).map(x => x match {
	        	  case c : Constant => 
	        	    val nc = new Constant(OMMOD(c.home.toMPath.doc ? name), c.name, c.tp, c.df, c.rl)
	        	    controller.add(nc)
	        	  case i : Include => 
	        	    val in = new Include(OMMOD(i.home.toMPath.doc ? name), i.from)
	        	    controller.add(in)
	        	})
	      }
	      
	    case IdenticalModule(path, tp) => 
	      controller.add(cold.get(path))
	  }
	}
	
	def applyDeclarationChange(cd : ChangeDeclaration, cold : Controller, controller : Controller) = {
	  cd match {
	    case AddDeclaration(d, tp) => controller.add(d) 
	    case UpdateDeclaration(path, tp, changes) => {
	      val dcl = applyComponentChanges(cold.get(path),tp, changes)
	      controller.add(dcl)
	    }
	    case DeleteDeclaration(path, tp) => None
	    case RenameDeclaration(path, tp, name) => (tp, cold.get(path)) match {
	      case ("constant", c : Constant) => 
	        val nd = new Constant(c.home, name, c.tp, c.df, c.rl)
	        controller.add(nd)
	    }
	    case IdenticalDeclaration(path, tp) => controller.add(cold.get(path))
	  }
	}
	
	def applyComponentChanges(s : StructuralElement, tp : String, componentChanges : List[ChangeComponent]) : StructuralElement = {
	  (tp,s) match {
	    case ("constant", c : Constant) => 
	      val home = c.home
	      val name = c.name
	      var tp = c.tp
	      var df = c.df
	      var rl = c.rl
	      componentChanges.map(x => x match {
	        case AddComponent(name, comp) => (name, comp) match {
	          case ("type", Component(t : Term)) => tp = Some(t)
	          case ("definition", Component(t : Term)) => df = Some(t)
	          case _ => throw ImplementationError("match error in Constant components")
	        }   
	        case DeleteComponent(ctp,name) => (name) match {
	          case "type" => tp = None
	          case "definition" => df = None
	          case _ => throw ImplementationError("match error in Constant components")
	        }
	        case UpdateComponent(name,comp,changes) => (name,comp) match {
	          case ("type", Component(t : Term)) => tp = Some(t)
	          case ("definition", Component(t : Term)) => df = Some(t)
	          case _ => throw ImplementationError("match error in Constant components")

	        }
	        case IdenticalComponent(ctp,name) => None //already set above
	      })
	      val cn = new Constant(home, name, tp, df, rl)
	      cn
	    case ("theory", d : DeclaredTheory) => 
	      val doc = d.parent
	      val name = d.name
	      var meta = d.meta 
	      componentChanges.map(x => x match {
	        case AddComponent(name, comp) => (name, comp) match {
	          case ("meta", Component(p : OMMOD)) => meta = Some(p.toMPath)
	          case _ => throw ImplementationError("match error in Declared Theory components")

	        }   
	        case DeleteComponent(ctp,name) => (name) match {
	          case "meta" => meta = None
	          case _ => throw ImplementationError("match error in Declared Theory components")
	        }
	        case UpdateComponent(name,comp,changes) => (name,comp) match {
	          case ("meta", Component(p : OMMOD)) => meta = Some(p.toMPath)
	          case _ => throw ImplementationError("match error in Declared Theory components")

	        }
	        case IdenticalComponent(ctp,name) => None //already set above
	      })
	      
	      val dn = new DeclaredTheory(doc, name, meta)
	      dn
	  }
	}
	
	def _declarations(m : Module) : List[Declaration] = {
	  m.components.flatMap(x => x match {
	    case d : Declaration => List(d)
	    case _ => Nil
	  })
	}

}

*/