package info.kwarc.mmt.api.moc

import info.kwarc.mmt.api._

import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.patterns._


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